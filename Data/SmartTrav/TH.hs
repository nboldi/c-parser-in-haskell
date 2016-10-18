{-# LANGUAGE LambdaCase #-}

module Data.SmartTrav.TH where
  
import Language.Haskell.TH
import Data.Maybe
import Control.Monad
import Control.Applicative
  
-- | Derive SmartTrav
deriveSmartTrav :: Name -> Q [Dec]
deriveSmartTrav nm = reify nm >>= (\case
  TyConI dt -> case dt of
    DataD _ tyConName typArgs dataCons _ -> 
      createInstance tyConName typArgs dataCons  
    NewtypeD _ tyConName typArgs dataCon _ -> 
      createInstance tyConName typArgs [dataCon]  
    _ -> fail "Unsupported data type"
  _ -> fail "Expected the name of a data type or newtype"
  )
  
createInstance :: Name -> [TyVarBndr] -> [Con] -> Q [Dec]
createInstance tyConName typArgs dataCons   
          = do (clauses, preds) <- unzip <$> mapM createClause dataCons
               return [InstanceD (concat preds) 
                                 (AppT (ConT className) 
                                 (foldl AppT (ConT tyConName) 
                                        (map getTypVarTyp (init typArgs))))
                                 [FunD funName clauses]]
  where  -- | Gets the variable that is traversed on
        varToTraverseOn :: Q Name
        varToTraverseOn = case reverse typArgs of 
          (PlainTV last : _)        -> return last
          (KindedTV last StarT : _) -> return last
          (KindedTV last _ : _)     -> fail $ "The kind of the last type parameter is not *"
          []                        -> fail $ "The kind of type " ++ show tyConName ++ " is *"
  
        -- | Creates a clause for a constructor, the needed context is also generated
        createClause :: Con -> Q (Clause,[Pred])
        createClause (RecC conName conArgs) 
          = createClause' conName (map (\(_,_,r) -> r) conArgs)
        createClause (NormalC conName conArgs) 
          = createClause' conName (map snd conArgs)
        
        createClause' conName argTypes
          = do bindedNames <- replicateM (length argTypes) (newName "p")
               (handleParams,ctx) <- unzip <$> zipWithM processParam
                                                        bindedNames argTypes
               return $ (Clause [ VarP desc, VarP asc, VarP f
                               , ConP conName (map VarP bindedNames) ] 
                               (NormalB (createExpr conName handleParams)) []
                        , concat ctx)


        -- | Creates an expression for the body of a smartTrav clause
        -- using the matches created for parameters
        createExpr :: Name -> [Exp] -> Exp
        createExpr ctrName []
          = AppE applPure $ ConE ctrName
        createExpr ctrName (param1:params)
          = foldl (\coll new -> InfixE (Just coll) applStar (Just new)) 
                  (InfixE (Just $ ConE ctrName) applDollar (Just param1))
                  params
        
        applStar = VarE (mkName "Control.Applicative.<*>")
        applStarR = VarE (mkName "Control.Applicative.*>")
        applStarL = VarE (mkName "Control.Applicative.<*")
        applDollar = VarE (mkName "Control.Applicative.<$>")
        applPure = VarE (mkName "Control.Applicative.pure")
        
        className = mkName "SmartTrav"
        funName = mkName "smartTrav"
        desc = mkName "desc"
        asc = mkName "asc"
        f = mkName "f"
       
        -- | Creates the expression and the predicate for a parameter
        processParam :: Name -> Type -> Q (Exp, [Pred])
        processParam name (VarT v)  -- found the type variable to traverse on
          = do travV <- varToTraverseOn 
               if v == travV then return (AppE (VarE f) (VarE name), [])
                             else return (AppE applPure (VarE name), [])
        processParam name (AppT tf ta) = do
          expr <- createExprForHighKind' name (VarE f) ta
          case expr of Just (e,ctx) -> return (e, if isTypVar tf then ClassP className [tf] : ctx
                                                                 else ctx)
                       Nothing -> return (AppE applPure (VarE name), [])
        processParam name _
          = return (AppE applPure (VarE name), [])
  
        -- | Create an expression and a context for a higher kinded parameter
        createExprForHighKind' :: Name -> Exp -> Type -> Q (Maybe (Exp, [Pred]))
        createExprForHighKind' name f (AppT tf ta)
          = do res <- createExprForHighKind' name (applExpr f) ta
               case res of Just (e,ctx) -> return $ Just (e, if isTypVar tf then ClassP className [tf] : ctx
                                                                            else ctx)
                           Nothing -> return Nothing
        createExprForHighKind' name f (VarT v)
          = do travV <- varToTraverseOn 
               if v == travV then
                 return $ Just (InfixE (Just $ VarE desc) applStarR 
                                       (Just $ InfixE (Just (applExpr f `AppE` (VarE name))) 
                                                       applStarL 
                                                       (Just $ VarE asc)), [])
                else return Nothing
        createExprForHighKind' _ name _
          = return Nothing
          
        applExpr f = (((VarE funName) `AppE` (VarE desc)) `AppE` (VarE asc)) 
                          `AppE` f
                          
               
isTypVar :: Type -> Bool
isTypVar (VarT _) = True
isTypVar _ = False               
  
getTypVarTyp :: TyVarBndr -> Type
getTypVarTyp (PlainTV n) = VarT n
getTypVarTyp (KindedTV n _) = VarT n
  
  
thExamine :: Q [Dec] -> Q [Dec]
thExamine decl = do d <- decl
                    runIO (print d)
                    return d


