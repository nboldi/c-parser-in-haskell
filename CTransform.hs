{-# LANGUAGE FlexibleInstances, TypeSynonymInstances, NamedFieldPuns, LambdaCase, ViewPatterns
           , FlexibleContexts, TemplateHaskell, RankNTypes #-}

-- | Reimplementation of Remix-C transformation program.
module CTransform where

import MiniC.ParseProgram
import MiniC.Representation
import MiniC.AST
import MiniC.PrettyPrint
import MiniC.GenTemplate
import MiniC.SourceNotation
import MiniC.RangeTree
import MiniC.Semantics
import MiniC.Helpers
import MiniC.SymbolTable
import MiniC.TransformInfo
import MiniC.Instances

import SourceCode.ASTElems
import SourceCode.ToSourceTree
import SourceCode.ASTNode

import Control.Monad
import Control.Lens hiding ((<.>))
import Control.Lens.Plated
import Data.Data.Lens
import Data.Function
import Data.Maybe
import Data.Either.Combinators
import Control.Applicative
import System.Environment
import System.Directory
import System.FilePath
import Debug.Trace

main :: IO ()
main = do (file:trfs) <- getArgs
          wd <- getCurrentDirectory
          transforms <- parseTrfs trfs
          runProgram (ProgramParams file transforms) >>= \case
            Right prog -> writeFile (modifiedFileName transforms file) (prettyPrint prog)
            Left err -> putStrLn err
          
runProgram :: ProgramParams -> IO (Either String TranslationUnitNI)
runProgram (ProgramParams file transforms) =
  readFile file
    >>= parseProgram file
    >>= return 
         . mapBoth show
                   ( flip transformAST transforms 
                      . analyseAST
                      . transformSourceInfo )
                   
       
data ProgramParams 
  = ProgramParams { _inputFile :: String 
                  , _transformations :: [Transformation]
                  }
  deriving (Show)
        
-- | Transformations that the tool can produce
data Transformation = IntroduceIndirection QualifiedName
                    | RemoveIndirection QualifiedName
                    | CreateSkeleton
  deriving Show
        
-- | Parses command line arguments
parseTrfs :: [String] -> IO [Transformation]
parseTrfs ("-ri":qname:rest) 
  = (:) <$> (RemoveIndirection <$> parseQualName qname) <*> parseTrfs rest
parseTrfs ("-ii":qname:rest) 
  = (:) <$> (IntroduceIndirection <$> parseQualName qname) <*> parseTrfs rest
parseTrfs ("-cc":rest) 
  = (CreateSkeleton :) <$> parseTrfs rest
parseTrfs [] = return []
-- handle command line errors
parseTrfs (tr:_) | tr `elem` ["-ri","-ii"] 
  = error ("Not enough parameters for transformation " ++ tr) 
parseTrfs (command:_) = error $ "Unknown command: " ++ command
  
-- | Produces the name of the transformed file
modifiedFileName :: [Transformation] -> FilePath -> FilePath
modifiedFileName [CreateSkeleton]   = addPlusExtension "skeleton"
modifiedFileName _                  = addPlusExtension "configured" 

-- | Adds a new extensions before an existing one
addPlusExtension ext fn 
  = dropExtensions fn <.> ext <.> takeExtensions fn
     
transformAST :: TranslationUnitNI -> [Transformation] -> TranslationUnitNI
transformAST = foldl doTrfAST 
  where doTrfAST :: TranslationUnitNI -> Transformation -> TranslationUnitNI
        doTrfAST tu (RemoveIndirection name) = removeIndirection name tu
        doTrfAST tu (IntroduceIndirection name) = addIndirection name tu
        doTrfAST tu CreateSkeleton = createSkeleton tu
                                                
addIndirection :: QualifiedName -> TranslationUnitNI -> TranslationUnitNI
addIndirection qn tu
  = let declaration :: Simple Traversal TranslationUnitNI VariableDeclarationNI
        declaration = biplate . checkScope qn . filterTrav (has (matchQualItself qn)) simple
        exprs = biplate :: Simple Traversal TranslationUnitNI ExpressionNI
     in case toListOf declaration tu of
          [] -> error "No matching declaration for adding indirection"
          [_] -> over exprs trfExpr $ over declaration trfDecl tu
          _ -> error "Multiple matching declarations for adding indirection"
     
  where -- | Add indirection to a declaration
        trfDecl :: VariableDeclarationNI -> VariableDeclarationNI
        trfDecl vd = vd & matchQualItself qn %~ (addPtrToQual (vd ^? matchParentQual qn))
  
        trfExpr :: ExpressionNI -> ExpressionNI
        trfExpr memb@(Member{}) 
          | toTrf qn (memb ^?! exprBase)
             && (case memb ^?! exprMemberDeref of SimpleMember {} -> True; _ -> False)
          = memb & exprMemberDeref .~ genMemberDeref
        trfExpr name 
          | toTrf qn name
          = genDerefExpr name
        trfExpr other 
          = over uniplate trfExpr other
        
-- | Match the parent or the qualifier itself where changes need to be done
matchParentQual, matchQualItself :: QualifiedName -> Simple Traversal VariableDeclarationNI TypeQualifierNI
matchParentQual qn = varDeclQualTyp . qualTypQual . matchQualParent (qn ^. qnTypeQual)
matchQualItself qn = varDeclQualTyp . qualTypQual . matchQual (qn ^. qnTypeQual)
  
        
createSizeOfQual :: QualifiedTypeNI -> ExpressionNI
createSizeOfQual = undefined
        
-- | Removes indirection 
removeIndirection :: QualifiedName -> TranslationUnitNI -> TranslationUnitNI
removeIndirection qn tu
  = let declaration :: Simple Traversal TranslationUnitNI VariableDeclarationNI
        declaration = biplate . checkScope qn . filterTrav (has (matchQualItself qn)) simple
        exprs = biplate :: Simple Traversal TranslationUnitNI ExpressionNI
     in case toListOf declaration tu of
          [] -> error "No matching declaration for removing indirection"
          [_] -> over exprs trfExpr $ over declaration trfDecl tu
          _ -> error "Multiple matching declarations for removing indirection"
      
  where trfDecl :: VariableDeclarationNI -> VariableDeclarationNI
        trfDecl vd = vd & matchQualItself qn %~ (removePtrFromQual (vd ^? matchParentQual qn))
        
        trfExpr :: ExpressionNI -> ExpressionNI
        trfExpr memb@(Member{})
          | case memb ^?! exprMemberDeref of 
              DerefMember {} -> fmap (view qnTypeQual) (getQualName (memb ^?! exprBase))
                                  == Just (removePtrFromQual Nothing (qn ^. qnTypeQual))
              SimpleMember {} -> False
          = memb & exprMemberDeref .~ genMemberSimple
        trfExpr un 
          | Just (DereferenceOp {}) <- un ^? exprUnaryOp
          , Just name <- un ^? exprOperand
          , toTrf qn un
          = name
        trfExpr other 
          = over uniplate trfExpr other
      

-- | Check that the element is in the scope we search for
checkScope qn = filterTrav (((==) `on` view qnScope . simplifyQualName) qn) 
                           (info.semanticInfo.declQualName._Just) 
    
-- | Check that the elem is the element we want to transform
toTrf qn elem 
  | Just qualName <- getQualName elem
    = simplifyQualName qualName == simplifyQualName qn
  | otherwise 
    = False
            
getQualName elem = (join (elem ^? info.semanticInfo.referenceQualName)
                       <|> join (elem ^? info.semanticInfo.declQualName))
        
-- | Creates a skeleton file, a file where only declarations are kept.
createSkeleton :: TranslationUnitNI -> TranslationUnitNI
createSkeleton = transformOn (biplate :: Simple Traversal TranslationUnitNI StatementNI) 
                             removeStatements
  where removeStatements :: StatementNI -> StatementNI
        removeStatements 
          = compoundStmts %~ astFilter hasDeclaration
          
        hasDeclaration :: ASTEitherNI Declaration Statement 
                            -> Maybe (ASTEitherNI Declaration Statement)
        hasDeclaration (ASTLeft d) = Just (ASTLeft d)
        hasDeclaration (ASTRight stmt) 
          = if null (universeOn (biplate :: Simple Traversal StatementNI DeclarationNI) stmt) 
              then Nothing 
              else Just (ASTRight (transform removeStatements stmt)) 
        

-- | Propagates additional information in the syntax tree
analyseAST :: TranslationUnitNI -> TranslationUnitNI
analyseAST = transformOn (biplate :: Simple Traversal TranslationUnitNI ExpressionNI)
                         additionalScopes
  where additionalScopes expr@(Unary {}) 
          | Just (DereferenceOp {}) <- expr ^? exprUnaryOp
          , Just (Just qn) <- expr ^? exprOperand.refName
            = expr & refName %~ (<|> Just (qn & qnTypeQual %~ addPtrToQual Nothing))
        additionalScopes expr@(Indexing {})
          | Just (Just qn) <- expr ^? exprBase.refName
            = expr & refName %~ (<|> Just (qn & qnTypeQual %~ genArrayQual ))
        additionalScopes expr@(NameExpr {})
          | Just qn <- expr ^? exprIdent.refName
            = expr & refName %~ (<|> qn)
        -- additionalScopes expr@(Member {})
          -- | Just qn <- expr ^? exprBase.refName
          -- , Just deref <- expr ^? exprMemberDeref
            -- = expr & refName %~ (<|> Just (qn & qnTypeQual %~ case deref of DerefMember {} -> addPtrToQual Nothing
                                                                            -- SimpleMember {} -> id))
        additionalScopes expr@(ParenExpr {})
          | Just qn <- expr ^? parenExpr.refName
            = expr & refName %~ (<|> qn)
        additionalScopes expr = expr
          
        refName :: ASTNode node NI => Lens' (node NI) (Maybe QualifiedName)
        refName = info.semanticInfo.referenceQualName
        
$(makeLenses ''ProgramParams)

        