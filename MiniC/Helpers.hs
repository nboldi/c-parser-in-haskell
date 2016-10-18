{-# LANGUAGE FlexibleInstances, FlexibleContexts, MultiParamTypeClasses, RankNTypes
           , UndecidableInstances, ScopedTypeVariables, StandaloneDeriving #-}

-- | Helper functions for parsing and low-level analyse of C code.
module MiniC.Helpers where

import MiniC.Representation
import MiniC.AST
import MiniC.Instances
import MiniC.Semantics
import MiniC.PrettyPrint 
import Data.Data
import Data.Maybe
import Control.Lens
import Control.Applicative
import Control.Monad.State
import Data.Data.Lens
import SourceCode.ASTElems
import SourceCode.SourceInfo
import Debug.Trace

-- * AST utility functions

-- | A class to retrieve identifiers of AST nodes. This is semi-boilerplate code.
-- There are some recurring patterns but there are a lot of exceptions.
-- Declared Id in 'SemaInfo' is generated from this.
class (Typeable ni, Data (a ni)) => Id a ni where
  -- Gives a traversal, that captures the only one identifier if found.
  ident :: a ni -> Traversal' (a ni) (Ident ni)

  -- | Getter for the identifier
  getId :: a ni -> Maybe (Ident ni)
  getId x = x ^? ident x
  
  -- | Setter for the identifier
  setId :: Ident ni -> a ni -> a ni
  setId id x = x & ident x .~ id

instance (Typeable a, Data (Type a), Data a) => Id Declaration a where
  ident d@(TypeDecl {}) = typeDecl . ident (d ^?! typeDecl)
  ident d@(FuncDecl {}) = funDecl . ident (d ^?! funDecl)
  ident d@(VarDecl {}) = varDecl . ident (d ^?! varDecl)
  ident d@(ParamDecl {}) = varDecl . ident (d ^?! varDecl)

instance (Typeable a, Data (Type a), Data a) => Id TypeDefinition a where
  ident d = typeDefType . ident (d ^. typeDefType)

instance (Typeable a, Data (Type a), Data a) => Id FunctionDeclaration a where
  ident d = funDeclQualType . ident (d ^. funDeclQualType)

instance (Typeable a, Data (Type a), Data a) => Id VariableDeclaration a where
  ident d = varDeclQualTyp . ident (d ^. varDeclQualTyp)

instance (Typeable a, Data (Type a), Data a) => Id VariableDefinition a where
  ident d = varDefQualTyp . ident (d ^. varDefQualTyp)

instance (Typeable a, Data (Type a), Data a) => Id ParameterDeclaration a where
  ident d = paramDeclQualTyp . ident (d ^. paramDeclQualTyp)
  
instance (Typeable a, Data (Type a), Data a) => Id TypeQualifier a where
  ident (Scalar {}) = qualIdent.astMaybe.traverse
  ident d = qualBase . ident (d ^?! qualBase)

instance (Typeable a, Data (Type a), Data a) => Id QualifiedType a where
  ident d = qualTypQual . ident (d ^. qualTypQual)

instance (Typeable a, Data (Type a), Data a) => Id Type a where
  ident d@(StructType {}) = structTypFields . ident (d ^?! structTypFields)
  ident d@(UnionType {}) = unionTypFields . ident (d ^?! unionTypFields)
  ident d@(EnumType {}) = enumTypVariants . ident (d ^?! enumTypVariants)
  ident d@(TypeDef {_typDefIdent = ASTJust _}) = typDefIdent.astMaybe.traverse
  ident d@(TypeDef {}) = typDefTyp . ident (d ^?! typDefTyp)
  ident d@(TypeName {}) = typNameId

instance (Typeable a, Data (Type a), Data a) => Id StructUnion a where
  ident _ = structUnionId.astMaybe.traverse

instance (Typeable a, Data (Type a), Data a) => Id Enumeration a where
  ident _ = enumName.astMaybe.traverse
  
instance (Typeable a, Data (ASTTriple n m p a), Data a, Id n a) => Id (ASTTriple n m p) a where
  ident d = ast_triple_1 . ident (d ^. ast_triple_1)

-- TODO: review the validity of use of qualifier application
  
-- | Applies a type qualifier on a qualified type. 
-- For example: Applying @*@ to @int a@ gives @int *a@. 
--              Applying @*a[10]@ to @int *a@ gives @int *(*a)[10]@.
applyQualTo :: forall ni . (SourceInfo ni) => TypeQualifier ni -> QualifiedType ni -> ni -> QualifiedType ni
applyQualTo tq (QualifiedType tq2 t _) = QualifiedType (tq2 `qualOn` tq) t
  where qualOn :: TypeQualifier ni -> TypeQualifier ni -> TypeQualifier ni
        (Scalar ASTNothing _) `qualOn` tq = tq
        (PtrQual tq' ni') `qualOn` tq 
          = PtrQual (tq' `qualOn` tq) noNodeInfo
        (ConstQual tq' ni') `qualOn` tq 
          = ConstQual (tq' `qualOn` tq) noNodeInfo
        (VolatileQual tq' ni') `qualOn` tq 
          = VolatileQual (tq' `qualOn` tq) noNodeInfo
        (ArrayQual arr tq' ni') `qualOn` tq 
          = ArrayQual arr (tq' `qualOn` tq) noNodeInfo
        (RestrictQual tq' ni') `qualOn` tq 
          = RestrictQual (tq' `qualOn` tq) noNodeInfo
          
data QualMatch ni = QualMatch { qmMatched :: TypeQualifier ni
                              , qmCtx :: Maybe (TypeQualifier ni) }
     
    
matchQualParent :: Eq ni => TypeQualifier ni -> Simple Traversal (TypeQualifier ni) (TypeQualifier ni)
matchQualParent pattern f t 
  | Just childQual <- t ^? qualBase 
  , childQual `similarQual` pattern 
  = f t
matchQualParent pattern f t
  = (qualBase . matchQualParent pattern) f t

matchQual :: Eq ni => TypeQualifier ni -> Simple Traversal (TypeQualifier ni) (TypeQualifier ni)
matchQual pattern f t 
  | t `similarQual` pattern 
  = f t
matchQual pattern f t
  = (qualBase . matchQual pattern) f t

filterTrav :: (b -> Bool) -> Simple Traversal a b -> Simple Traversal a a
filterTrav pred q f t = case pred <$> (t ^? q) of Just True -> f t 
                                                  _         -> pure t

-- | Applies a function to a specified position of the type qualifier.
{-
modifyQual 
  :: (Typeable ni, Data ni, Eq ni) 
  => (Maybe (TypeQualifier ni) -> TypeQualifier ni -> TypeQualifier ni) 
       -- ^ additional qualification, based on a context (previous qualification)
      -> TypeQualifier ni -- ^ positioning qualification, addition happens when this matches the rest
      -> TypeQualifier ni -- ^ type qualification to change
      -> (TypeQualifier ni, Maybe ())
modifyQual tf q0 q = modifyQual' Nothing tf q0 q
  where modifyQual' ctx tf q0 q 
          | q0 `similarQual` q
          = modify (\st -> st { qmInner = q }) >> return (tf ctx q)
        modifyQual' _ tf q0 q 
          = q & qualBase <%= modifyQual' (Just q) tf q0
-}
      
-- | The user don't have to input the exact qualified name, but a simplified version
similarQual :: Eq ni => TypeQualifier ni -> TypeQualifier ni -> Bool
-- similar qual ignores array indexes and element type specifiers
(ArrayQual _ qb1 _) `similarQual` (ArrayQual _ qb2 _) 
  = qb1 `similarQual` qb2
tq1 `similarQual` tq2 
  = tq1 == tq2
      
    
instance Show QualifiedName where
  show (QualifiedName ns sc name) = show ns ++ show sc ++ "::" ++ prettyPrint name
  
deriving instance Show SemaInfo
       