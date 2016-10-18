{-# LANGUAGE NoMonomorphismRestriction, LambdaCase, DeriveDataTypeable, DeriveFunctor, TemplateHaskell
           , TypeSynonymInstances, FlexibleInstances #-}
module MiniC.Semantics where

import MiniC.AST
import MiniC.Representation
import MiniC.PrettyPrint

import Control.Applicative
import Control.Lens
import Control.Lens.Plated
import Data.Data.Lens
import Data.Data
import Data.Monoid
import Data.Function
import Data.Maybe
import Data.Typeable
import SourceCode.SourceInfo
import SourceCode.ToSourceTree
  

type BI = NodeInfo BasicInfo SemaInfo
type NI = NodeInfo TemplateInfo SemaInfo
  
data SemaInfo 
  = SemaInfo { _declQualName :: Maybe QualifiedName
             , _referenceQualName :: Maybe QualifiedName
             , _exprType :: Maybe SemType
             }
    deriving (Eq, Ord, Typeable, Data)  
  
data SemType 
  = Int Sign IntSize 
  | Float FloatingSize
  -- ...
  deriving (Show, Eq, Ord, Typeable, Data) 
               
  -- | A C name with the scope of the name        
data QualifiedName
  = QualifiedName { _qnNameSpace :: NameSpace
                  , _qnScope :: Scope
                  , _qnTypeQual :: TypeQualifier NI
                  }
  deriving (Eq, Ord, Typeable, Data)

data NameSpace = NormalNS  -- ^ Namespace for variables, functions, 
                           -- typedef-ed names, enum variants
               | ComplexNS -- ^ Namespace for structs, unions and enums
  deriving (Eq, Ord, Typeable, Data)  
  
newtype Scope = Scope { _scopeNames :: [String] }
  deriving (Eq, Ord, Typeable, Data) 
      
instance Show NameSpace where
  show NormalNS = ""
  show ComplexNS = "$"
 
emptyScope :: Scope
emptyScope = Scope []
  
$(makeLenses ''Scope)
$(makeLenses ''QualifiedName)

instance Show Scope where
  show (Scope sc) = foldr (\b a -> a ++ "::" ++ b) "" sc
  
type Fun a = a -> a

simplifyQualName :: QualifiedName -> QualifiedName
simplifyQualName 
  = qnScope.scopeNames %~ filter (null . (id :: Fun [(Int,String)]) . reads)

outerScope :: QualifiedName -> Maybe QualifiedName
outerScope qn 
  = if null (qn ^. qnScope.scopeNames) 
      then Nothing 
      else Just $ qn & qnScope.scopeNames %~ tail
      
$(makeLenses ''SemaInfo)   
  
instance SourceInfo srci => SourceInfo (NodeInfo srci SemaInfo) where
  generateInfo anc children 
    = NodeInfo (generateInfo (anc ^. sourceInfo) (children ^.. each.sourceInfo)) emptySemaInfo
  noNodeInfo = NodeInfo noNodeInfo emptySemaInfo
  
emptySemaInfo :: SemaInfo
emptySemaInfo 
  = SemaInfo Nothing Nothing Nothing
  
