{-# LANGUAGE FlexibleInstances, FlexibleContexts, MultiParamTypeClasses, NamedFieldPuns, DeriveDataTypeable #-}
{-# LANGUAGE StandaloneDeriving, TemplateHaskell, ImpredicativeTypes, UndecidableInstances #-}

-- | Extends 'MiniC.AST' with node information types, and class instances for AST nodes.
module MiniC.Representation where

import GHC.Generics (Generic)
import SourceCode.ASTElems
import MiniC.AST
import MiniC.SourceNotation
import SourceCode.SourceTree
import SourceCode.ToSourceTree
import SourceCode.ASTNode
import SourceCode.SourceInfo
import Data.Typeable
import Data.Data
import Data.List
import Data.Maybe
import Data.Function
import Text.Parsec hiding ((<|>), optional, many)
import Text.Parsec.PosOps
import Data.Data.Lens
import Control.Lens
import Control.Applicative
import Debug.Trace

-- | Syntactic and semantic information about an AST node
data NodeInfo src sem
  = NodeInfo { _sourceInfo :: src
             , _semanticInfo :: sem
             }      
    deriving (Show, Typeable, Data)             

makeLenses ''NodeInfo
    
-- All source info is considered equal, so in a container all AST nodes 
-- with the same structure are not differentiated.
instance Eq (NodeInfo source sema) where
  _ == _ = True 
instance Ord (NodeInfo source sema) where
  _ `compare` _ = EQ
    
-- | An intermediate node information that contains the source range 
-- of the node and the remaining input of the parser when it started 
-- parsing the element. We could only store the text from which the node
-- was parsed, but then we could not extend nodes from their children.
data BasicInfo -- ^ The node was originally in the source 
  = BasicInfo { _biRange :: SourceRange
              , _biInput :: String
              }
  | InheritInfo
  deriving (Eq, Ord, Typeable, Data)
  
$(makeLenses ''BasicInfo)
  
instance SourceInfo BasicInfo where
  generateInfo ancestor []
    = ancestor & biRange %~ rngStartAsRange
  generateInfo _ children
    = foldl1 unifyBasicInfo children
  noNodeInfo = InheritInfo
  
deriving instance Typeable SourceTemplateElem
deriving instance Data SourceTemplateElem
deriving instance Typeable NodeIndex
deriving instance Data NodeIndex

-- | Gets the range of a basic info
getRange :: BasicInfo -> Maybe SourceRange
getRange = preview biRange

-- | Combines two basic infos into one
unifyBasicInfo :: BasicInfo -> BasicInfo -> BasicInfo
unifyBasicInfo (BasicInfo rng1 inp1) (BasicInfo rng2 inp2) 
  = BasicInfo (rng1 `srcRngUnion` rng2) 
              (if srcRangeBegin rng1 < srcRangeBegin rng2 then inp1 else inp2)
unifyBasicInfo bi InheritInfo = bi
unifyBasicInfo InheritInfo bi = bi

instance Show BasicInfo where
  show (BasicInfo rng inp) 
    = shortShowRng rng ++ " '" ++ takeSourceRange (rngFromStart rng) inp ++ "'"
  show InheritInfo = "<inherited>"
      
-- | The final meta information that can be used to pretty print the AST
data TemplateInfo 
  = TemplateInfo 
      { _niRange     :: Maybe SourceRange
      , _niTemplate  :: Maybe SourceTemplate 
      }
      -- [Comment]    -- ^ comments belonging
      -- [Attribute]  -- ^ attributes
         deriving (Eq, Ord, Typeable, Data)
  
$(makeLenses ''TemplateInfo)

  
instance SourceInfo TemplateInfo where
  generateInfo ancestor [] 
    = TemplateInfo (rngStartAsRange <$> view niRange ancestor) (Just [])
  generateInfo _ infs
    = TemplateInfo (foldl1 (\a b -> srcRngUnion <$> a <*> b) (infs ^.. traverse.niRange)) 
                   (Just (map (\i -> NodeElem (NthChildOf i Current)) 
                              (take (length infs) [0..])))
  noNodeInfo = TemplateInfo Nothing Nothing
  
instance Show TemplateInfo where
  show (TemplateInfo rng templ) = maybe "" shortShowRng rng ++ "||" ++ maybe "" (concatMap show) templ ++ "||"
      
nodeTemplate :: TemplateInfo -> SourceTemplate
nodeTemplate (TemplateInfo { _niTemplate = Just t }) = t
  
-- * Preprocessor-handled source elements
  
data Comment = LineComment String
             | BlockComment String
             | DocComment String
             
data AttributeList a 
  = AttributeList { attributes    :: [Attribute a]
                  , attributeInfo :: a
                  } deriving (Show, Eq)
    
data Attribute a
  = Alias        { attribAlias          :: String
                 , attribInfo           :: a 
                 }                      
  | Aligned      { attribAlign          :: Int
                 , attribInfo           :: a
                 }                      
  | AllocSize    { attribAllocSizeArgs  :: [Int]
                 , attribInfo           :: a
                 }                      
  | AlwaysInline { attribInfo           :: a }
  | CDecl        { attribInfo           :: a }
  | Const        { attribInfo           :: a }
  | Constructor  { attribInfo           :: a }
  | Destructor   { attribInfo           :: a }
  | Deprecated   { attribInfo           :: a }
  | Format       { attribFormatFunct    :: String
                 , attribFormatStrArg   :: Int 
                 , attribFormatCheckArg :: Int 
                 , attribInfo           :: a 
                 }
  | Malloc       { attribInfo           :: a }
  | Mode         { attribMode           :: ModeSpec 
                 , attribInfo           :: a
                 }
  | NoInline     { attribInfo           :: a }
  | NoReturn     { attribInfo           :: a }
  | Optimize     { attribOptArgs        :: [Either String Int]
                 , attribInfo           :: a 
                 }
  | Packed       { attribInfo           :: a }
  | Pure         { attribInfo           :: a }
  | RegParam     { attribRegNum         :: Int
                 , attribInfo           :: a 
                 }
  | Section      { attribSection        :: String
                 , attribInfo           :: a 
                 }
  | StdCall      { attribInfo           :: a }
  | TransparentUnion { attribInfo           :: a }
  | Unused       { attribInfo           :: a }
  | Used         { attribInfo           :: a }
  | Visibility   { attribVisibility     :: VisibilityMode
                 , attribInfo           :: a 
                 }
  | Weak         { attribInfo           :: a }
  deriving (Show, Eq)
                 
data ModeSpec = ByteMode | WordMode | PointerMode
     deriving (Show, Eq)
data VisibilityMode = DefaultVisibility | HiddenVisibility | InternalVisibility | ProtectedVisibility
     deriving (Show, Eq)
  
  


