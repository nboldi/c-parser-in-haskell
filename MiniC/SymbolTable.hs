{-# LANGUAGE DeriveDataTypeable, TemplateHaskell, TupleSections #-}

-- | This module controls how symbols are represented in the symbol table. This module
-- holds the definitions relating to symbol tables that are pure (non-monadic). Monadic
-- operations are defined in 'MiniC.Parser.Base'.
module MiniC.SymbolTable where

import MiniC.AST
import MiniC.Helpers
import MiniC.Semantics
import qualified Data.Map as M
import Data.Data
import Control.Applicative
import Control.Lens
   

newtype SymbolTable ni 
  = SymbolTable { _symbolMap :: M.Map QualifiedName (SymbolEntry ni) }

data SymbolEntry ni
  = VariableEntry { _varEntryStorage :: [StorageSpecifier ni]
                  , _varEntryQualTyp :: QualifiedType ni
                  , _varEntryAsmSpec :: Maybe (AsmSpecifier ni)
                  , _varEntryInit :: Maybe (Expression ni)
                  , _varEntryInfo :: ni
                  }
  | ParameterEntry { _parEntryDecl :: ParameterDeclaration ni }
  | FunctionEntry { _funEntryDecl :: FunctionDeclaration ni }
  | TypeEntry { _typeEntryDecl :: TypeDefinition ni }
  | VariantEntry { _variantEntryDecl :: Variant ni }
  deriving (Show)
  
data CUserState ni = CUserState 
  { _symbolTable :: SymbolTable ni
  , _currentScope :: Scope
  , _currentIds :: [Int]
  }

initUserState :: CUserState ni
initUserState = CUserState (SymbolTable M.empty) emptyScope [0]

$(makeLenses ''SymbolTable)
$(makeLenses ''CUserState)
$(makeLenses ''SymbolEntry)

lookupSymbol :: QualifiedName -> SymbolTable ni -> Maybe (QualifiedName, SymbolEntry ni)
lookupSymbol qn st
  = ((qn, ) <$> M.lookup qn (view symbolMap st)) 
      <|> (outerScope qn >>= flip lookupSymbol st)
