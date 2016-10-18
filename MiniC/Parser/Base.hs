{-# LANGUAGE FlexibleContexts, FlexibleInstances, MultiParamTypeClasses
           , OverlappingInstances, TupleSections, ScopedTypeVariables #-}

-- | Parser combinators to handle additional information assigned to AST nodes.
-- Has functions to handle source information and semantic information.
module MiniC.Parser.Base where

import MiniC.Representation
import MiniC.SourceNotation
import MiniC.SymbolTable
import MiniC.Semantics
import MiniC.Helpers
import MiniC.Instances
import MiniC.AST
import SourceCode.ASTNode
import SourceCode.ASTElems
import SourceCode.SourceTree
import SourceCode.SourceInfo
import Text.Preprocess.Rewrites

import Data.Maybe
import qualified Data.Map as M
import Data.Function
import Data.List
import Text.Parsec
import Text.Parsec.PosOps
import Text.Parsec.ExtraCombinators
import Control.Monad.Reader
import Control.Monad.State
import Control.Applicative hiding ((<|>), many)
import Control.Lens
import Debug.Trace

-- * C parser types
type CParser a = ParsecT CStream (CUserState BI) CMonad a
type CStream = String
type CMonad = Reader RewriteSet
type SymbolEntryBI = SymbolEntry BI

-- Overrides the normal MonadState instance for a parser that relies on an underlying
-- state monad. But we need to use user state, because it resets when parsing one 
-- alternative fails.
instance MonadState (CUserState a) (ParsecT CStream (CUserState a) CMonad) where
  get = getState
  put = setState

-- * Handling source info

-- | Adds node information to a parsed AST node.
-- Composes the source template by cutting out children nodes.
withInfo :: CParser (BI -> b) -> CParser b
withInfo p = do inp <- getInput
                (res, sr) <- captureSourceRange p
                return $ res (NodeInfo (BasicInfo sr inp) emptySemaInfo)
                
-- | Orders the parsed AST node to compose it's node information from it's children.
inheritInfo :: CParser (BI -> b BI) -> CParser (b BI)
inheritInfo p = p <*> pure noNodeInfo
        
-- | Replaces the node information in a node with the current node information.
-- Discards node information composed by 'unifyInfo'
withNewInfo :: (ASTNode a BI) => CParser (a BI) -> CParser (a BI)
withNewInfo p = withInfo (p >>= \res -> return $ \inf -> setInfo inf res) 
      
-- * Handling semantic info
      
-- | Nodes that represent some kind of declaration that can be put into the symbol table
class CanBeRegistered node where
  -- | Gets the namespace of the declaration
  nameSpace :: node ni -> NameSpace
  nameSpace _ = NormalNS
  
  -- | Creates a symbol table entry from qualified name and declaration
  createEntry :: SourceInfo ni => QualifiedName -> node ni -> SymbolEntry ni
  
-- | Adds a declaration to the symbol table. Solves redeclaration.
registerDeclaration :: forall node . (Show (node BI), Id node BI, ASTNode node BI, CanBeRegistered node) => node BI -> CParser (node BI)
registerDeclaration decl
  | Just name <- view identStr <$> getId decl
  = do qualName <- inScope (nameSpace decl) name
       let decl' = decl & info.semanticInfo.declQualName .~ Just qualName 
       symbolTable.symbolMap %= M.insert qualName (createEntry qualName decl')
       return decl'
-- when the declaration has no name (for example: anonymous parameter) do not register
registerDeclaration decl
  = return decl
  
instance CanBeRegistered VariableDeclaration where
  createEntry _ (VariableDeclaration ss qt asm init _ info) 
    = VariableEntry (astGetList ss) qt (asm ^. astMaybe) (init ^. astMaybe) info
  
instance CanBeRegistered (ASTTriple VariableDefinition (ASTList StorageSpecifier) QualifiedType) where
  createEntry _ (ASTTriple (VariableDefinition qual asm init info) ss qt _) 
    = VariableEntry (astGetList ss) (applyQualTo qual qt noNodeInfo) (asm ^. astMaybe) (init ^. astMaybe) info
      
instance CanBeRegistered ParameterDeclaration where
  createEntry _ = ParameterEntry
      
instance CanBeRegistered FunctionDeclaration where
  createEntry _ = FunctionEntry
            
instance CanBeRegistered TypeDefinition where
  nameSpace td = case td ^. typeDefType of TypeDef {} -> NormalNS
                                           _ -> ComplexNS
  createEntry _ = TypeEntry
            
instance CanBeRegistered Variant where
  createEntry _ = VariantEntry
                   
-- | Parses the given declaration in an inner scope  
insideScope :: String -> CParser a -> CParser a
insideScope s p = do currentScope %= \(Scope sx) -> Scope (s:sx)
                     currentIds %= (0:)
                     res <- p
                     currentScope %= \(Scope sx) -> Scope (tail sx)
                     currentIds %= tail
                     return res
       
-- | Parses the given declaration inside an anonym scope 
-- (for example: anonym structs, compound stmts)
insideAnonymScope :: CParser a -> CParser a
insideAnonymScope p 
  = do let currentId = currentIds . ix 0
       id <- gets (head . view currentIds)
       currentId += 1
       insideScope (show id) p
       
insideScope' :: Maybe (Ident ni) -> CParser a -> CParser a
insideScope' (Just id) = insideScope (view identStr id)
insideScope' Nothing = insideAnonymScope

-- | Gets the qualified name of an id in current scope
inScope :: NameSpace -> String -> CParser QualifiedName
inScope ns s = QualifiedName ns <$> gets (view currentScope) <*> pure (genScalarQual s)
  where genScalarQual s = Scalar (ASTJust (Ident s (noNodeInfo & sourceInfo.niTemplate .~ Just [TextElem s]))) 
                                 (noNodeInfo & sourceInfo.niTemplate .~ Just [NodeElem (NthChildOf 0 (NthChildOf 0 Current))])        
          
-- | Looks up a name in the symbol table
lookupName' :: NameSpace -> String -> CParser (Maybe (QualifiedName, SymbolEntryBI))
lookupName' ns s = do qname <- inScope ns s
                      gets (lookupSymbol qname . view symbolTable)

lookupName = lookupName' NormalNS

lookupNameBoth :: String -> CParser (Maybe (QualifiedName, SymbolEntryBI))
lookupNameBoth s = mplus <$> lookupName' NormalNS s <*> lookupName' ComplexNS s
           
-- | Checks that a given name represents a type
checkIsTypeSymbol :: IdentBI -> CParser IdentBI
checkIsTypeSymbol res 
  = do let simpleName = res ^. identStr
       entry <- lookupNameBoth simpleName
       case entry of 
         Just (name, TypeEntry {}) -> return (res & identInfo.semanticInfo.referenceQualName .~ Just name)
         _ -> fail $ "Unknown type name: " ++ simpleName
 
-- | Checks that the given name can appear in an expression
checkIsExpression :: IdentBI -> CParser IdentBI
checkIsExpression res
  = do let simpleName = res ^. identStr
       entry <- lookupName simpleName
       case entry of 
         Nothing -> fail $ "Unknown name: " ++ simpleName
         Just (name, TypeEntry {}) -> fail $ "Value expected, type found: " ++ simpleName
         Just (name, _) -> return (res & identInfo.semanticInfo.referenceQualName .~ Just name)

debugSymbolTable :: CParser ()
debugSymbolTable
  = gets (show . (each._2 %~ take 40 . show) . M.assocs . view (symbolTable.symbolMap)) 
      >>= flip trace (return ())