{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, DeriveDataTypeable, DeriveGeneric, DeriveFunctor, DeriveFoldable, DeriveTraversable
           , FlexibleContexts, UndecidableInstances, ScopedTypeVariables, TemplateHaskell, LambdaCase
           , StandaloneDeriving #-}

module MiniLanguage where

import MiniC.SourceNotation
import SourceCode.ASTElems
import SourceCode.ASTNode
import SourceCode.ToSourceTree
-- import MiniC.PrettyPrint
import MiniC.Parser.Lexical (whole)
import MiniC.Parser.Base
import MiniC.Representation (BasicInfo)

import Control.Applicative hiding ((<|>), many)
import Control.Monad.State
import Control.Lens
import Control.Lens.Plated
import Data.Data
import Data.Data.Lens
 
import Data.SmartTrav
import Data.SmartTrav.TH
import Debug.Trace
import Data.List
import Data.Foldable
import Data.Function
import Data.Either.Combinators
import Control.Monad.Reader
import GHC.Generics
import Text.Parsec hiding (State)
import Text.Parsec.String
import Text.Parsec.Language
import Text.Parsec.ExtraCombinators
import Text.Parsec.PosOps
import Text.Preprocess.Rewrites (emptyRewSet)
import qualified Text.Parsec.Token as T

-- * AST

data Lit a = IntLit { _litValue :: Integer, _litInfo :: a }
     deriving (Show, Generic, Functor)

data Expr a = LitExpr { _litExpr :: Lit a } 
            | Variable { _varName :: String, _exprInfo :: a }
            | Neg { _negExpr :: Expr a, _exprInfo :: a }
            | Plus { _leftExpr :: Expr a, _rightExpr :: Expr a, _exprInfo :: a }
     deriving (Show, Generic, Functor)
     
data Instr a = Assign { _leftSide :: Expr a, _rightSide :: Expr a, _instrInfo :: a }
             | Sequence { _seqInstrs :: ASTList Instr a, _instrInfo :: a }
     deriving (Show, Generic, Functor) 
     
data Decl a = Procedure { _procName :: String
                        , _procInstr :: Instr a
                        , _procInfo :: a 
                        }
     deriving (Show, Generic, Functor)
    
    
-- * Instances for AST

instance Show a => ToSourceRose Lit a
instance Show a => ToSourceRose Expr a
instance Show a => ToSourceRose Instr a
instance Show a => ToSourceRose Decl a

instance ASTNode Lit a
instance ASTNode Expr a
instance ASTNode Instr a
instance ASTNode Decl a
    
makeLenses ''Lit
instance Data a => Plated (Lit a) where
  plate = uniplate    
makeLenses ''Expr
instance Data a => Plated (Expr a) where
  plate = uniplate    
makeLenses ''Instr
instance Data a => Plated (Instr a) where
  plate = uniplate
makeLenses ''Decl
instance Data a => Plated (Decl a) where
  plate = uniplate  
  
negLits :: forall a . (Data a, Typeable a) => Instr a -> Instr a
negLits = transformOn (biplate :: Simple Traversal (Instr a) (Expr a)) $ \case
   Neg li@(LitExpr _) a -> fmap (const a) li & litExpr.litValue %~ negate
   other                -> other
  
 

-- deriveSmartTrav Nothing [t| Lit |]
  
instance SmartTrav Expr where 
  smartTrav desc asc f (LitExpr lit) 
    = LitExpr 
       <$> (desc *> smartTrav desc asc f lit <* asc)
  smartTrav desc asc f (Variable varName info) 
    = Variable 
       <$> pure varName 
       <*> f info
  smartTrav desc asc f (Neg expr info) 
    = Neg
       <$> (desc *> smartTrav desc asc f expr <* asc)
       <*> f info
  smartTrav desc asc f (Plus lhs rhs info) 
    = Plus
       <$> (desc *> smartTrav desc asc f lhs <* asc)
       <*> (desc *> smartTrav desc asc f rhs <* asc)
       <*> f info
  
instance SmartTrav Instr where 
  smartTrav desc asc f (Assign lhs rhs info) 
    = Assign 
       <$> (desc *> smartTrav desc asc f lhs <* asc)
       <*> (desc *> smartTrav desc asc f rhs <* asc)
       <*> f info
  smartTrav desc asc f (Sequence instrs info) 
    = Sequence 
       <$> (desc *> smartTrav desc asc f instrs <* asc)
       <*> f info
       
instance SmartTrav Decl where 
  smartTrav desc asc f (Procedure name instr info) 
    = Procedure 
       <$> pure name
       <*> (desc *> smartTrav desc asc f instr <* asc)
       <*> f info
  
-- | Higher kinded version
instance SmartTrav e => SmartTrav (ASTList e) where 
  smartTrav desc asc f ASTNil = ASTNil
  smartTrav desc asc f (ASTCons head tail info) 
    = ASTCons 
       <$> (desc *> f head <* asc)
       <$> (desc *> smartTrav desc asc (smartTrav desc asc f) tail <* asc)
       <*> f info  

  

  
  
-- * Lexer
lexical :: T.GenTokenParser CStream CUserState CMonad
lexical = T.makeTokenParser (T.LanguageDef
           { T.commentStart    = ""
           , T.commentEnd      = ""
           , T.commentLine     = ""
           , T.nestedComments  = False
           , T.identStart      = letter <|> oneOf "_"
           , T.identLetter	 = alphaNum <|> oneOf "_"
           , T.opStart         = oneOf ""
           , T.opLetter        = oneOf ""
           , T.reservedOpNames = []
           , T.reservedNames   = []
           , T.caseSensitive   = True
           })

integer :: CParser Integer
integer = T.integer lexical

identifier :: CParser String
identifier = T.identifier lexical

symbol :: String -> CParser String
symbol = T.symbol lexical

braces :: CParser a -> CParser a
braces = T.braces lexical

-- * Parser
    
parseLit :: CParser (Lit BasicInfo)
parseLit = withInfo $ IntLit <$> integer
    
parseTerm :: CParser (Expr BasicInfo)
parseTerm = (LitExpr <$> parseLit) 
              <|> (withInfo $ Variable <$> identifier) 
              
parseExpr :: CParser (Expr BasicInfo)
parseExpr = try (withInfo $ Plus <$> parseTerm <* symbol "+" <*> parseExpr) 
              <|> try (withInfo $ Neg <$> (symbol "-" *> parseExpr)) 
              <|> parseTerm 
              
parseInstr :: CParser (Instr BasicInfo)
parseInstr = withInfo $ (Assign <$> parseExpr <* symbol ":=" <*> parseExpr <* symbol ";") 
               <|> (Sequence <$> braces (withInfo $ ASTList <$> many parseInstr)) 
     
-- * Tests
     
test00 :: Expr BasicInfo    
test00 = test parseExpr "1"   
     
test0 :: Expr BasicInfo    
test0 = test parseExpr "a+b"   
   
test1 :: Expr BasicInfo    
test1 = test parseExpr "1+2+3" 

test2 :: Instr BasicInfo
test2 = test parseInstr "{ a:=1; b:=2; }" 

test3 :: Instr BasicInfo
test3 = test parseInstr "{ a:=-1; b:=a+2; }" 

test p = fromRight' . flip runReader emptyRewSet . runParserT (whole p) () "" 



