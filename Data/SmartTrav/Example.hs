{-# LANGUAGE StandaloneDeriving, TemplateHaskell, FlexibleContexts #-}
module Data.SmartTrav.Example where

import Data.SmartTrav.Class
import Data.SmartTrav.Instances
import Data.SmartTrav.TH
import Data.SmartTrav.Indexing
import Control.Applicative


data Lit a = IntLit Integer a
     deriving (Show)

data Expr a = LitExpr (Lit a) 
            | Variable String a
            | Neg (Expr a) a
            | Plus (Expr a) (Expr a) a
     deriving (Show)
     
data Instr a = Assign (Expr a) (Expr a) a
             | Sequence (ASTList Instr a)
     deriving (Show) 
     
data Decl a = Procedure String (Instr a) a 
     deriving (Show)

data ASTList e a 
  = ASTList { _listElems     :: [e a]
            , _listInfo      :: a 
            }
deriving instance (Show a, Show (e a)) => Show (ASTList e a)
     
data ASTMaybe n a
  = ASTJust  { _astJust      :: n a }
  | ASTNothing
deriving instance (Show a, Show (e a)) => Show (ASTMaybe e a)
  
program1 = Procedure "program1" (Sequence (ASTList 
             [ Assign (Variable "a" ()) (LitExpr (IntLit 1 ())) ()
             , Assign (Variable "v" ()) (Plus (Variable "b" ()) 
                                        (LitExpr (IntLit 2 ())) ()) ()
             ] ())) ()
     
deriveSmartTrav ''Lit
deriveSmartTrav ''Expr
deriveSmartTrav ''Instr
deriveSmartTrav ''Decl
deriveSmartTrav ''ASTList
deriveSmartTrav ''ASTMaybe

-- $(thExamine [d| 
    -- instance SmartTrav e => SmartTrav (ASTList e) where 
      -- smartTrav desc asc f (ASTList elems info) 
        -- = ASTList 
           -- <$> (desc *> smartTrav desc asc (smartTrav desc asc f) elems <* asc)
           -- <*> f info  
 -- |])

     
test = indexedTraverse (\_ i -> i) program1
     