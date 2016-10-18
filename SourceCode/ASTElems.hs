{-# LANGUAGE StandaloneDeriving, DeriveFunctor, DeriveGeneric, DeriveDataTypeable, LambdaCase
           , MultiParamTypeClasses, FlexibleInstances, FlexibleContexts, KindSignatures
           , UndecidableInstances, TemplateHaskell #-}

-- | Commonly used AST elements. They abstract the type of additional node information,
-- so it is easier to define instances on them.
module SourceCode.ASTElems where

import SourceCode.ASTNode

-- TODO: add isos for pairs, triples and lists to a pair of a tuple or list and an info
-- TODO: add infos to pairs and triples
-- TODO: _1, _2, _3 lenses for pairs and triples, ix lens for lists, 
--       _left and _right traversals for ASTEither, _just traversal for ASTMaybe
-- TODO: ASTNode instances for all

import Data.Data
import Data.Typeable
import GHC.Generics
import Control.Lens
    
-- | An AST node that wraps another node. This can be used to capture additional input.
data ASTWrapper (n :: * -> *) a
  = ASTWrapper  { _astWrapped :: n a
                , _astWrappedInfo :: a 
                }

makeLenses ''ASTWrapper
    
deriving instance (Show a, Show (n a)) => Show (ASTWrapper n a)
deriving instance (Eq a, Eq (n a)) => Eq (ASTWrapper n a)
deriving instance (Ord a, Ord (n a)) => Ord (ASTWrapper n a)
deriving instance (Functor n) => Functor (ASTWrapper n)
deriving instance (Generic (n a)) => Generic (ASTWrapper n a)    
deriving instance Typeable ASTWrapper
deriving instance (Data a, Data (e a), Typeable e) => Data (ASTWrapper e a)   
 
-- | An AST node that is optional
data ASTMaybe (n :: * -> *) a
  = ASTJust  { _astJust      :: n a }
  | ASTNothing

makeLenses ''ASTMaybe
  
astMaybe :: Iso' (ASTMaybe n a) (Maybe (n a))
astMaybe = iso (\case (ASTJust a) -> Just a; ASTNothing -> Nothing)
               (\case (Just a) -> ASTJust a; Nothing -> ASTNothing)
    
deriving instance (Show a, Show (n a)) => Show (ASTMaybe n a)
deriving instance (Eq a, Eq (n a)) => Eq (ASTMaybe n a)
deriving instance (Ord a, Ord (n a)) => Ord (ASTMaybe n a)
deriving instance (Functor n) => Functor (ASTMaybe n)
deriving instance (Generic (n a)) => Generic (ASTMaybe n a)    
deriving instance Typeable ASTMaybe
deriving instance (Data a, Data (e a), Typeable e) => Data (ASTMaybe e a)

-- | One of the two node types. For example either a declaration or a statement 
-- in languages that the two can be mixed. This is preferred over defining a 
-- new node 'DeclarationOrStatement'.
data ASTEither (n :: * -> *) (m :: * -> *) a
  = ASTLeft  { _astLeft      :: n a }
  | ASTRight { _astRight     :: m a }
  
makeLenses ''ASTEither
  
astEither :: Iso' (ASTEither n m a) (Either (n a) (m a))
astEither = iso (\case ASTLeft a -> Left a; ASTRight a -> Right a)
                (\case Left a -> ASTLeft a; Right a -> ASTRight a)
  
deriving instance (Show a, Show (n a), Show (m a)) => Show (ASTEither n m a)
deriving instance (Eq a, Eq (n a), Eq (m a)) => Eq (ASTEither n m a)
deriving instance (Ord a, Ord (n a), Ord (m a)) => Ord (ASTEither n m a)
deriving instance (Functor n, Functor m) => Functor (ASTEither n m)
deriving instance (Generic (n a), Generic (m a)) => Generic (ASTEither n m a)
deriving instance Typeable ASTEither
deriving instance (Data a, Data (n a), Data (m a), Typeable n, Typeable m) => Data (ASTEither n m a)

-- | Two AST nodes linked together.
data ASTPair (n :: * -> *) (m :: * -> *) a
  = ASTPair  { _astFirst      :: n a
             , _astSecond     :: m a 
             }
             
makeLenses ''ASTPair
             
astGetPair :: ASTPair n m a -> (n a, m a)
astGetPair (ASTPair fst snd) = (fst, snd)
  
deriving instance (Show a, Show (n a), Show (m a)) => Show (ASTPair n m a)
deriving instance (Eq a, Eq (n a), Eq (m a)) => Eq (ASTPair n m a)
deriving instance (Ord a, Ord (n a), Ord (m a)) => Ord (ASTPair n m a)
deriving instance (Functor n, Functor m) => Functor (ASTPair n m)
deriving instance (Generic (n a), Generic (m a)) => Generic (ASTPair n m a)
deriving instance Typeable ASTPair
deriving instance (Data a, Data (n a), Data (m a), Typeable n, Typeable m) => Data (ASTPair n m a)
    
-- | Three AST nodes linked together.
data ASTTriple (n :: * -> *) (m :: * -> *) (p :: * -> *) a
  = ASTTriple { _ast_triple_1       :: n a
              , _ast_triple_2       :: m a 
              , _ast_triple_3       :: p a 
              , _ast_triple_info    :: a
              }
             
makeLenses ''ASTTriple
             
astGetTriple :: ASTTriple n m p a -> (n a, m a, p a)
astGetTriple (ASTTriple fst snd thrd _) = (fst, snd, thrd)
  
deriving instance (Show a, Show (n a), Show (m a), Show (p a)) => Show (ASTTriple n m p a)
deriving instance (Eq a, Eq (n a), Eq (m a), Eq (p a)) => Eq (ASTTriple n m p a)
deriving instance (Ord a, Ord (n a), Ord (m a), Ord (p a)) => Ord (ASTTriple n m p a)
deriving instance (Functor n, Functor m, Functor p) => Functor (ASTTriple n m p)
deriving instance (Generic (n a), Generic (m a), Generic (p a)) => Generic (ASTTriple n m p a)
deriving instance Typeable ASTTriple
deriving instance (Data a, Data (n a), Data (m a), Data (p a), Typeable n, Typeable m, Typeable p) => Data (ASTTriple n m p a)
instance (Generic (n a), Generic (m a), Generic (p a), ASTNode n a, ASTNode m a, ASTNode p a) => ASTNode (ASTTriple n m p) a where
    
-- | A list of AST elements.
data ASTList e a 
  = ASTCons { _listHead      :: e a
            , _listTail      :: ASTList e a 
            , _listInfo      :: a 
            }
  | ASTNil
            
makeLenses ''ASTList
            
astGetList :: ASTList e a -> [e a]
astGetList (ASTCons listHead listTail _) = listHead : astGetList listTail
astGetList (ASTNil) = []
            
deriving instance (Show a, Show (e a)) => Show (ASTList e a)
deriving instance (Eq a, Eq (e a)) => Eq (ASTList e a)
deriving instance (Ord a, Ord (e a)) => Ord (ASTList e a)
deriving instance Functor e => Functor (ASTList e)
deriving instance Generic (e a) => Generic (ASTList e a)
deriving instance Typeable ASTList
deriving instance (Data a, Data (e a), Typeable e) => Data (ASTList e a)

type ASTParenList e = ASTWrapper (ASTList e)

