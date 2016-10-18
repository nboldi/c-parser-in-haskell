{-# LANGUAGE MultiParamTypeClasses, TypeOperators, FlexibleInstances, FlexibleContexts, DefaultSignatures, OverlappingInstances, ImpredicativeTypes #-}

-- | Provides a derivable class that can be used to access information stored in nodes.
module SourceCode.ASTNode where

import Control.Lens (Lens', lens)
import GHC.Generics
import Control.Applicative
import Data.Maybe

-- | Mark data in the AST that is not a node itself
-- No data type should ever be instantiated with both 'ASTData' and 'ASTNode' classes.
class ASTData d where

-- | Mark a datatype that it is an AST node. Provides access to the node information.
-- TODO : replace with a SmartTrav traversal
class ASTNode a inf where
  info :: Lens' (a inf) inf
  info = lens getInfo (flip setInfo)

  getInfo :: a inf -> inf
  
  default getInfo :: (Generic (a inf), GASTNode (Rep (a inf)) inf)
                  => a inf -> inf
  getInfo = fromMaybe (error "getInfo: No info for node") . ggetInfo True . from
  
  setInfo :: inf -> a inf -> a inf
  default setInfo :: (Generic (a inf), GASTNode (Rep (a inf)) inf)
                  => inf -> a inf -> a inf
  setInfo inf = to . gsetInfo True inf . from
  
-- | Generic class to access info members inside nodes
class GASTNode f inf where
  ggetInfo :: Bool -> f a -> Maybe inf
  ggetInfo _ _ = Nothing
  
  gsetInfo :: Bool -> inf -> f a -> f a
  gsetInfo _ _ k = k
  
-- | No info in empty constructors
instance GASTNode U1 inf where
  
-- | Ignore metainformation
instance (GASTNode a inf) => GASTNode (M1 t c a) inf where
  ggetInfo k (M1 x) = ggetInfo k x
  gsetInfo k inf m1@(M1 x) = M1 $ gsetInfo k inf x
  
-- | Add the results from parts of products, 
-- but can only follow a member if there is only one
instance (GASTNode a inf, GASTNode b inf) 
           => GASTNode (a :*: b) inf where
  ggetInfo _ (a :*: b) = ggetInfo False a <|> ggetInfo False b
  gsetInfo _ inf (a :*: b) = gsetInfo False inf a :*: gsetInfo False inf b
  
-- | Ignore other possiblities
instance (GASTNode a inf, GASTNode b inf) 
           => GASTNode (a :+: b) inf where
  ggetInfo k (L1 x) = ggetInfo k x
  ggetInfo k (R1 x) = ggetInfo k x
  gsetInfo k inf (L1 x) = L1 $ gsetInfo k inf x
  gsetInfo k inf (R1 x) = R1 $ gsetInfo k inf x
  
-- | When a field is the info return / replace it
instance GASTNode (K1 i inf) inf where
  ggetInfo _ (K1 x) = Just x
  gsetInfo _ inf (K1 x) = K1 inf
  
-- | On data nodes inside AST, return Nothing / leave it be
instance ASTData a => GASTNode (K1 i a) inf where
  
-- | On children nodes, if there is only one child try to return the info
-- from there or replace there. This helps to keep AST from containing 
-- redundant info data.
instance ASTNode a inf => GASTNode (K1 i (a inf)) inf where      
  ggetInfo True (K1 x) = Just $ getInfo x
  ggetInfo False _ = Nothing
  
  gsetInfo True inf (K1 x) = K1 $ setInfo inf x
  gsetInfo False inf constr = constr
