module Data.SmartTrav.Class where

import Control.Applicative
 
class SmartTrav t where
  smartTrav :: Applicative f => f () -> f () -> (a -> f b) -> t a -> f (t b)
  
