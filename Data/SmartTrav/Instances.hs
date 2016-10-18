module Data.SmartTrav.Instances where

import Data.SmartTrav.Class
import Control.Applicative
import Data.Traversable
 
instance SmartTrav [] where
  smartTrav _ _ = traverse