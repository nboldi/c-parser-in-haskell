{-# LANGUAGE LambdaCase #-}
module Data.SmartTrav.Indexing where

import Data.SmartTrav.Class
import Control.Monad.State
import Control.Applicative

indexedTraverse :: SmartTrav t => (a -> [Int] -> b) -> t a -> t b
indexedTraverse f
  = flip evalState ([], 0) 
      . smartTrav ( modify (\(st   ,i) -> (i:st ,0)) )
                  ( modify (\(s:st ,_) -> (st   ,s)) ) 
                  ( \a -> f a <$> gets fst <* modify ( \case (s:st,i) -> ((s+1):st,i)
                                                             ([]  ,i) -> ([]      ,i) ) )
                        
  