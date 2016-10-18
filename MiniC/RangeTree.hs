{-# LANGUAGE NamedFieldPuns #-}

-- | A 2-Dimensional R-Tree for intervals
module MiniC.RangeTree where 

import Text.Parsec.Pos
import Text.Parsec.PosOps
import MiniC.SourceNotation
import MiniC.Representation
import SourceCode.SourceTree

import Control.Lens
import Data.List
import Data.Maybe
import Debug.Trace

-- | Trees of ranges in a hierarchical structure, with ranges and indices.
data RangeTree = RangeTree { rtRange :: SourceRange 
                           , rtIndex :: RootIndex 
                           , rtChildren :: [RangeTree]
                           }
     deriving (Eq)
 
instance Show RangeTree where
  show = show' 0
    where show' i (RangeTree rng ind children) 
            = "\n" ++ replicate (4*i) '-' ++ shortShowRng rng 
                ++ " <= " ++ show ind ++ concatMap (show' (i+1)) children
 
-- | Generates a range tree from a source rose. Indexes the tree according to the
-- hiearchy of nodes in the original tree.
generateRangeTree :: RootIndex -> SourceRose BasicInfo -> [RangeTree]
generateRangeTree ri (SourceRose inf original children)
  = (case (original, inf ^? biRange) of (True, Just rng) -> insertToRangeTree rng ri; _ -> id)
      $ foldl rangeTreeUnion [] 
                   (zipWith generateRangeTree (map (flip RootIndex ri) [0..]) children)

-- | A tree of one element
singletonRT :: SourceRange -> RootIndex -> RangeTree
singletonRT sr ri = RangeTree sr ri []
        
-- Inserts a new node into a range tree
insertToRangeTree :: SourceRange -> RootIndex -> [RangeTree] -> [RangeTree]
insertToRangeTree sr ri ranges
  = let consumedIn = map (\rng -> if sr `rangeInside` rtRange rng 
                                       && not (ri `rootPrefixOf` rtIndex rng)
                                     then Just (rng { rtChildren = insertToRangeTree sr ri (rtChildren rng) })
                                     else Nothing
                              ) ranges 
     in case length $ filter isJust consumedIn of
          1 -> zipWith fromMaybe ranges consumedIn
          0 -> let (inside, outside) = partition (\rt -> rtRange rt `rangeInside` sr) ranges
                in (singletonRT sr ri) { rtChildren = inside } : outside
          _ -> error "insertToRangeTree: invalid RangeTree"
             
-- | Creates the union of two range trees
rangeTreeUnion :: [RangeTree] -> [RangeTree] -> [RangeTree]
rangeTreeUnion (RangeTree rng ri children : rest) rt2 
  = rangeTreeUnion rest $ insertToRangeTree rng ri $ rangeTreeUnion children rt2
rangeTreeUnion [] rt2
  = rt2 
  
-- | Finds indices of nodes in the tree that are inside the given source range and
-- satisfy the given predicate.
findContainedWhere :: (SourceRange -> RootIndex -> Bool) -> SourceRange -> [RangeTree] -> [RootIndex]
findContainedWhere pred sr (RangeTree rng ind _ : more)
  | rng `rangeInside` sr && pred rng ind
  = ind : findContainedWhere pred sr more
findContainedWhere pred sr (tree : more)
  | rtRange tree `rangeOverlaps` sr
  = findContainedWhere pred sr (rtChildren tree) ++ findContainedWhere pred sr more
findContainedWhere pred sr (_ : more)
  = findContainedWhere pred sr more
findContainedWhere _ sr [] = []
        

