{-# LANGUAGE LambdaCase, DefaultSignatures, TypeOperators, MultiParamTypeClasses, FlexibleContexts
           , FlexibleInstances, OverlappingInstances, ScopedTypeVariables #-}
-- | Generically transforms an ADT into a 'SourceTree'
module SourceCode.ToSourceTree where

import GHC.Generics
import Control.Applicative
import Control.Monad.State
import Data.Either
import Data.Maybe
import Data.SmartTrav
import SourceCode.SourceTree
import SourceCode.ASTNode
import SourceCode.SourceInfo
                         
-- | An intermediate data struture. Used to construct a 'SourceRose' tree.
data SourceRoseCollect a = RoseCollect [SourceRoseCollect a]
                         | RoseInfo a
     
instance Show a => Show (SourceRoseCollect a) where
  show = show' 0
    where show' i (RoseCollect children) 
            = "\n" ++ replicate (2*i) '#' ++ concatMap (show' (i+1)) children
          show' i (RoseInfo inf) 
            = "\n" ++ replicate (2*i) '#' ++ show inf
     
-- | Transforms the intermediate representation 'SourceRoseCollect' to a 'SourceRose'     
collectRose :: forall a . (SourceInfo a) 
            => SourceRoseCollect a -> SourceRose a
collectRose = collectRose' (error "No ancestor node has source info.")
  where collectRose' ancestor (RoseCollect coll) 
          = case partitionRoses coll of 
              ([inf], branch) -> SourceRose inf True 
                                            (map (collectRose' inf) branch)
              ([], branch)    -> let children = map (collectRose' ancestor) branch
                                  in SourceRose (generateInfo ancestor (map roseInfo children)) 
                                                False children
                                            
              (infos, branch) -> SourceRose (generateInfo ancestor infos) False 
                                            (map (collectRose' ancestor) branch)
           where -- | Partition collected nodes into info nodes and subnodes
                 partitionRoses :: [SourceRoseCollect a] -> ([a], [SourceRoseCollect a])
                 partitionRoses = partitionEithers 
                                    . (map $ \case c@(RoseCollect _) -> Right c
                                                   RoseInfo i -> Left i)
           
-- | A class for converting AST tree into a more simpler rose tree, where the nodes are annotated
-- with the node info of the original nodes.
class (SourceInfo inf, SmartTrav n) => ToSourceRose n inf where
  toRose :: n inf -> SourceRose inf
  toRose tree = collectRose . toSourceRose $ tree
  
  -- | Creates an intermediate version of the rose tree
  toSourceRose :: n inf -> SourceRoseCollect inf
  toSourceRose n = evalState (toSrcRoseSt n) [[]]
    where toSrcRoseSt :: n inf -> State [[SourceRoseCollect inf]] (SourceRoseCollect inf)
          toSrcRoseSt n = smartTrav desc asc f n *> gets (RoseCollect . reverse . head)
    
          desc  = modify ([]:)
          asc   = modify (\(x:y:xs) -> ((RoseCollect (reverse x) : y) : xs))
          f inf = modify (\(x:xs) -> (RoseInfo inf : x) : xs)
