{-# LANGUAGE LambdaCase, ScopedTypeVariables, FlexibleContexts, MultiParamTypeClasses #-}

-- | The purpose of this module is to transform an AST with basic infos to an AST with templates.
module MiniC.TransformInfo where

import MiniC.Representation
import MiniC.SourceNotation
import MiniC.RangeTree
import MiniC.Semantics
import Control.Lens
import Control.Monad.State
import Control.Monad.Reader
import Data.SmartTrav.Class
import Data.Maybe
import Text.Parsec.PosOps
import SourceCode.SourceTree
import SourceCode.ToSourceTree
import Debug.Trace

        
-- | Creates source templates from simple source strings
transformSourceInfo :: (SmartTrav node, ToSourceRose node BI)
                    => node BI -> node NI
transformSourceInfo = cutOutTemplates . expandNodes

-- | Expands nodes to contain all their children
expandNodes :: SmartTrav node => node BI -> node BI
expandNodes n = evalState (expandNodes' n) [Nothing]
  where expandNodes' :: SmartTrav node => node BI -> State [Maybe BI] (node BI)
        expandNodes' = smartTrav desc asc f
        
        desc = modify (Nothing:)
        asc  = modify (\case (x : y : xs) -- if both exist union, otherwise keep existing
                                          -> maybe y (\x' -> maybe x (Just . (sourceInfo %~ unifyBasicInfo (x' ^. sourceInfo))) y) x : xs)
        f inf 
          = do ni <- gets head
               let newInfo = maybe inf (\ni -> inf & sourceInfo %~ unifyBasicInfo (ni ^. sourceInfo)) ni
               modify (\case (_ : xs) -> Just newInfo : xs)
               return newInfo

-- | Replaces assigned input of nodes with source templates.
-- Goes top-down and replaces the info in every node according to structure of the whole tree.
cutOutTemplates :: forall node . (SmartTrav node, ToSourceRose node BI) 
                => node BI -> node NI
cutOutTemplates ast 
  = let rose = fmap (view sourceInfo) (toRose ast)
     in evalState (runReaderT (cutOutTemplates' ast)
                              (rose, generateRangeTree Root rose))
                  (Root,0)                 

  where cutOutTemplates' :: node BI -> ReaderT (SourceRose BasicInfo, [RangeTree]) 
                                                      (State (RootIndex, Int)) 
                                                      (node NI)
        cutOutTemplates' 
          = do smartTrav ( lift $ modify (\(ri, i) -> (RootIndex i ri, 0)) )
                         ( lift $ modify (\(RootIndex i ri, _) -> (ri, i+1)) )
                         createTempInf
        createTempInf bi 
          = do (tree, rngTree) <- ask
               currInd <- gets fst
               let rng = bi ^?! sourceInfo.biRange
                   inp = bi ^?! sourceInfo.biInput
                   relativeIndAndRng ind = (info,rel)
                     where info = fromMaybe (error "No range for a node that is to be cut out")
                                    . getRange 
                                    . roseInfo 
                                    $ resolveRootInd ind tree
                           rel = ind `indRelativelyTo` currInd 
                   contained = findContainedWhere 
                                 (\rng ind -> not (emptyRange rng) 
                                                && not (ind `rootPrefixOf` currInd)) 
                                 rng rngTree
                   toRemove = map relativeIndAndRng contained 
               return $ bi & sourceInfo .~ TemplateInfo (Just rng) 
                                                        (Just $ createTemplate toRemove rng inp)
