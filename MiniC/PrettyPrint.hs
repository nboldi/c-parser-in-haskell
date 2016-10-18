{-# LANGUAGE FlexibleInstances, FlexibleContexts, UndecidableInstances, NamedFieldPuns #-}

-- | Adds source templates to a source tree and uses them 
-- to pretty print it with the original format.
module MiniC.PrettyPrint where

import SourceCode.SourceTree
import SourceCode.ToSourceTree
import MiniC.SourceNotation
import MiniC.Representation
import MiniC.RangeTree
import Data.Maybe
import Data.List
import Data.Function
import Text.Parsec.Pos
import Text.Parsec.PosOps
import Control.Lens
import Control.Monad
import Control.Applicative

-- | Pretty prints an AST by using source templates stored as node info
prettyPrint :: (Functor node, ToSourceRose node TemplateInfo) 
            => node (NodeInfo TemplateInfo si) -> String
prettyPrint = printRose . toRose . fmap (view sourceInfo)
      
-- | Pretty prints a rose tree according to the source templates remainig from the original AST
printRose :: SourceRose TemplateInfo -> String
printRose r = printRose' [r]
  where printRose' :: [SourceRose TemplateInfo] -> String
        printRose' path = concatMap (printElem path) 
                            . nodeTemplate 
                            . roseInfo 
                            . head 
                            $ path
    
        printElem :: [SourceRose TemplateInfo] -> SourceTemplateElem -> String
        printElem _ (TextElem str) = str
        printElem path ni@(NodeElem i) 
          = case resolveRoseInd i path of Right path -> printRose' path
                                          -- When only a part of the source tree is printed,
                                          -- linked nodes can be missing.
                                          -- Left err -> error ("Error while pretty printing: " 
                                                               -- ++ err ++ " in " ++ show (head path)
                                                               -- ++ "\n\ninside\n" ++ show (head (tail path)))
                                          Left err -> "<???>"
        
