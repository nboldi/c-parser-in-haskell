{-# LANGUAGE TypeOperators, FlexibleInstances, FlexibleContexts, DefaultSignatures #-}
{-# LANGUAGE MultiParamTypeClasses, OverlappingInstances, LambdaCase #-}

-- | A module for representing how to print the AST in the original format
module MiniC.SourceNotation where

import SourceCode.ASTNode
import SourceCode.SourceTree

import GHC.Generics
import Control.Applicative
import Data.Maybe
import Data.Either
import Data.List
import Data.Function
import Debug.Trace
import Text.Parsec.PosOps
    
-- | A pattern that controls how the original source code can be
-- retrieved from the AST. A source template is assigned to each node.
-- It has holes where the content of an other node should be printed.
type SourceTemplate = [SourceTemplateElem]   
 
data SourceTemplateElem = TextElem String 
                        | NodeElem NodeIndex
     deriving (Eq, Ord)
     
isTextElem :: SourceTemplateElem -> Bool
isTextElem (TextElem _) = True
isTextElem _ = False

isNodeElem :: SourceTemplateElem -> Bool
isNodeElem (NodeElem _) = True
isNodeElem _ = False
      
instance Show SourceTemplateElem where
  show (TextElem s) = s
  show (NodeElem ni) = "<" ++ show ni ++ ">"
     
getIndexedNode :: SourceTemplateElem -> Maybe NodeIndex
getIndexedNode (NodeElem ni) = Just ni
getIndexedNode _ = Nothing
     
steCombine :: SourceTemplateElem -> SourceTemplateElem -> SourceTemplateElem
steCombine (TextElem s1) (TextElem s2) = TextElem (s1 ++ s2)
steCombine (TextElem "") e = e
steCombine e (TextElem "") = e
steCombine e1 e2 = error $ "Source template elements " 
                             ++ show e1 ++ " and " ++ show e2 
                             ++ " cannot be combined."

-- | Creates a template from a source range and an input string by cutting out each given node
createTemplate :: [(SourceRange, NodeIndex)] -> SourceRange -> String -> SourceTemplate
createTemplate toCutOut sr input
  = let source = takeSourceRange (rngFromStart sr) input
        sortedNodes = sortBy (flip compare `on` fst) toCutOut
     in foldl (cutOutNode sr) [TextElem source] sortedNodes
  where -- | Must be applied to the last child to cut out, so the prefix remains an intacts string
        cutOutNode :: SourceRange -> SourceTemplate -> (SourceRange, NodeIndex) -> SourceTemplate
        cutOutNode sr (TextElem txt : rest) (cutOutSr, index) = 
            let cutOutRng = cutOutSr `rangeRelativelyTo` srcRangeBegin sr
                textBefore = snd $ takeToPos' (srcRangeBegin cutOutRng) txt
                textAfter = snd $ dropToPos' (srcRangeEnd cutOutRng) txt 
             in TextElem textBefore : NodeElem index : TextElem textAfter : rest

      
