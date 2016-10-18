{-# LANGUAGE DeriveDataTypeable #-}

-- | Operations on source positions 
module Text.Parsec.PosOps where

import Text.Parsec
import Text.Parsec.Pos
import Data.Function
import Data.Data
import Debug.Trace

-- * Utils on source positions

-- | Shows a source position in a @line:col@ format, omitting file name.
shortShow :: SourcePos -> String
shortShow sp = show (sourceLine sp) ++ ":" ++ show (sourceColumn sp)
  
-- | Initial position in the same file as 
initPosIn :: SourcePos -> SourcePos
initPosIn sp = initialPos (sourceName sp)
  
takeToPos' sp = takeToPos sp (initPosIn sp)
dropToPos' sp = dropToPos sp (initPosIn sp)
  
takeToPos :: SourcePos -> SourcePos -> String -> (SourcePos, String)
takeToPos to curr (c:s) | curr < to  = let (endPos, res) = takeToPos to (updatePosChar curr c) s
                                        in (endPos, c : res)
takeToPos to curr s                  = (curr, [])

dropToPos :: SourcePos -> SourcePos -> String -> (SourcePos, String)
dropToPos to curr (c:s) | curr < to  = dropToPos to (updatePosChar curr c) s
dropToPos to curr s                  = (curr, s)

-- * Source Ranges

-- | A range of characters in a source file given by a begin and an end position.
data SourceRange = SourceRange { srcRangeBegin :: SourcePos 
                               , srcRangeEnd   :: SourcePos
                               } deriving (Eq, Ord, Typeable, Data)
                               
instance Show SourceRange where
  show sr = show (srcRangeBegin sr) ++ " -- " ++ show (srcRangeEnd sr)

-- | Shows a range in a @line:col-line:col@ format
shortShowRng :: SourceRange -> String
shortShowRng sr = shortShow (srcRangeBegin sr) ++ "-" ++ shortShow (srcRangeEnd sr)

  
-- | Creates a source range between two source positions
srcRange :: SourcePos -> SourcePos -> SourceRange
srcRange pos0 pos1 = if pos0 < pos1 then SourceRange pos0 pos1
                                    else SourceRange pos1 pos0
                               
-- | True, if a source range is completely inside another range
rangeInside :: SourceRange -> SourceRange -> Bool
(SourceRange from1 to1) `rangeInside` (SourceRange from2 to2) 
  = from2 <= from1 && to1 <= to2 && from2 < to1  
  
-- | True, if a source range is completely inside another range
rangeStrictInside :: SourceRange -> SourceRange -> Bool
(SourceRange from1 to1) `rangeStrictInside` (SourceRange from2 to2) 
  = from2 <= from1 && to1 <= to2 && (from2 < from1 || to1 < to2)
                               
-- | True, two source ranges overlap
rangeOverlaps :: SourceRange -> SourceRange -> Bool
(SourceRange from1 to1) `rangeOverlaps` (SourceRange from2 to2) 
  = (from1 <= from2 && from2 <= to1) || (from1 <= to2 && to2 <= to1)

-- | The smallest range that contains the two given range.
srcRngUnion :: SourceRange -> SourceRange -> SourceRange
srcRngUnion sr1 sr2 = SourceRange ((min `on` srcRangeBegin) sr1 sr2) ((max `on` srcRangeEnd) sr1 sr2)

-- | Returns a range that has the same size as the given range but starts from file position of zero.
rngFromStart :: SourceRange -> SourceRange
rngFromStart rng = rangeRelativelyTo rng (srcRangeBegin rng)

-- | Returns an empty range at the beginning of the given range.
rngStartAsRange :: SourceRange -> SourceRange
rngStartAsRange (SourceRange begin _) = SourceRange begin begin

-- | Returns an empty range at the end of the given range.
rngEndAsRange :: SourceRange -> SourceRange
rngEndAsRange (SourceRange _ end) = SourceRange end end

-- | True, iff the range is empty
emptyRange :: SourceRange -> Bool
emptyRange (SourceRange begin end) = (begin == end)

-- | Addition on source positions.
offsetedBy :: SourcePos -> SourcePos -> SourcePos 
sp1 `offsetedBy` sp2 
  = newPos (sourceName sp1) (sourceLine sp1 + sourceLine sp2 - 1) (if sourceLine sp2 == 1 then sourceColumn sp1 + sourceColumn sp2 - 1 else sourceColumn sp2)

-- | Substraction on source positions.
relativelyTo :: SourcePos -> SourcePos -> SourcePos
sp1 `relativelyTo` sp2 
  = let newLine = sourceLine sp1 - sourceLine sp2 + 1
     in newPos (sourceName sp1) newLine (if newLine == 1 then sourceColumn sp1 - sourceColumn sp2 + 1 else sourceColumn sp1)
      
-- | Gets a source range, with a specified position substracted from beginning and end.
rangeRelativelyTo :: SourceRange -> SourcePos -> SourceRange
sr `rangeRelativelyTo` sp = rangeMapBoth (`relativelyTo` sp) sr
  
-- | Applies a function on source positions to both begin and end of range
rangeMapBoth :: (SourcePos -> SourcePos) -> SourceRange -> SourceRange
rangeMapBoth f (SourceRange sp0 sp1) = SourceRange (f sp0) (f sp1)

-- | Takes a substring indicated by a source range from the contents of the file
takeSourceRange :: SourceRange -> String -> String
takeSourceRange (SourceRange from to) s
  = let init = (initialPos (sourceName from))
        (startPos,startsAtPos) = dropToPos from init s
     in snd $ takeToPos to startPos startsAtPos

     