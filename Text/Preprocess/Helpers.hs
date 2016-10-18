module Text.Preprocess.Helpers where

import Data.Char (isSpace)

trimWhiteSpace = dropWhile isSpace . reverse . dropWhile isSpace . reverse         
            
updateWhere :: (a -> Maybe a) -> [a] -> Maybe [a]
updateWhere f [] = Nothing
updateWhere f (head:rest) = case f head of
  Just x -> Just (x : rest)
  Nothing -> fmap (head :) (updateWhere f rest)
            