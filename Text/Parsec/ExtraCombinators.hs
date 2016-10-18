{-# LANGUAGE TupleSections #-}

-- | Helper functions for parsing
module Text.Parsec.ExtraCombinators where

import Text.Parsec
import Text.Parsec.PosOps
import Control.Applicative hiding ((<|>), many, optional)
import Data.List
import Data.Function

-- | Chooses one of the strings and returns the correspondig result.
-- Builds a prefix tree to deal with overlapping strings
choose :: Monad m => [(String, b)] -> ParsecT String u m b
choose pars
  = let sortedPars = reverse $ sortBy (compare `on` fst) pars
        groupedPars = groupBy ((==) `on` (take 1 . fst)) sortedPars
        dropFstChar ((c:rest),r) = (rest,r)
        parsifyGroup gr@(([],r):_) = return r
        parsifyGroup gr@(((c:rest),r):_) = char c *> choose (map dropFstChar gr)
     in choice $ map parsifyGroup groupedPars

-- | Only accepts the parse result if it fulfills the given condition. Otherwise causes a parse error with the specified message.
onlyWhen :: String -> (a -> Bool) -> ParsecT s u m a -> ParsecT s u m a
onlyWhen mess pred pars 
  = do r <- pars
       if pred r then return r
                 else fail mess

-- | Parses two elements into a tuple.
tuple :: Monad m => ParsecT s u m a -> ParsecT s u m b -> ParsecT s u m (a,b)
tuple p1 p2 = (,) <$> p1 <*> p2
               
-- | Throws away the parsed element.
skip :: ParsecT s u m a -> ParsecT s u m ()
skip = (>> return ())

-- | Parses an element but reverts the user state afterward.
revertState :: Monad m => ParsecT s u m a -> ParsecT s u m a
revertState p = do st <- getState
                   res <- p
                   setState st
                   return res

-- | Combines the result with the user state.
resWithState :: Monad m => ParsecT s u m a -> ParsecT s u m (a,u)
resWithState p = tuple p getState

-- | Saves the original input that was parsed into the representation
captureInputStr :: (Monad m) => ParsecT String u m a -> ParsecT String u m (a, String)
captureInputStr p = do inpBefore <- getInput
                       (res, rng) <- captureSourceRange p
                       return (res, takeSourceRange (rngFromStart rng) inpBefore)

captureSourceRange :: (Monad m) => ParsecT [s] u m a -> ParsecT [s] u m (a, SourceRange)
captureSourceRange p = do pos0 <- getPosition
                          res <- p
                          pos1 <- getPosition
                          return (res, srcRange pos0 pos1)                          
             
-- | Parses many occasions of an element until the end is parsed successfully. Returns both the parsed elements and the end element.
manyUntil :: (Stream s m t) => ParsecT s u m a -> ParsecT s u m end -> ParsecT s u m ([a], end)
manyUntil p end = scan
  where scan = (([], ) <$> end) <|> ((\e (ls,r) -> (e:ls, r)) <$> p <*> scan)
                 
-- | Parses two different element and gives it back as Left or Right
(<||>) :: ParsecT s u m a -> ParsecT s u m b -> ParsecT s u m (Either a b)
p <||> q = Left <$> p <|> Right <$> q 

-- | Parses any number of transformations and an argument. Applies the argument to the combination of the transformations from left to right.      
manyApp :: Monad m => ParsecT s u m (a -> a) -> ParsecT s u m a -> ParsecT s u m a
manyApp pf pa = do funs <- many pf
                   arg <- pa
                   return (foldl (flip ($)) arg funs)

-- | Returns true if the given element is successfully parsed, false otherwise
ifAccept :: ParsecT s u m a -> ParsecT s u m Bool
ifAccept p = (try p *> return True) <|> return False         

         