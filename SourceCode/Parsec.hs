module SourceCode.Parsec where

import Text.Parsec
import Control.Applicative hiding ((<|>), many, optional)

import SourceCode.ASTElems

astParseEither :: ParsecT s u m (a i) -> ParsecT s u m (b i) 
               -> ParsecT s u m (ASTEither a b i)
astParseEither p1 p2 
  = (ASTLeft <$> p1) <|> (ASTRight <$> p2)
  

  
