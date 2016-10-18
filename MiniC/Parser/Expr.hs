
-- | A copy of Text.Parsec.Expr with a minor change.
-- The Parsec version lacked the support to parse multiple unary operators.
module MiniC.Parser.Expr 
  ( MiniC.Parser.Expr.buildExpressionParser
  , Assoc(..), Operator(..), OperatorTable
  ) where

import Text.Parsec
import Text.Parsec.Expr

buildExpressionParser :: (Stream s m t)
                      => OperatorTable s u m a
                      -> ParsecT s u m a
                      -> ParsecT s u m a
buildExpressionParser operators simpleExpr
    = foldl makeParser simpleExpr operators
    where
      makeParser term ops
        = let (rassoc,lassoc,nassoc
               ,prefix,postfix)      = foldr splitOp ([],[],[],[],[]) ops

              rassocOp   = choice rassoc
              lassocOp   = choice lassoc
              nassocOp   = choice nassoc
              prefixOp   = choice prefix  <?> ""
              postfixOp  = choice postfix <?> ""

              ambigious assoc op = try $ op >> fail ("ambiguous use of a " ++ assoc
                                                 ++ " associative operator")

              ambigiousRight    = ambigious "right" rassocOp
              ambigiousLeft     = ambigious "left" lassocOp
              ambigiousNon      = ambigious "non" nassocOp

              termP      = do { pre  <- many prefixOp
                              ; x    <- term
                              ; post <- many postfixOp
                              ; return (foldl (flip ($)) (foldr ($) x pre) post)
                              }

              rassocP x  = do { f <- rassocOp
                              ; y  <- do{ z <- termP; rassocP1 z }
                              ; return (f x y)
                              }
                           <|> ambigiousLeft
                           <|> ambigiousNon

              rassocP1 x = rassocP x  <|> return x

              lassocP x  = do { f <- lassocOp
                              ; y <- termP
                              ; lassocP1 (f x y)
                              }
                           <|> ambigiousRight
                           <|> ambigiousNon

              lassocP1 x = lassocP x <|> return x

              nassocP x  = do { f <- nassocOp
                              ; y <- termP
                              ;    ambigiousRight
                               <|> ambigiousLeft
                               <|> ambigiousNon
                               <|> return (f x y)
                              }

           in do { x <- termP
                 ; rassocP x <|> lassocP  x <|> nassocP x <|> return x
                   <?> "operator"
                 }


      splitOp (Infix op assoc) (rassoc,lassoc,nassoc,prefix,postfix)
        = case assoc of
            AssocNone  -> (rassoc,lassoc,op:nassoc,prefix,postfix)
            AssocLeft  -> (rassoc,op:lassoc,nassoc,prefix,postfix)
            AssocRight -> (op:rassoc,lassoc,nassoc,prefix,postfix)

      splitOp (Prefix op) (rassoc,lassoc,nassoc,prefix,postfix)
        = (rassoc,lassoc,nassoc,op:prefix,postfix)

      splitOp (Postfix op) (rassoc,lassoc,nassoc,prefix,postfix)
        = (rassoc,lassoc,nassoc,prefix,op:postfix)