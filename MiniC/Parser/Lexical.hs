{-# LANGUAGE ViewPatterns, ScopedTypeVariables #-}

-- | Lexer for C. Parts from 'Text.Parsec.Token' have been copied here, because they
-- could not handle source information, or literal type specification, so it may
-- be considered as a reimplementation of common lexical elements.
module MiniC.Parser.Lexical where

import MiniC.AST
import MiniC.Instances
import MiniC.Parser.Base
import MiniC.Representation

import Data.Char (digitToInt)
import Control.Lens
import Control.Applicative hiding ((<|>), many, optional)
import Text.Parsec
import Text.Parsec.Token ( GenLanguageDef(..))
import qualified Text.Parsec.Token as P

cStyle :: Monad m => GenLanguageDef CStream u m
cStyle = LanguageDef
           { commentStart    = "/*"
           , commentEnd      = "*/"
           , commentLine     = "//"
           , nestedComments  = False
           , identStart      = letter <|> oneOf "_"
           , identLetter	 = alphaNum <|> oneOf "_"
           , opStart         = oneOf ""
           , opLetter        = oneOf ""
           , reservedOpNames = []
           , reservedNames   = []
           , caseSensitive   = True
           }
           
cDef :: Monad m => GenLanguageDef CStream u m
cDef = cStyle 
         { reservedNames = [ "asm", "auto", "break", "bool", "case", "char", "const", "continue"
                           , "default", "do", "double", "else", "enum", "extern"
                           , "float", "for", "goto", "if", "int", "long", "register"
                           , "return", "restrict", "short", "signed", "sizeof", "static"
                           , "struct" , "switch", "typedef", "typeof", "union", "unsigned", "void"
                           , "volatile", "while" 
                           ]
         }

lexer :: Monad m => P.GenTokenParser CStream u m
lexer = P.makeTokenParser cDef    
  
  

whole :: CParser a -> CParser a
whole p = whiteSpace *> p <* eof
  
-- * Inherited tokens
lexeme :: Monad m => ParsecT CStream u m a -> ParsecT CStream u m a
lexeme = P.lexeme lexer

symbol :: Monad m => String -> ParsecT CStream u m String
symbol = P.symbol lexer
  
parens, braces, brackets :: Monad m => ParsecT CStream u m a -> ParsecT CStream u m a
parens = P.parens lexer
braces = P.braces lexer
brackets = P.brackets lexer

identifier = lexeme $ withInfo $ Ident <$> noWSIdentifier

simpleIdentifier :: Monad m => ParsecT String u m String
simpleIdentifier = lexeme noWSIdentifier

noWSIdentifier :: forall u m . Monad m => ParsecT String u m String
noWSIdentifier = try $
     do name <- identOrReserved
        if name `elem` reservedNames (cDef :: GenLanguageDef CStream u m)
         then unexpected ("reserved word " ++ show name)
         else return name

identOrReserved :: Monad m => ParsecT String u m String
identOrReserved = (:) <$> identStart cDef <*> many (identLetter cDef) <?> "identifier"

reserved :: Monad m => String -> ParsecT String u m ()
reserved = P.reserved lexer

-- | This function decides which symbol can follow another symbol while the whole lexeme remains
-- a valid C operator.
opNoFollow op 
  = concat $ ["=" | op `elem` ["+","-","*","/","%","&","^","|","<",">","<<",">>"]]
               ++ [op | op `elem` ["&","|","+","-",">","<"]]
               ++ [">" | op == "-"]
               
reservedOp :: String -> CParser ()
reservedOp name 
  = try $ lexeme (string name) 
            >> (notFollowedBy (oneOf $ opNoFollow name) <?> ("end of " ++ show name))

whiteSpace :: Monad m => ParsecT String u m ()
whiteSpace = P.whiteSpace lexer

comma, colon, semicolon, openParen, closeParen
  , openBrace, closeBrace, openBracket, closeBracket :: Monad m => ParsecT String u m String
comma = symbol ","
colon = symbol ":"
semicolon = symbol ";"
openParen = symbol "("
closeParen = symbol ")"
openBrace = symbol "{"
closeBrace = symbol "}"
openBracket = symbol "["
closeBracket = symbol "]"

---------------------
-- number literals --
---------------------

-- integer literals
integer :: CParser LiteralBI
integer = lexeme $ withInfo $ IntLit <$> (sign <*> nat) <*> integerSuffix

nat = zeroNumber <|> decimal
int = lexeme sign <*> nat
sign = (char '-' *> return negate) <|> (optional (char '+') *> return id)
zeroNumber = char '0' *> (hexadecimal <|> binary <|> octal <|> return 0) <?> ""

decimal = P.decimal lexer
hexadecimal = P.hexadecimal lexer
octal = number 8 octDigit
binary = oneOf "bB" *> number 2 (oneOf "01")

-------------------------------
-- * floating point literals --
-------------------------------

float :: CParser LiteralBI
float = lexeme floating <?> "float literal"

floating = withInfo $ FloatLit 
  <$> ( try (char '0' *> oneOf "xX" *> floatingBase hexDigit 16 (oneOf "pP"))
           <|> floatingBase digit 10 (oneOf "eE")) 
  <*> optionMaybe fractSuffix 

floatingBase :: CParser Char -> Integer -> CParser a -> CParser Rational
floatingBase digit baseNum exponentSign 
  = try ( combine (option 0 integralPart) (char '.' *> fractionPart) (option 1 exponentPart) )
      <|> try ( combine (integralPart <* char '.') (option 0 fractionPart) (option 1 exponentPart) )
      <|> combine integralPart (option 0 (char '.' *> fractionPart)) exponentPart
  where combine ip fp ep = (*) <$> ( (+) <$> ip <*> fp ) <*> ep
        integralPart = fromIntegral <$> number baseNum digit
        fractionPart = fractNumber baseNum digit
        exponentPart = exponentSign *> ((fromIntegral baseNum ^) <$> (sign <*> decimal))
               
fractSuffix 
  = (oneOf "fF" *> return LitFloat) <|> (oneOf "lL" *> return LitLongDouble)
               
integerSuffix = do suffs <- many (intSignSuffix <|> intSizeSuffix)
                   return $ foldl (flip ($)) (IntLitSpec Nothing Nothing) suffs 
                 
intSignSuffix = try (oneOf "uU") *> return (set intLitSign (Just LitUnsigned) )

intSizeSuffix = (try (string "ll" <|> string "LL") *> return (set intLitSize (Just LitLongLong)))
                  <|> (try (oneOf "lL") *> return (set intLitSize (Just LitLong)))

number :: Monad m => Integer -> ParsecT CStream u m Char -> ParsecT CStream u m Integer
number base baseDigit
    = do{ digits <- many1 baseDigit
        ; let n = foldl (\x d -> base*x + toInteger (digitToInt d)) 0 digits
        ; seq n (return n)
        }

fractNumber :: Integer -> CParser Char -> CParser Rational
fractNumber (fromIntegral -> base) baseDigit
    = do{ digits <- many1 baseDigit
        ; let (n,e) = foldl (\(x,e) d -> (x + (fromIntegral (digitToInt d) / e), e * base)) (0,base) digits
        ; seq n (return n)
        }

hexaNum :: Monad m => ParsecT CStream u m Integer
hexaNum = number 16 hexDigit
                 
--------------------------------
-- * Char and String literals --
--------------------------------
         
unicodePrefix = option False (oneOf "lL" *> return True)      

charLiteral     
  = withInfo ( lexeme $ CharLit <$> unicodePrefix
                 <*> simpleCharLiteral )
      <?> "character"

simpleCharLiteral :: Monad m => ParsecT CStream u m Char
simpleCharLiteral = between (char '\'')
                            (char '\'' <?> "end of character")
                                characterChar

characterChar :: Monad m => ParsecT CStream u m Char                                
characterChar   = do c <- (Just <$> charLetter) <|> escapeCode
                     case c of Just x -> return x
                               Nothing -> characterChar
                <?> "literal character"

charLetter :: Monad m => ParsecT CStream u m Char                                                
charLetter = satisfy (\c -> (c /= '\'') && (c /= '\\') && (c > '\026'))

stringLiteral 
  = withInfo ( StringLiteral <$> unicodePrefix
                             <*> simpleStringLiteral
                             <?> "literal string")

simpleStringLiteral :: Monad m => ParsecT CStream u m String
simpleStringLiteral 
  = lexeme $ foldr (maybe id (:)) ""
          <$> between (char '"') (char '"' <?> "end of string")
                      (many stringChar)

stringChar :: Monad m => ParsecT CStream u m (Maybe Char)
stringChar = (Just <$> stringLetter) <|> escapeCode <?> "string character"

stringLetter :: Monad m => ParsecT CStream u m Char
stringLetter = satisfy (\c -> ((c /= '"') && (c /= '\\') && (c > '\026')) || c == '\n')

-- escape codes
escapeCode :: Monad m => ParsecT CStream u m (Maybe Char)                                
escapeCode = char '\\' *> ((Just <$> try charNum) <|> charEsc) <?> "escape code"

charNum :: Monad m => ParsecT CStream u m Char
charNum = (toEnum . fromInteger) 
            <$> ((char '0' *> number 8 octDigit)
                   <|> (oneOf "xX" *> number 16 hexDigit)
                   <|> (oneOf "uU" *> number 16 hexDigit))
    
charEsc :: Monad m => ParsecT CStream u m (Maybe Char)                                
charEsc = choice (map (\(c,code) -> char c *> return code) escMap)

escMap          = zip "abfnrtv\\\"\'0\n" (map Just "\a\b\f\n\r\t\v\\\"\'\0" ++ [Nothing])



