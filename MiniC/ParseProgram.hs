
-- | A module to preprocess and parse C programs
module MiniC.ParseProgram where

import MiniC.Parser
import MiniC.Parser.Base
import MiniC.MiniCPP (cPreproc)
import MiniC.Parser.Lexical (whole)
import MiniC.SymbolTable (initUserState)
import MiniC.Semantics
import MiniC.TransformInfo
import Text.Preprocess.Parser
import Text.Preprocess.Rewrites

import Text.Parsec
import Text.Parsec.Error
import Data.Either.Combinators
import Control.Monad.Reader
import Control.Applicative hiding ((<|>), many, optional)
import Data.List.Split
import MiniC.Parser.Lexical

-- | Parse a whole C translation unit.
parseProgram = parseWithPreproc (whole translationUnit)

-- | Parse something after using the preprocessor on it.
parseWithPreproc :: CParser a -> String -> String -> IO (Either ParseError a)
parseWithPreproc parser name src 
  = do res <- runParserT (preprocessSource cPreproc) ((), initState name) name src
       case res of
           Right (preprocessed,((),st)) ->
             return $ case runReader (runParserT parser initUserState name preprocessed) (rewrites st) of
                Right tu -> Right tu
                Left err -> Left $ setErrorPos (correctPos (rewrites st) (errorPos err)) err
           Left err -> return $ Left $ addErrorMessage (Message "Preprocessor failed") err

 
parseQualName :: String -> IO QualifiedName
parseQualName src = either (error . ("While parsing qualified name: " ++) . show) id
                     <$> parseWithPreproc (whole qualifiedName) "<qualified name>" src
            
qualifiedName :: CParser QualifiedName
qualifiedName = lexeme $ 
    QualifiedName <$> (try (string "$::" *> pure ComplexNS) 
                        <|> (optional (string "::") *> pure NormalNS))
      <*> (Scope <$> many ( try (many1 alphaNum <* string "::") )) 
      <*> (transformSourceInfo <$> typeQualifiedIdentifier)
          