
-- | Preprocessor for C
module MiniC.MiniCPP where

import Text.Parsec
import Data.Maybe
import Data.List (find)
import Control.Monad.Trans.Class (lift)
import Control.Applicative hiding ((<|>), many, optional)
import System.Directory
import System.FilePath

import Text.Preprocess.Parser
import Text.Parsec.ExtraCombinators
import MiniC.Parser.Lexical

-- TODO: macro concatenation with ##
-- TODO: multiline macros with \
-- TODO: include directories
  
cPreproc = defaultPreprocessor 
  { macroDef = 
     do try $ newline *> symbol "#define"
        -- do not capture whitespace from macro value
        name <- identOrReserved
        params <- option [] (try (whiteSpace *> openParen) *> (identOrReserved `sepBy1` comma) <* char ')') 
        val <- manyTill anyChar (skip (lookAhead newline) <|> eof)
        return $ MacroDef name Just (execMacro name params val)
      <?> "macro definition"
  , macroAppl =
     do name <- try identOrReserved
        params <- option [] (char '(' *> (macroArg `sepBy1` char ',') <* char ')')
        return (MacroAppl name params)
      <?> "macro application"
  , includeDirective = \file -> 
     do try $ newline *> symbol "#include"
        name <- (char '"' *> many (noneOf "\"") <* char '"')
                   <|> (char '<' *> many (noneOf ">") <* char '>' )
        let incFile = combine (takeDirectory file) name
        exist <- lift $ doesFileExist incFile
        src <- lift $ if exist then readFile incFile else return ""
        return (src, incFile)
  , condDirective = \st -> 
     do ifdef <- try $ newline *> (try (symbol "#ifdef") *> return id
                                     <|> symbol "#ifndef" *> return not)
        macro <- identOrReserved
        let res = ifdef $ isJust (find ((==macro) . name) (defs st))
        thenB <- (if res then id else revertState) $ preprocess cPreproc
        elseB <- option [] (try (newline *> string "#else") 
                             *> (if res then revertState else id) (preprocess cPreproc))
        newline *> string "#endif" <?> "#endif"
        return [NoPP (if res then thenB else elseB)]
  , failDirective = try (newline *> char '#') *> fail "unexpected macro"
  , identToken = identOrReserved
  }
    where execMacro name params val (MacroAppl applName actParams)
            = let replaceParser :: Parsec String () [PPRes]
                  pAssoc = zip params actParams
                  replaceChoice = choice $ map (\(from,to) -> (try (string from) *> return (PPAgain to))) pAssoc
                                           ++ map (\(from, to) -> (try (string ("#"++from)) *> return (NoPP ("\""++to++"\"")))) pAssoc
                  replaceParser = many (replaceChoice 
                                          <|> (NoPP <$> identOrReserved) 
                                          <|> ((NoPP . (:[])) <$> anyChar)
                                          )
               in if name == applName 
                    then Just $ if length params /= length actParams 
                           then Left $ name ++ ": Expected " ++ show (length params) 
                                            ++ " arguments, received " ++ show (length actParams)
                           else Right $ either (error . ("Macro replace failed: "++) . show) id 
                                               (runParser replaceParser () "<macroreplace>" val)
                    else Nothing

                    
                    
macroArg :: Monad m => ParsecT String u m String
macroArg = macroArg' True

macroArg' :: Monad m => Bool -> ParsecT String u m String
macroArg' commaForb 
  = concat <$> many (((:[]) <$> noneOf ((if commaForb then (',':) else id) "()'\"")) 
                       <|> ((\a->"("++a++")") <$> (char '(' *> macroArg' False <* char ')'))
                       <|> ((\a->['\'',a,'\'']) <$> simpleCharLiteral)
                       <|> ((\a->'"':a++"\"") <$> simpleStringLiteral)
                       )
                       
    
-- | The source that could possibly be rewritten if there is a corresponding macro definition.    
data MacroAppl
  = MacroAppl { macroApplName :: String
              , macroApplParams :: [String]
              }
              