{-# LANGUAGE FlexibleContexts, KindSignatures #-}

-- | Generic components for a preprocessor. Support for macro definitions with or without parameters, comments, file includes, conditional compilation.
module Text.Preprocess.Parser where

import Text.Parsec hiding (label)
import Text.Preprocess.Rewrites
import Text.ParserCombinators.Parsec.Pos
import Text.Parsec.ExtraCombinators
import Control.Monad.Identity
import Control.Monad.Trans
import Control.Applicative hiding ((<|>), many, optional)
import Data.List
import Data.Maybe
import Data.Either
import Debug.Trace

-- | Contains the specification of the preprocessor
data Preprocessor s u appl (m :: * -> *)
  = Preprocessor { macroDef :: ParsecT s (u, PPState s u appl) m (MacroDef appl)
                 , comments :: ParsecT s (u, PPState s u appl) m ()
                 , -- | Should parse all occasions where a macro replacement could occur.
                   macroAppl :: ParsecT s (u, PPState s u appl) m appl
                 , includeDirective :: FilePath -> ParsecT s (u, PPState s u appl) m (String, FilePath)
                 , condDirective :: PPState s u appl -> ParsecT s (u, PPState s u appl) m [PPRes]
                 , failDirective :: ParsecT s (u, PPState s u appl) m ()
                 , identToken :: ParsecT s (u, PPState s u appl) m String
                 }

-- | A default preprocessor with no parsers defined.
defaultPreprocessor :: Preprocessor s u appl m
defaultPreprocessor = Preprocessor { macroDef = fail "No macros defined" 
                                   , comments = fail "No comments defined"
                                   , macroAppl = fail "No macro application defined" 
                                   , includeDirective = fail "No include directive defined"
                                   , condDirective = fail "No cond directive defined"
                                   , failDirective = fail "No failure directive defined"
                                   , identToken = fail "No identifier token defined"
                                   }
    
-- | All information about a macro definition
--
-- Contains it's name (for debugging), the way macro definition conflicts are resolved and a function
-- that decides if a possible source code can be rewritten by this macro and does the rewrite if so.
data MacroDef appl
  = MacroDef { name :: String
             , resolveConflict :: MacroDef appl -> Maybe (MacroDef appl)
             , macroExpand :: appl -> Maybe (Either String [PPRes])
             }
             
instance Show (MacroDef appl) where
  show md = "MacroDef { name = " ++ name md ++ ", ... }"
             
-- | Wraps a part of a generated source code with the information that it is to be preprocessed again or not.
data PPRes = PPAgain { fromPPRes :: String }
           | NoPP { fromPPRes :: String }
                 
-- | Preprocessing state
data PPState s u appl 
  = PPState { defs :: [MacroDef appl] -- ^ Defined macros
            , rewrites :: RewriteSet  -- ^ Performed rewrites
            , genPos :: SourcePos     -- ^ Current position in the generated file, 
                                      -- to keep track of destination position of rewrites
            } deriving Show
       
-- | Creates an initial preprocessing state for parsing a file with the given name.       
initState :: FilePath -> PPState s u appl
initState fileName = PPState [] emptyRewSet (initialPos fileName)
               
-- | The parser type that includes preprocessing state in the user state
type PPParserT s u appl m a 
  = ParsecT s (u, PPState s u appl) m a
                           
-- | Instructs the preprocessor to modify the macro definition table                       
modifyDefs :: (Monad m) => ([MacroDef appl] -> Either String [MacroDef appl]) -> PPParserT s u appl m ()
modifyDefs f = do (u,s) <- getState
                  case f (defs s) of Right defs' -> putState (u,s { defs = defs' })
                                     Left err -> fail err

-- | Registers a rewrite, so it can be undone when the original source is queried
registerRewrite :: (Monad m) => SourcePos -> String -> String -> RewriteTrigger -> PPParserT s u appl m ()
registerRewrite origPos origStr toStr trig
  = modifyState $ \(u,s) -> (u, s { rewrites = addRewrite (createRewrite origPos origStr (genPos s) toStr trig) (rewrites s) })
  
-- | Updates the generated position
moveGenPos :: (Monad m) => String -> PPParserT s u appl m ()
moveGenPos s = modifyState $ \(u,st) -> (u,st { genPos = updatePosString (genPos st) s })
  
-- | Runs the preprocessor on a complete source file
preprocessSource :: Monad m => Preprocessor String u appl m
                -> PPParserT String u appl m (String, (u, PPState String u appl))
preprocessSource pp = (,) <$> preprocess pp <* eof <*> getState
  
-- | Preprocesses the source, returns preprocessing state.
-- Runs while the source is valid for preprocessing. 
-- Can be used to capture branches of conditionally compiled source code.
preprocess :: Monad m => Preprocessor String u appl m
                -> PPParserT String u appl m String
preprocess pp 
  = do dir <- sourceName <$> getPosition
       (concat . concat) <$> many ( try $ do 
         pos <- getPosition
         ((prepRes, file, trig), src) <- captureInputStr $
                      (comments pp >> return ([], dir, CommentRemoval)) 
                         <|> (parseDefine pp >> return ([], dir, MacroDefinition))
                         <|> ( do (r, newFile) <- includeDirective pp dir
                                  return ([PPAgain r], newFile, IncludeTriggered) ) 
                         <|> (try (replaceDefine pp) >>= \ppres -> return (ppres, dir, MacroRewrite)) 
                         <|> (identToken pp >>= \str -> return ([NoPP str], dir, MacroRewrite))
                         <|> (do st <- snd <$> getState
                                 ppres <- condDirective pp st 
                                 return (ppres, dir, IfDefTriggered))
                         <|> (failDirective pp *> undefined)
                         <|> (anyChar >>= \c -> return ([NoPP [c]], dir, MacroRewrite))
         let strRes = concatMap fromPPRes prepRes
         when (src /= strRes) $ do
             registerRewrite pos src strRes trig
         moveGenPos strRes
         mapM (\r -> case r of NoPP s -> return s
                               PPAgain s -> runPreprocAgain file s) prepRes
         )
  where runPreprocAgain file s = do 
          st <- getState
          pos <- getPosition
          res <- lift $ runParserT ( do setPosition (setSourceName pos file)
                                        preprocessSource pp
                                   ) st file s
          case res of
            Right (resTxt, resSt) -> 
              do setState resSt
                 return resTxt
            Left err -> fail (show err)
      
-- | Parses a define directive
parseDefine :: Monad m => Preprocessor String u appl m 
                 -> PPParserT String u appl m ()
parseDefine pp 
  = do (macro, src) <- captureInputStr (macroDef pp)
       modifyDefs (\defs -> case find (\m -> name m == name macro) defs of 
                              Just other -> case resolveConflict macro other of
                                              Just newMacro -> Right $ newMacro : defs
                                              Nothing -> Left "Macro redefined"
                              Nothing -> Right $ macro : defs )
       
-- | Replaces a name of a define with its body
replaceDefine :: Monad m => Preprocessor String u appl m 
                   -> PPParserT String u appl m [PPRes]
replaceDefine pp = try $ do st <- getState
                            appl <- macroAppl pp
                            case partitionEithers $ catMaybes $ map (\md -> macroExpand md appl) (defs $ snd st) of
                              (s:_, _) -> error s
                              ([], []) -> fail ""
                              ([], a:_) -> return a
                
-- | Creates a line comment that starts from the given symbol to the end of line     
lineComment :: Monad m => String -> PPParserT String u appl m ()
lineComment str = do try (string str)
                     skip $ manyTill anyChar (skip (string "\n") <|> eof)
             
-- | Creates a block comment  
blockComment :: Monad m => String -> String -> PPParserT String u appl m ()
blockComment start end = skip $ do string start
                                   manyTill anyChar (try (string end))
 
       