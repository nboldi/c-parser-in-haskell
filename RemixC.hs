{-# LANGUAGE LambdaCase, ViewPatterns #-}

module RemixC where

import CTransform
import Text.Parsec
import qualified Text.Parsec.Token as T
import qualified Text.Parsec.Language as L
import Test.HUnit hiding (test)
import Control.Lens
import Control.Applicative hiding ((<|>),many)
import Data.Maybe
import System.Directory
import System.FilePath
import qualified MiniC
import MiniC.ParseProgram
import MiniC.Parser
import MiniC.Parser.Base
import MiniC.Parser.Lexical
import MiniC.PrettyPrint
import MiniC.TransformInfo
import Debug.Trace

test = tests >>= runTestTT . TestList

tests = do trfTests <- testOracles 
           miniC_tests <- MiniC.tests
           return [ TestLabel "MiniC.tests" miniC_tests
                  , TestLabel "trfTests" trfTests
                  ]
          

testOracles :: IO Test
testOracles
  = do wd <- getCurrentDirectory 
       testfiles <- getDirectoryContents (wd </> "testfiles\\ok")
       let tests = map (\fn -> wd </> "testfiles\\ok" </> fn)
                     . filter (\fn -> takeExtensions fn `elem` [".c",".cpp",".h"])
                     $ testfiles
       TestList . catMaybes 
         <$> mapM fileTest tests
       
  where fileTest :: FilePath -> IO (Maybe Test)
        fileTest file 
          = readFile file
              >>= parseTestFile 
              >>= \case
                (Right (Just param, expected)) -> 
                  do runProgram (param & inputFile %~ (dropFileName file </>)) >>= \case
                       Right result -> mkTest file $ assertTrfEqual param (transformSourceInfo expected) result 
                       Left err -> mkTest file $ assertFailure (show err)
                Right _ -> return Nothing
                Left err -> mkTest file $ assertFailure (show err)
        mkTest file = return . Just . TestLabel file . TestCase
        assertTrfEqual param expected result 
          = assertEqual ("TRANSFORMING " ++ show param ++ "\n\nSHOULD RESULT IN\n\n" 
                           ++ prettyPrint expected  ++ "\n\nRATHER THAN\n\n" 
                           ++ prettyPrint result) 
                        (fmap (const ()) expected) (fmap (const ()) result)
          -- = assertBool ("TRANSFORMING " ++ show param ++ "\n\nSHOULD RESULT IN\n\n" 
                           -- ++ prettyPrint expected ++ "\n\nRATHER THAN\n\n" 
                           -- ++ prettyPrint result) 
                        -- (expected == result)
       
       
parseTestFile = parseWithPreproc ((,) <$> commentParser <*> whole translationUnit) "<test comment>"
      
commentParser :: CParser (Maybe ProgramParams)
commentParser 
  = optionMaybe $
      try (symbol "//") *> symbol "RESULT" *> symbol "OF" *> symbol "`" *> symbol "remix" 
        *> (ProgramParams
             <$> lexeme (many1 (alphaNum <|> oneOf "/\\_-.,"))
             <*> many ( try (symbol "-ii") *> (IntroduceIndirection <$> qualifiedName)
                          <|> try (symbol "-ri") *> (RemoveIndirection <$> qualifiedName)
                          <|> try (symbol "-cc") *> pure CreateSkeleton ))
        <* symbol "`"

configAll 
  = do wd <- getCurrentDirectory 
       testfiles <- getDirectoryContents (wd </> "testfiles\\ok") 
       let tests = map (\fn -> wd </> "testfiles\\ok" </> fn)
                     . filter (\fn -> takeExtensions fn `elem` [".c",".cpp",".h"])
                     $ testfiles
       mapM_ (runProgram . flip ProgramParams [CreateSkeleton]) tests
       