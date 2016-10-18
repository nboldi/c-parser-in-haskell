{-# LANGUAGE FlexibleContexts, MultiParamTypeClasses, TypeOperators, TupleSections #-}

-- | Tests for the C parser
module MiniC where
 
import MiniC.ParseProgram
import MiniC.Parser
import MiniC.Parser.Lexical
import MiniC.Parser.Base hiding (tuple)
import MiniC.AST
import MiniC.MiniCPP
import MiniC.Semantics
import MiniC.SymbolTable
import MiniC.Representation
import MiniC.Helpers
import MiniC.Instances
import MiniC.PrettyPrint
import MiniC.SourceNotation
import MiniC.TransformInfo
import Text.Preprocess.Parser
import SourceCode.ToSourceTree (ToSourceRose)
import SourceCode.SourceInfo (noNodeInfo)
import SourceCode.ASTElems

import GHC.Generics
import Text.Parsec
import Text.Parsec.Error
import Text.Preprocess.Rewrites
import Debug.Trace
import Data.SmartTrav
import Data.Maybe
import Data.Map(fromList)
import Data.Function
import Data.Either.Combinators
import Control.Arrow
import Control.Applicative hiding ((<|>), many)
import Control.Lens
import Control.Monad
import Control.Monad.Reader
import System.IO.Unsafe (unsafePerformIO)
import System.Timeout
import System.FilePath
import System.Directory
import Test.HUnit hiding (test)
import Test.HUnit.Find

test = tests >>= runTestTT
  
tests = do progTests <- programTests 
           return $ TestList 
                      [ TestLabel "literalTests" literalTests
                      , TestLabel "exprTests" exprTests
                      , TestLabel "instrTests" instrTests
                      , TestLabel "typeTests" typeTests
                      , TestLabel "declarationTests" declarationTests
                      , TestLabel "stmtDeclTests" stmtDeclTests
                      , TestLabel "macroTests" macroTests
                      , TestLabel "ifTests" ifTests
                      , TestLabel "programTests" progTests
                      ]
          
      
-- * Literal tests
      
literalTests = TestList $ map (\lit -> TestLabel lit $ TestCase (assertParsedAST equallyPrinted literal lit)) literals

integerLiterals = [ "1", "0", "-1", "215u", "0xFeeL", "01", "30ul", "0b011010" ]
floatLiterals = [ "0.123", "1.4324", "1e-6", "1e-6L", "0.123f", ".1E4f", "58."
                , "0x1.999999999999ap-4", "0x1p-1074", "0xcc.ccccccccccdp-11" ]
charLiterals = map (\cl -> "'" ++ cl ++ "'") 
                   [ "c", "\\n", "\\0", "\\xAA", "\\0123", "\\u00e9" ]
stringLiterals = map (\sl -> "\"" ++ sl ++ "\"") 
                     [ "bla", "almafa\\n", "almafa\\0", "\\xAA" ]
                  ++ ["L\"r\\u00e9sum\\u00e9\""]
compoundLiterals = [ "{}", "{ 1,2,3 }", "{ .x = 1, .y = 2 }"
                   , "{ [0] = 1, [2] = 3 }"
                   , "{ [0] = 1, 2, [2] = 3 }" ]

literals = integerLiterals 
             ++ floatLiterals 
             ++ charLiterals 
             ++ stringLiterals
             ++ compoundLiterals

-- * Expressions tests
             
-- | Tests that each expression is parsed successfully and printed back the same
exprTests = TestList $ map (\expr -> TestLabel expr $ TestCase (assertEqualPrint expr)) expressions
  where assertEqualPrint = assertParsedAST equallyPrinted (addVars exprVarsToAdd >> expression)

exprVarsToAdd = ["a","b","c","i","n","x","f","g","scanf","printf"]
           
expressions = [ "&a", "*a", "*&a++"
              , "a+b", "a = c , b"
              , "a += *c , b"
              , "x(1)"
              , "x[0]"
              , "x[0][1]"
              , "x[0](1)"
              , "(g)()"
              , "(*f)(a,b)"
              , "f(a,g)"
              , "a>b?a:b"
              , "a?:b"
              , "a>b?&a:&b"
              , "(int) 1"
              , "(int) a"
              , "(int) (a+1)"
              , "sizeof(a)"
              , "sizeof(int)"
              , "1+sizeof(int)"
              , "sizeof( struct { int a; } )"
              , "scanf(\"%d\",n)"
              , "scanf(\"%d\",&n)"
              , "scanf(\"%d\",a+b)"
              , "printf(\"%d\",a)"
              , "a[i][i].x[i]" 
              ] ++ literals
              
-- * Instruction tests
     
-- | Tests that each statement is parsed successfully and printed back the same
instrTests 
  = TestList $ map (\instr -> TestLabel instr $ TestCase (assertEqualPrint instr)) instructions
  where assertEqualPrint = assertParsedAST equallyPrinted (addVars instrVarsToAdd >> statement)

instrVarsToAdd = exprVarsToAdd ++ ["c","it","msg","next","sum","y"]
        
instructions = [ "label : a = c;"
               , "a += 4;"
               , "f(4); // Bazz@#&#& "
               , "f( /* Bazz@#&#& */ 4);"
               , "f (1);"
               , "a += (b + 4);"
               , "{ }"
               , "{ a = b = 10; a += (b + 4); b = a - 1; }"
               , "{ int a = 10; }"
               , "{ int a = 10; a = 11; }"
               , "{ int a = 10; a = 11; int b = 12; b += a; }"
               , "if (a==4) b = 10; else { x = 0; y = 0; }"
               , "if (a==4) if (b==1) c = 10;"
               , "switch (x) { case 0 : msg = \"brumm\"; break; "
                     ++ " case 1 : msg = \"brummbrumm\"; break; "
                     ++ " default : break; "
                     ++ "}"
               , "while (x > 0) --x;"
               , "while (next(it));"
               , "do ++x; while (x < 100);"
               , "for (i=0; i<n; ++i) { sum += i*i; }"
               , "for (int i=0; i<n; ++i) { sum += i*i; }"
               , "{ return 10; break; continue; }"
               , "((void ( * ) (void)) 0 ) ( );"
               , "asm (\"movb %bh (%eax)\");"
               , "asm (\"movl %eax, %ebx\\n\\t\" \"movl $56, %esi\\n\\t\");"
               , "asm (\"cld\\n\\t\" \"rep\\n\\t\" \"stosl\" : : \"c\" (count), \"a\" (fill_value), \"D\" (dest) : \"%ecx\", \"%edi\" );"
               , "asm volatile (\"sidt %0\\n\" : :\"m\"(loc));"
               , "return 0;"
               ] ++ map (++";") expressions
  
-- * Type tests
          
typeTests = TestList $ map (\typ -> TestLabel typ $ TestCase (assertEqualPrint typ)) types
  where assertEqualPrint = assertParsedAST equallyPrinted (addVars typesVarsToAdd >> qualifiedType)
typesVarsToAdd = ["strlen","s1","s2","x"]     

baseVarTypes = [ "void", "int", "int*"
               , "int[5]", "int[]", "int*[10]" 
               ]

varTypes = baseVarTypes ++ map (++"*") baseVarTypes
            ++ [ "void (*foo)(int)", "void *(*foo)(int *)", "void (*)(void)"
               , "void (*) (int* (*) (int*,int), char (*) (char))" 
               , "int* (*) ()" 
               , "int* (*) (int*,int)" 
               , "char (*) (char)" 
               , "struct myType" 
               , "enum myType" 
               , "union myType" 
               , "char str[strlen (s1) + strlen (s2) + 1]" 
               , "typeof (int)" 
               , "typeof (int *)" 
               , "typeof (x[0](1))" 
               , "typeof (*x)" 
               ]

funTypes = [ "void f()", "int main()", "int main(void)", "int f(int)", "int f(int a)", "int f(int,int)" ]
          
types = funTypes ++ varTypes
               
-- * Declaration tests
      
declarationTests = TestList $ map (\dd -> TestLabel dd $ TestCase (assertEqualPrint dd)) declarations
  where assertEqualPrint = assertParsedAST equallyPrinted (addVars ddVarsToAdd >> declaration)
ddVarsToAdd = typesVarsToAdd  
          
declarations = [ "int a;"
               , "int a asm(\"r1\");"
               , "struct addr *p;"
               , "int[5] is;"
               , "int a, b;"
               , "int a, b asm(\"r1\");"
               , "int (*arr2)[8];"
               , "int *arr2[8];"
               , "int a[];"
               , "int a[8];"
               , "int *a[8];"
               , "int a [static 8];"
               , "void f(int [static 8]);"
               , "const int * a;"
               , "int const * a;"
               , "int const * const a;"
               , "int restrict * volatile a;"
               , "int const * restrict a;"
               , "int a, b = 10, c;"
               , "void *pt, *pt2, *pt3;"
               , "int a = 1, b = 2, c = 3;"
               , "int a = 1, b;"
               , "int a, b = 2, c = 3;"
               , "int a = 4;" 
               , "enum myEnum;" 
               , "enum myEnum { a, b, c };" 
               , "enum myEnum { a, b, c, };" 
               , "int whitespace[256] = { [' '] = 1, ['\\t'] = 1, ['\\f'] = 1, ['\\n'] = 1, ['\\r'] = 1 };"
               , "int *(*(i));"
               , "static void (*f)();"
               , "void f() asm (\"myLab\");"
               , "int main() { return 0; }"
               , "int main(void) { return 0; }"
               , "typedef int (*bar)(int);"
               , "typedef struct { int i; } b;"
               , "struct { union { int b; float c; }; int d; } foo;"
               ] ++ map (++" a;") varTypes
                 ++ map (++" a = 0;") varTypes
                 ++ map (++";") funTypes
                 ++ map (++" {}") funTypes
               
stmtDeclTests = TestList []
               
-- * Macro tests
               
macroTests = TestList $ map (\(tu,prep) -> TestCase (assertParsedSame eqAST (addVars vars >> whole translationUnit) tu prep)) macros
  where vars = ["a","b","c","f","p","printf"]
          
macros = [ ("\n#define a int x;\n a","int x;")
         , ("\n#define declare(typ,name) typ name;\n declare(int,x)", "int x;") 
         , ("\n#define min(X, Y)  ((X) < (Y) ? (X) : (Y))\n"
               ++ "int x = min(a, b), y = min(1, 2), z = min(a + 28, *p);"
           , "int x = ((a) < (b) ? (a) : (b))," 
               ++ "y = ((1) < (2) ? (1) : (2)),"
               ++ "z = ((a + 28) < (*p) ? (a + 28) : (*p));"
           )
         , ("\n#define pair(t1,t2,name) struct name { t1 first; t2 second; };\n"
               ++ "pair(int,bool,ib)\n" ++ "pair(int,bool,)"
           , "struct ib { int first; bool second; };\n struct { int first; bool second; };")
         , ("\n#define PRINT(x) printf(#x)\n int main() { PRINT(a); }"
           ,"int main() { printf(\"a\"); }"
           )
         , ("\n#define x y\n int xx;", "int xx;")
         , ("\n#define X 1\n void g() { return (X); }", "void g() { return (1); }")
         , ("\n#define F(x) f x\n int main(){ F((1,2,3)); }", "int main(){ f(1,2,3); }")
         , ("\n#define FAIL_IF(condition, error_code) if (condition) return (error_code)" 
              ++ "\n int main(){ FAIL_IF(c=='(',0); }", "int main(){ if (c=='(') return (0); }")
         ]
         
ifTests = TestList $ map (\(tu,prep) -> TestCase (assertParsedSame eqAST (whole translationUnit) tu prep)) ifs
         
ifs = [ ("\n#ifdef D\n int x; \n#endif", "")
      , ("\n#define D\n#ifdef D\n int x; \n#endif", "int x;")
      , ("\n#ifdef D\nint x;\n#else\nint y;\n#endif", "int y;")
      , ("\n#define D\n#ifdef D\n int x; \n#else\n int y; \n#endif", "int x;")
      ]
                        
-- * Sample program tests
  
programTests :: IO Test
programTests = 
  TestList . map (\file -> TestLabel file $ TestCase (testCase file))
    <$> getTestsInDir "testfiles\\ok"
  where testCase file = readFile file >>= assertParsedOkCustom 1000000 file (whole translationUnit)
  
getTestsInDir :: String -> IO [String]
getTestsInDir dir = liftM ( map (combine dir) 
                               . filter (not . (`elem` [".",".."])) 
                          ) (getDirectoryContents dir)
       
useASTSource p f src 
  = do res <- parseWithPreproc (whole p) "(test)" src
       putStrLn $ either show f res 
       
showASTSource p src 
  = do res <- parseWithPreproc (whole p) "(test)" src
       putStrLn $ either show show res 
       
prettyPrintSource p src 
  = do res <- parseWithPreproc (whole p) "(test)" src
       putStrLn $ either show (prettyPrint . transformSourceInfo) res
         
prettyPrintTestFile fileName 
  = do src <- readFile fileName
       res <- parseWithPreproc (whole translationUnit) fileName src
       putStrLn $ either show (prettyPrint . transformSourceInfo) res
  
-- * Helper functions

addVars :: [String] -> CParser ()
addVars varsToAdd 
  = modifyState $ symbolTable.symbolMap .~ fromList entries
  where entries = map ((unsafePerformIO.parseQualName) &&& defaultVarEntry) varsToAdd
        defaultVarEntry name 
          = VariableEntry [] defaultType Nothing Nothing noNodeInfo
        defaultType = QualifiedType (Scalar ASTNothing noNodeInfo) 
                                    (IntType Signed MiniC.AST.Int noNodeInfo) 
                                    noNodeInfo
                                    
equallyPrinted :: (ToSourceRose a BI, ToSourceRose a TemplateInfo, SmartTrav a, Functor a) 
               => String -> a BI -> Maybe String
equallyPrinted s a = let ppres = (prettyPrint . transformSourceInfo) a
  in if ppres == s then Nothing 
                   else Just $ "The result of pretty printing is `" ++ ppres ++ "`"
  
defaultTimeoutMSecs = 100000

assertParsedOk = assertParsedOkCustom defaultTimeoutMSecs "(test)"
assertParsedAST = assertParsedASTCustom defaultTimeoutMSecs "(test)"

-- | Protect tests from infinite loops
withTimeout :: Int -> IO a -> IO a
withTimeout msecs comp
  = do res <- timeout msecs comp 
       return $ fromMaybe (error $ "Did not terminate in " 
                                      ++ show (fromIntegral msecs / 100000.0 :: Double) ++ " seconds.") res

-- | Assert that parse is successful
assertParsedOkCustom :: Int -> String -> CParser a -> String -> Assertion
assertParsedOkCustom msecs srcname
  = assertParsedASTCustom msecs srcname (\_ _ -> Nothing)
         
-- | Assert parse success and check the AST with a function that returns just an error message
-- when the AST is not correct.
assertParsedASTCustom :: Int -> String -> (String -> a -> Maybe String) -> CParser a -> String -> Assertion
assertParsedASTCustom msecs srcname validate parser source 
  = withTimeout msecs $ 
      do res <- parseWithPreproc (whole parser) srcname source
         assertBool ("'" ++ source ++ "' was not accepted: " ++ show (fromLeft' res)) (isRight res)
         case validate source $ fromRight' res of 
           Just err -> assertFailure ("'" ++ source ++ "' was not correct: " ++ err)
           Nothing -> return ()
        
-- | Assert that the parse fails
assertSyntaxError :: (Show a) => CParser a -> String -> (String -> Bool) -> Assertion 
assertSyntaxError = assertSyntaxErrorTimeout defaultTimeoutMSecs 
               
assertSyntaxErrorTimeout :: (Show a) 
  => Int -> CParser a -> String -> (String -> Bool) -> Assertion
assertSyntaxErrorTimeout msecs parser source failMess 
  = withTimeout msecs $
      do res <- parseWithPreproc (whole parser) "(test)" source
         case res of       
           Left pErr -> case errorMessages pErr of 
             err:_ -> assertBool ("`" ++ source ++ "` should fail with a correct message. Failed with: " ++ messageString err) 
                        (failMess (messageString err))
             [] -> assertFailure $ "`" ++ source ++ "` should fail with a correct message. It failed without a message"
           Right val -> assertFailure $ "`" ++ source ++ "` should fail with a correct message. Parsed: " ++ show val

instance Eq ParseError where
  (==) = undefined
  
-- | Assert that two ASTs are structurally equivalent
eqAST :: (Functor a, Eq (a ())) => a b -> a b -> Bool
eqAST = (==) `on` fmap (const ())
  
-- | Parses two inputs with the same parser and compares the results
assertParsedSame :: (Eq a, Show a) => (a -> a -> Bool) -> CParser a -> String -> String -> Assertion
assertParsedSame = assertParsedSameTimeout defaultTimeoutMSecs
  
assertParsedSameTimeout :: (Eq a, Show a) => Int -> (a -> a -> Bool) -> CParser a -> String -> String -> Assertion
assertParsedSameTimeout msecs isSameAs parser s1 s2 
  = withTimeout msecs $ 
      do parseRes1 <- parseWithPreproc (whole parser) "(test)" s1
         parseRes2 <- parseWithPreproc (whole parser) "(test)" s2
         case (parseRes1,parseRes2) of
           (Right result1, Right result2) ->  
                    assertBool ("Parse results from `" ++ s1 ++ "` and `" ++ s2 ++ "` are not the same" )
                                  (result1 `isSameAs` result2)
           (Left err, _) -> assertFailure $ "Paring of `" ++ s1 ++ "` failed with error: " ++ show err
           (_, Left err) -> assertFailure $ "Paring of `" ++ s2 ++ "` failed with error: " ++ show err
  
  