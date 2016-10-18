module Test.HUnit.Find where

import Test.HUnit
import Data.Maybe

-- | Gets a test with the given index
(%%) :: Test -> Int -> Test
(TestLabel _ test) %% i = test %% i
(TestList tests) %% i = tests !! i
test %% i = error $ show test ++ " %% " ++ show i

-- | Gets a test with the given name, recursively
(%?) :: Test -> String -> Test
test %? tname = fromMaybe (error $ "Test with name '" ++ tname ++ "' not found.") (test %?? tname)
  where 
    (TestLabel name test) %?? testname = if name == testname then Just test 
                                                             else test %?? testname
    (TestList tests) %?? testname = listToMaybe $ catMaybes $ map (%?? testname) tests
    (TestCase _) %?? _ = Nothing