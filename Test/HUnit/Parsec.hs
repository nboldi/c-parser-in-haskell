 
module Test.HUnit.Parsec where

import Text.Parsec
import Text.Parsec.ExtraCombinators
import Test.HUnit
 
defaultTimeoutMSecs = 100000

assertParsedOk = assertParsedOkCustom defaultTimeoutMSecs "(test)"

-- | Protect tests from infinite loops
withTimeout :: Int -> IO a -> IO a
withTimeout msecs comp
  = do res <- timeout msecs comp 
       return $ fromMaybe (error $ "Did not terminate in " 
                                      ++ show (fromIntegral msecs / 100000.0) ++ " seconds.") res

assertParsedOkCustom :: Int -> String -> CParser a -> String -> Assertion
assertParsedOkCustom msecs srcname parser source 
  = withTimeout msecs $ 
      do res <- parseProgram (whole parser) srcname source
         assertBool ("'" ++ source ++ "' was not accepted: " ++ show (fromLeft' res)) (isRight res)
         
assertSyntaxError :: (Show a) => CParser a -> String -> (String -> Bool) -> Assertion 
assertSyntaxError = assertSyntaxErrorTimeout defaultTimeoutMSecs 
               
assertSyntaxErrorTimeout :: (Show a) 
  => Int -> CParser a -> String -> (String -> Bool) -> Assertion
assertSyntaxErrorTimeout msecs parser source failMess 
  = withTimeout msecs $
      do res <- parseProgram (whole parser) "(test)" source
         case res of       
           Left pErr -> case errorMessages pErr of 
             err:_ -> assertBool ("`" ++ source ++ "` should fail with a correct message. Failed with: " ++ messageString err) 
                        (failMess (messageString err))
             [] -> assertFailure $ "`" ++ source ++ "` should fail with a correct message. It failed without a message"
           Right val -> assertFailure $ "`" ++ source ++ "` should fail with a correct message. Parsed: " ++ show val

instance Eq ParseError where
  (==) = undefined
  
assertParsedSame :: (Eq a, Show a) => CParser a -> String -> String -> Assertion
assertParsedSame = assertParsedSameTimeout defaultTimeoutMSecs
  
assertParsedSameTimeout :: (Eq a, Show a) => Int -> CParser a -> String -> String -> Assertion
assertParsedSameTimeout msecs parser s1 s2 
  = withTimeout msecs $ 
      do parseRes1 <- parseProgram (whole parser) "(test)" s1
         parseRes2 <- parseProgram (whole parser) "(test)" s2
         case (parseRes1,parseRes2) of
           (Right result1, Right result2) ->  
             assertEqual ( "Parse results from `" ++ s1 ++ "` and `" ++ s2 ++ "` are not equal" )
                           parseRes1 parseRes2
           (Left err, _) -> assertFailure $ "Paring of `" ++ s1 ++ "` failed with error: " ++ show err
           (_, Left err) -> assertFailure $ "Paring of `" ++ s2 ++ "` failed with error: " ++ show err
  