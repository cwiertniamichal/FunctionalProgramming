module SpecHU (runHU) where

import Test.HUnit

import Lib(foo)

test1 = TestCase (assertEqual "for (foo 3, has to pass)," (1,2) (foo 3))
test_failed = TestCase (assertEqual "for (foo 3, has to fail)," (1,3) (foo 3))
test_2_failed = TestCase (assertEqual "for (foo 4) has to fail" (1,3) (foo 4)) 

tests = 
    TestList[
        TestLabel "test1" test1, 
        TestLabel "test_failed" test_failed,
        TestLabel "test_2_failed" test_2_failed
    ]

runHU :: IO ()
runHU = do
    putStr "\n\nTrying HUnit:\n"
    putStr "===============================================\n"
    runTestTT tests
    putStr "===============================================\n\n"
