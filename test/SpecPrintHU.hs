module SpecPrintHU (runPrintHU) where

import Test.HUnit

import CParser

test_simple = TestCase (
        do
            ast <- (parseProgram "files/testfiles/simple_decl.txt")
            assertEqual 
                "Simple test, should pass" 
                "DECL \n|=\n||a\n||0\n" 
                (show ast)
        )

tests = 
    TestList[
        TestLabel "test_simple" test_simple
    ]


runPrintHU :: IO ()
runPrintHU = do
    putStr "\n\nTrying Printing HUnit:\n"
    putStr "===============================================\n"
    runTestTT tests
    putStr "===============================================\n\n"
