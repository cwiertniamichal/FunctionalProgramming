module SpecHU (runHU) where

import Test.HUnit

import Lib(foo)
import CParser
import AST 

test1 = TestCase (assertEqual "for (foo 3, has to pass)," (1,2) (foo 3))
--test_failed = TestCase (assertEqual "for (foo 3, has to fail)," (1,3) (foo 3))
--test_2_failed = TestCase (assertEqual "for (foo 4) has to fail" (1,3) (foo 4)) 

test_simple = TestCase (
        do
            ast <- parseProgram "files/simple.txt"
            assertEqual 
                "Simple test, should pass" 
                "[StmtBlock (Decl [(TInt,\"a\",Just (IntConst 0))])]" 
                (show ast)
        )
{-
test_trial = TestCase (

            assertEqual
                "Simple"
                [FunDefBlock (FunDef TInt "fun" [Argument TInt "a"] (Seq [Decl [(TInt,"b",Nothing)],Assign "b" (IntConst 1),Return (IntConst 1)]))]
                (parseProgram "files/trial1.txt")
    )
-}

test_trial = TestCase (
        do 
            ast <- (parseProgram "files/trial1.txt")
            assertEqual 
                "Simple "
                [StmtBlock (Decl [(TInt,"a",Just (IntConst 0))])]
                ast
    )

{-
test2 = TestCase (do (x,y) <- partA 3
                     assertEqual "for the first result of partA," 5 x
                     b <- partB y
                     assertBool ("(partB " ++ show y ++ ") failed") b)
-}

tests = 
    TestList[
        TestLabel "test1" test1
        , TestLabel "test_simple" test_simple
        , TestLabel "test_trial" test_trial
        --,TestLabel "test_failed" test_failed,
        --,TestLabel "test_2_failed" test_2_failed
    ]


runHU :: IO ()
runHU = do
    putStr "\n\nTrying HUnit:\n"
    putStr "===============================================\n"
    runTestTT tests
    putStr "===============================================\n\n"
