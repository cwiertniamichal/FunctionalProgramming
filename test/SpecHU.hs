module SpecHU (runHU) where

import Test.HUnit

import CParser
import AST 

test_simple = TestCase (
        do
            ast <- parseProgram "files/simple.txt"
            assertEqual 
                "Simple test, should pass" 
                "DECL\n|=\n||a\n||0\n" 
                (show ast)
        )
{-
test_trial2 = TestCase (
            do 
                ast <- (parseProgram "files/trial2.txt")
                assertEqual
                    
                    "Simple"
                    
                    [FunDefBlock (FunDef TInt "trial" [] (Seq [Decl [(TInt,"b",Nothing)],Assign "b" (BinaryOp "+" (IntConst 1) (IntConst 2)),
                    If (BinaryOp "==" (IntConst 1) (IntConst 2)) (Seq [Assign "a" (IntConst 5)]) (Seq [Assign "a" (IntConst 3)])])),FunDefBlock (FunDef TInt "main"
                    [Argument TInt "argc"] (Seq [Assign "a" (Condition (BoolConst True) (IntConst 1) (IntConst 2)),While (BoolConst True) (Seq [Assign "a"
                    (IntConst 1),Assign "b" (IntConst 3)])])),FunDefBlock (FunDef TInt "fun" [Argument TInt "a"] (Seq [Decl [(TInt,"b",Nothing)],Assign "b"
                    (IntConst 1),Return (IntConst 1)]))]

                    ast
                
    )


test_trial1 = TestCase (
        do 
            ast <- (parseProgram "files/trial1.txt")
            assertEqual 
                "Simple "
                [StmtBlock (Decl [(TInt,"a",Just (IntConst 0))])]
                ast
    )

-}
tests = 
    TestList[
        TestLabel "test_simple" test_simple
        --, TestLabel "test_trial1" test_trial1
        --, TestLabel "test_trial2" test_trial2
    ]


runHU :: IO ()
runHU = do
    putStr "\n\nTrying HUnit:\n"
    putStr "===============================================\n"
    runTestTT tests
    putStr "===============================================\n\n"
