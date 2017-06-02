module SpecHU (runHU) where

import Test.HUnit
import Control.Exception

import CParser
import AST 

test_simple = 
    TestCase 
        (
            do
            ast <- (parseProgram "files/testfiles/simple_decl.txt")
            assertEqual 
                "Simple test, should pass" 
                (
                    Program 
                    [
                        StmtBlock 0 
                        (
                            Decl 0 [ (TInt 0, "a", Just $ IntConst 0 0) ]
                        )
                    ]
                )
                ast
        )

test_fundef = 
    TestCase 
        (
            do
            ast <- (parseProgram "files/testfiles/fundef.txt")
            assertEqual
                "Fundef"
                (
                    Program 
                    [
                        FunDefBlock 0 (
                            FunDef 
                            0 
                            ( TInt 0 ) 
                            "main" 
                            [
                                Argument 0 ( TInt 0 ) "argc", 
                                Argument 0 ( TInt 0 ) "argv",
                                Argument 0 ( TFloat 0 ) "a"
                            ] 
                            (
                                Seq 0 [ Return 0 $ IntConst 0 0 ]
                            )
                        )
                    ]
                )
                ast 
        )

test_lambda_py = 
    TestCase
        (
            do 
            ast <- (parseProgram "files/testfiles/lambda_py.txt")
            assertEqual
                "Lambda Py"
                (
                    Program 
                    [
                        StmtBlock 0 (
                            Assign 
                            0 
                            "y" 
                            (
                                PythonLambda 0 
                                [ LambdaArg "a", LambdaArg "b" ] 
                                ( BinaryOp 0 "*" (Variable 0 "a") (Variable 0 "b") ) 
                            ) 
                        ) 
                    ] 
                )
                ast
        )


test_lambda_hs = 
    TestCase
        (
            do 
            ast <- (parseProgram "files/testfiles/lambda_hs.txt")
            assertEqual
                "Lambda HS"
                (
                    Program 
                    [
                        StmtBlock 0 (
                            Assign 0 "y" 
                            (
                                HaskellLambda 0 
                                [ LambdaArg "a", LambdaArg "b" ] 
                                (
                                    BinaryOp 0 "+" 
                                    (Variable 0 "a") 
                                    (Variable 0 "b") 
                                ) 
                            ) 
                        ) 
                    ] 
                )
                ast
        )

test_termary = 
    TestCase 
        (
            do 
            ast <- (parseProgram "files/testfiles/termary.txt")
            assertEqual
                "Termary"
                (Program [StmtBlock 0 (Assign 0 "a" (Condition 0 (BoolConst 0 True) (IntConst 0 1) (IntConst 0 2) )) ])
                ast
        )

test_while =
    TestCase 
        (
            do 
            ast <- (parseProgram "files/testfiles/while.txt")
            assertEqual
                "While"
                (
                    Program 
                    [
                        StmtBlock 0 
                        (
                            While 0 ( BoolConst 0 True )  ( Seq 1 [Assign 0 "a" $ IntConst 0 1] )
                        )
                    ]
                )
                ast
        )

test_if_else = 
    TestCase
        (
            do 
            ast <- (parseProgram "files/testfiles/if_else.txt")
            assertEqual
                "If Else Test"
                (
                    Program 
                    [
                        StmtBlock 0
                        (
                            If 0 (BoolConst 0 True) (Seq 0 [Assign 0 "a" $ IntConst 0 1]) (Seq 0 [Assign 0 "a" $ IntConst 0 0])
                        ) 
                    ]
                )
                ast 
        )

test_instructions = 
    TestCase
        (
            do 
            ast <- (parseProgram "files/testfiles/instructions.txt")
            assertEqual
                "Instructions test"
                (
                    Program 
                    [
                        StmtBlock 0 
                        (
                            Print 0 (StringConst 0 "a")
                        ),

                        StmtBlock 0
                        (
                            Break 0                            
                        ),

                        StmtBlock 0
                        (
                            Continue 0
                        ),

                        StmtBlock 0
                        (
                            Return 0 (StringConst 0 "a")
                        ),

                        StmtBlock 0
                        (
                            Return 0 (IntConst 0 1)
                        )

                        

                    ]
                )
                ast
        )

test_declarations = 
    TestCase
        (
            do 
            ast <- (parseProgram "files/testfiles/declarations.txt")
            assertEqual 
                "Test Declarations"
                (
                    Program 
                    [
                        StmtBlock 0 (
                            Decl 0 
                            [
                                (TBool 0, "a", Just $ BoolConst 0 True)
                            ]
                        ),
                        StmtBlock 0 (
                            Decl 0 
                            [
                                (TInt 0, "b", Just $ BinaryOp 0 "+" (IntConst 0 1) (IntConst 0 2))
                            ]
                        ),
                        StmtBlock 0 (
                            Decl 0 [
                                (TFloat 0, "c", Just $ FloatConst 0 3.0),
                                (TFloat 0, "c", Just $ FloatConst 0 3.3)
                            ]
                        ),
                        StmtBlock 0 (
                            Decl 0 [
                                (TString 0, "d", Just $ StringConst 0 "Hello world")
                            ]
                        ),
                        StmtBlock 0 (
                            Decl 0 [
                                (TString 0, "e", Just $ FunCall 0 "main" [IntConst 0 1])
                            ]
                        )
                    ]
                )
                ast 
        )


test_broken = 
    TestCase 
        (
            do
            ast <- (parseProgram "files/testfiles/broken.txt")
            assertEqual
                "Should return empty program"
                (Program [])
                ast
        )

tests = 
    TestList[
        TestLabel "test_simple" test_simple
        , TestLabel "test_fundef" test_fundef
        , TestLabel "test_lambda_py" test_lambda_py
        , TestLabel "test_lambda_hs" test_lambda_hs
        , TestLabel "test_termary" test_termary
        , TestLabel "test_while" test_while
        , TestLabel "test_if_else" test_if_else
        , TestLabel "test_instructions" test_instructions
        , TestLabel "test_declarations" test_declarations
        , TestLabel "test_broken" test_broken
    ]


runHU :: IO ()
runHU = do
    putStr "\n\nTrying HUnit:\n"
    putStr "===============================================\n"
    runTestTT tests
    putStr "===============================================\n\n"
