{-# LANGUAGE FlexibleInstances #-}
module SpecQC (runQC) where

import Test.QuickCheck
import System.Random

import CParser(parseProgram, parseString)
import AST
import Control.Monad

import Data.Char (toLower)

------------------------------------------------------------------------
genSafeChar :: Gen Char
genSafeChar = elements ['a'..'z']

genSafeString :: Int -> Gen String 
genSafeString x = 
  sized $
    \n -> do
      k <- choose (1, x)
      sequence  [ genSafeChar | _ <- [1..k] ]   

pickUnaryOp :: Gen String 
pickUnaryOp = elements ["-", "!"]

pickBinaryOp :: Gen String 
pickBinaryOp = elements["*", "/", "%", "+", "-", ">=", "<=", "<", ">", "==", "!=", "||", "&&"]

instance Arbitrary LambdaArg where 
    arbitrary = liftM LambdaArg (genSafeString 1)

instance Arbitrary Argument where
    arbitrary = liftM3 Argument arbitrary arbitrary (genSafeString 2)

instance Arbitrary Expr where 
    arbitrary = oneof [
            liftM2 StringConst arbitrary (genSafeString 7),
            liftM2 IntConst arbitrary arbitrary,
            liftM2 FloatConst arbitrary arbitrary,
            liftM2 Variable arbitrary (genSafeString 3)
            ,liftM4 Condition arbitrary arbitrary arbitrary arbitrary
            ,liftM2 BoolConst arbitrary arbitrary
            --,liftM4 BinaryOp arbitrary pickBinaryOp arbitrary arbitrary
            --,liftM3 UnaryOp arbitrary pickUnaryOp arbitrary
            --,liftM3 FunCall arbitrary (genSafeString 3) arbitrary
            --,liftM3 PythonLambda arbitrary arbitrary arbitrary
            --,liftM3 HaskellLambda arbitrary arbitrary arbitrary

        ]

instance Arbitrary Type where 
    arbitrary = oneof [
            liftM TInt arbitrary,
            liftM TFloat arbitrary,
            liftM TBool arbitrary,
            liftM TString arbitrary,
            liftM TVoid arbitrary 
        ]

instance Arbitrary FunDef where 
    arbitrary = liftM5 FunDef arbitrary arbitrary (genSafeString 3) arbitrary arbitrary 

instance Arbitrary Stmt where 
    arbitrary = oneof
            [
                liftM3 Assign arbitrary (genSafeString 3) arbitrary,
                liftM4 If arbitrary arbitrary arbitrary arbitrary,
                liftM3 While arbitrary arbitrary arbitrary,
                liftM2 Return arbitrary arbitrary,
                return SNop,
                liftM2 Print arbitrary arbitrary,
                liftM Break arbitrary,
                liftM Continue arbitrary,
                liftM2 SExpr arbitrary arbitrary
                --,liftM2 Decl arbitrary arbitrary
                --,liftM2 Seq arbitrary arbitrary
            ]

instance Arbitrary Block where 
    arbitrary = 
        oneof [
            liftM2 StmtBlock arbitrary arbitrary
            --,liftM2 FunDefBlock arbitrary arbitrary
        ]

instance Arbitrary Program where
    --arbitrary = liftM Program arbitrary
    arbitrary = do 
        sb <- arbitrary 
        return $ Program [ sb ]
------------------------------------------------------------------------
class PrettyPrintable a where
    prettyPrint :: a -> String

instance PrettyPrintable LambdaArg where 
    prettyPrint (LambdaArg name) = name

instance PrettyPrintable Argument where
    prettyPrint (Argument _ t name) = prettyPrint t ++ " " ++ name   

instance PrettyPrintable [Expr] where 
    prettyPrint [] = ""
    prettyPrint (e:es) | es == [] = prettyPrint e 
    prettyPrint (e:es) = prettyPrint e ++ ", " ++ prettyPrint es

instance PrettyPrintable [LambdaArg] where
    prettyPrint [] = "" 
    prettyPrint (a:as) | as == [] = prettyPrint a 
    prettyPrint (a:as) = prettyPrint a ++ ", " ++ prettyPrint as

instance PrettyPrintable Expr where 
    prettyPrint (Condition _ e y n) = prettyPrint e ++ " ? " ++ prettyPrint y ++ " : " ++ prettyPrint n
    prettyPrint (BoolConst _ val) = map toLower $ show val 
    prettyPrint (StringConst _ val) = '\"': val ++ "\""
    prettyPrint (IntConst _ val) = show val
    prettyPrint (FloatConst _ val) = show val
    prettyPrint (UnaryOp _ op e) = op ++ " " ++ prettyPrint e
    prettyPrint (BinaryOp _ op e1 e2) = prettyPrint e1 ++ " " ++ op ++ " " ++ prettyPrint e2
    prettyPrint (FunCall _ name el) = name ++ "(" ++ prettyPrint el ++ ")"
    prettyPrint (Variable _ name) = name
    prettyPrint (PythonLambda _ la e) = "lambda " ++ prettyPrint la ++ ":" ++ prettyPrint e
    prettyPrint (HaskellLambda _ la e) = "\\" ++ prettyPrint la ++ " -> " ++ prettyPrint e 

instance PrettyPrintable Type where 
    prettyPrint (TInt _ ) = "int"
    prettyPrint (TFloat _) = "float"
    prettyPrint (TBool _) = "bool"
    prettyPrint (TString _) = "string"
    prettyPrint (TVoid _) = "void"

instance PrettyPrintable [Argument] where 
    prettyPrint [] = ""
    prettyPrint (a:as) | as == [] = prettyPrint a 
    prettyPrint (a:as) = prettyPrint a ++ ", " ++ prettyPrint as

instance PrettyPrintable FunDef where 
    prettyPrint (FunDef _ t name args stm) = prettyPrint t ++ " " ++ name ++ "(" ++ prettyPrint args ++ "){\n" ++ prettyPrint stm ++ "\n}"

instance PrettyPrintable [Stmt] where 
    prettyPrint [] = ""
    prettyPrint (s:ss) = prettyPrint s ++ prettyPrint ss

instance PrettyPrintable [(Type, String, Maybe Expr)] where 
    prettyPrint [] = ""
    prettyPrint ((t, name, e) : ts) = case e of 
        Just val -> prettyPrint t ++ " " ++ name ++ " " ++ prettyPrint val ++ ";\n" ++ prettyPrint ts 
        Nothing -> prettyPrint t ++ " " ++ name ++ ";\n" ++ prettyPrint ts

instance PrettyPrintable Stmt where 
    prettyPrint (Seq _ stms) = "{" ++ prettyPrint stms ++ "}"
    prettyPrint (Assign _ name e) = name ++ " = " ++ prettyPrint e ++ ";\n"
    prettyPrint (If _ e if_e else_e) = "if(" ++ prettyPrint e ++ ")\n" ++ prettyPrint if_e ++ "else\n" ++ prettyPrint else_e
    prettyPrint (While _ e s) = "while(" ++ prettyPrint e ++ ")\n" ++ prettyPrint s
    prettyPrint (Return _ e) = "return " ++ prettyPrint e ++ "\n;"
    prettyPrint (Decl _ dl) = prettyPrint dl 
    prettyPrint SNop = ";"
    prettyPrint (Print _ expr) = "print " ++ prettyPrint expr ++ ";\n"
    prettyPrint (Break _) = "break;\n"
    prettyPrint (Continue _) = "continue;\n"
    prettyPrint (SExpr _ e) = prettyPrint e ++ ";\n"


instance PrettyPrintable Program where 
    prettyPrint (Program blocks) = prettyPrint blocks

instance PrettyPrintable [Block] where 
    prettyPrint [] = ""
    prettyPrint (b:bs) = prettyPrint b ++ prettyPrint bs 

instance PrettyPrintable Block where 
    prettyPrint (StmtBlock _ stmt) = prettyPrint stmt
    prettyPrint (FunDefBlock _ stmt) = prettyPrint stmt
------------------------------------------------------------------------

-- Only one general property: generate random pieces of code, mount them together and check if the result can be parsed
prop_nested :: Program -> Bool
prop_nested x = case parseString $ prettyPrint x of 
    Left e -> False 
    Right _ -> True

print_generated_program = do 
    program <- generate $ arbitrary :: IO Program
    putStr $ prettyPrint program
    let res = parseString $ prettyPrint program
    putStr $ show res

runQC :: IO ()
runQC = do
    putStr "\n\nTrying QuickCheck:\n"
    putStr "===============================================\n"
    quickCheck prop_nested
    putStr "===============================================\n\n\n"
