{-|
Module      : AST
Description : This module includes definitions of data structures and implementations of theirs Eq and Show instances. Each value constructor has Int field that represents level of indentation. It is used during printing AST tree.
Maintainer  : Robert Bielas, Michal Cwiertnia
-}
module AST (Argument(..), Expr(..), Type(..), FunDef(..), Stmt(..), Block(..), Program(..), LambdaArg(..)) where

-- | Data structure for Program. It helps in printing pretty AST.
data Program = Program [Block] -- ^ Arguments: List of blocks that forms program.


-- | Data structure for Block. Block is a part of program. Block can be a function definition or a statement.
data Block = FunDefBlock Int FunDef |  -- ^ Block consisting of function definition. Arguments: function definition.
             StmtBlock Int Stmt        -- ^ Block consisting of statement. Argumetns: statement.



-- | Data structure for function definition. Function definition can be a part of block.
data FunDef = FunDef Int Type String [Argument] Stmt        -- ^ Arguments for this constructor: function's type, function's name, list of arguments,
                                                            -- function's body . 

-- | Data structure for statements. Statement can be part of a block or a function definition
data Stmt = Seq Int [Stmt] |                          -- ^ Sequence of statements. Arguments: a list of statements.
            Assign Int String Expr |                  -- ^ Assignment statement. Arguments: variable's name, expression, which result will be a value of variable.
            If Int Expr Stmt Stmt |                   -- ^ If-else statement. Arguments: condition, if body, else body.
            While Int Expr Stmt |                     -- ^ While statement. Arguments: condition, body.
            Return Int Expr |                         -- ^ Return statement. Arguments: expression which result will be returned.
            Decl Int [(Type, String, Maybe Expr)] |   -- ^ Declaration statement. Arguments: list of tuples which represents declarations. Single declaration 
                                                      -- have two forms:
                                                      -- 1) type, variable's name and expression, which result is wrapped into Just,
                                                      -- 2) type, variable's name and Nothing. 
            SNop |                                    -- ^ Empty statement.
            Print Int Expr |                          -- ^ Print statement. Arguments: expression, which result will be printed.
            Break Int |                               -- ^ Break statement. 
            Continue Int |                            -- ^ Continue statement.
            SExpr Int Expr                            -- ^ Single expression statement. Arguments: expression.


-- | Data structure for lambda's arguments. 
data LambdaArg = LambdaArg String -- ^ Arguments for this constructor: argument's name.
               deriving (Show, Eq) 

-- | Data structure for function's arguments.
data Argument = Argument Int Type String  -- ^ Arguments for this constructor: argument's type, argument's name

-- | Data structure for expressions. Expressions can be a part of statements.
data Expr = Condition Int Expr Expr Expr |      -- ^ Condition expression. Arguments: condition, expression that result will be returned if condtion was satisfied,
                                                -- expression that result will be returned if condtion wasn't satisfied
            BoolConst Int Bool |                -- ^ Boolean constant. Arguments: boolean value.
            StringConst Int String |            -- ^ String constant. Arguments: string value.
            IntConst Int Integer |              -- ^ Integer constant. Arguments: integer value.
            FloatConst Int Double |             -- ^ Double constatnt. Arguments: double value. 
            UnaryOp Int String Expr |           -- ^ Unary operation. Arguments: operator, expression.
            BinaryOp Int String Expr Expr |     -- ^ Binary operation. Arguments: operator, left expression, right expression. 
            FunCall Int String [Expr] |         -- ^ Function call. Arguments: function's name, list of expressions which results will be arguments of function call.
            Variable Int String |               -- ^ Variable. Arguments: variable's name.
            PythonLambda Int [LambdaArg] Expr | -- ^ Lambda in python style. Arguments: list of lambda's arguments, lambda's body.
            HaskellLambda Int [LambdaArg] Expr  -- ^ Lambda in haskell style. Arguments: list of lambda's arguments, lambda's body.

-- | Data structure for types
data Type = TInt Int |    -- ^ int type
            TFloat Int |  -- ^ float type
            TBool Int |   -- ^ bool type
            TString Int | -- ^ string type
            TVoid Int     -- ^ void type


-------------------------SHOW HELPER FUNS--------------------------------------------------

changeIndent expr = case expr of
  BoolConst indent val -> BoolConst (indent + 1)  val
  IntConst indent val -> IntConst (indent + 1) val
  FloatConst indent val -> FloatConst (indent + 1) val
  StringConst indent val -> StringConst (indent + 1) val
  UnaryOp indent op val -> UnaryOp (indent + 1) op (changeIndent val) 
  BinaryOp indent op val1 val2 -> BinaryOp (indent + 1) op (changeIndent val1) (changeIndent val2)
  FunCall indent id args -> FunCall (indent + 1) id (changeArgsIndent args)
    where changeArgsIndent [] = []
          changeArgsIndent (a:args) = changeIndent a : changeArgsIndent args
  Variable indent name -> Variable (indent + 1) name
  Condition indent e1 e2 e3 -> Condition (indent + 1) (changeIndent e1) (changeIndent e2) (changeIndent e3)


printLambdaArgs _ [] = ""
printLambdaArgs indent (a:as) = 
  replicate indent '|' ++ show a ++ "\n" ++ printLambdaArgs indent as


----------------------SHOW INSTANCES---------------------------------------------------------
instance Show Program where
  show (Program bs) = printBlocks bs
    where printBlocks [] = ""
          printBlocks (b:bs) = show b ++ printBlocks bs 

instance Show Block where
  show (FunDefBlock _ funDef ) = show funDef
  show (StmtBlock _ stmt ) = show stmt

instance Show Stmt where
  show (Seq indent stmts) = printStmt stmts 
    where printStmt [] = ""
          printStmt (s:stmts) = show s ++ printStmt stmts  
  
  show (Assign indent id expr) = 
    replicate indent '|' ++ "=\n" ++
    replicate (indent + 1) '|' ++ id ++ "\n" ++
    show expr
  
  show (If indent expr stmt1 stmt2) = 
    replicate indent '|' ++ "IF \n" ++
    show expr ++
    show stmt1 ++ 
    case stmt2 of
      SNop -> ""
      _ -> replicate indent '|' ++ "ELSE \n" ++ show stmt2
  
  show (While indent expr stmt ) = 
    replicate indent '|' ++ "While \n" ++
    show expr ++ 
    show stmt

  show (Return indent expr) = 
    replicate indent '|' ++ "Return \n" ++ show expr

  show (Print indent expr)  = 
    replicate indent '|' ++ "Print \n" ++ show expr

  show (Break indent) = 
    replicate indent '|' ++ "Break \n"

  show (Continue indent) = 
    replicate indent '|' ++ "Continue \n"

  show SNop = ""   
  
  show (SExpr indent expr) = show expr

  show (Decl indent dcls) = printDecls dcls 
    where printDecls [] = ""
          printDecls ((dtype, name, expr):dcls) =  
                    replicate indent '|' ++ "DECL \n" ++ 
                    replicate (indent + 1) '|' ++ "=\n" ++
                    replicate (indent + 2) '|' ++ name ++ "\n" ++
                    replicate (indent + 2) '|' ++ 
                    case expr of
                      (Just x) -> show x    
                      _        -> "Nothing\n"
                    ++ printDecls dcls 

instance Show FunDef where
  show (FunDef indent ftype name args body) = 
    "FunDef \n" ++ replicate (indent + 1) '|' ++ name ++ "\n" ++
    replicate (indent + 1) '|' ++ "RET " ++ show ftype ++ 
    printArgs args ++
    show body
    where printArgs [] = ""
          printArgs (a:args) = show a ++ printArgs args 

instance Show Expr where
  show (PythonLambda indent lambda_args expr) = 
    replicate indent '|'
     ++ "Python lambda\n" 
     ++ printLambdaArgs (indent + 2) lambda_args
     ++ show expr

  show (HaskellLambda indent lambda_args expr) = 
    replicate indent '|'
     ++ "Haskell lambda\n" 
     ++ printLambdaArgs (indent + 2) lambda_args
     ++ show expr

  show (BoolConst indent val) = replicate indent '|' ++ show val ++ "\n"
  
  show (IntConst indent val) = replicate indent '|' ++ show val ++ "\n"
  
  show (FloatConst indent val) = replicate indent '|' ++ show val ++ "\n"
  
  show (StringConst indent val) = replicate indent '|' ++ show val ++ "\n"
  
  show (UnaryOp indent op val) = replicate indent '|' ++ op ++ "\n" ++ show (changeIndent val) ++ "\n"
  
  show (BinaryOp indent op val1 val2) = replicate indent '|' ++ op ++ "\n" ++
    show (changeIndent val1) ++ 
    show (changeIndent val2)
  
  show (FunCall indent id args) = replicate indent '|' ++ "FUNCALL\n" ++  
    replicate (indent + 1) '|' ++ id ++ "\n" ++
    printArgs args 
    where printArgs [] = ""
          printArgs (a:args) = show (changeIndent a) ++ printArgs args
  
  show (Variable indent name) = replicate indent '|' ++ name ++ "\n"
  
  show (Condition indent e1 e2 e3) = replicate indent '|' ++ "?\n" ++
    show (changeIndent e1) ++
    replicate indent '|' ++ ":\n" ++
    show (changeIndent e2) ++
    show (changeIndent e3)

instance Show Argument where
  show (Argument indent atype name) = replicate indent '|' ++
    "ARG " ++ name ++ "\n" 

instance Show Type where
    show (TInt indent) = "int\n" -- (replicate indent '|') ++ "int\n"
    show (TFloat indent) = "float\n" -- (replicate indent '|') ++ "float\n"
    show (TBool indent) = "bool\n" -- (replicate indent '|') ++ "bool\n"
    show (TString indent) = "string\n" -- (replicate indent '|') ++ "string\n"
    show (TVoid indent) = "void\n" -- (replicate indent '|') ++ "void\n"

----------------------EQ INSTANCES------------------------------------------------------------

instance Eq Program where
    Program l1 == Program l2 = l1 == l2

instance Eq Expr where 
  (Condition _ e1 e2 e3) == (Condition _ e1' e2' e3') = 
      e1 == e1' && e2 == e2' && e3 == e3'
  (BoolConst _ b1) == (BoolConst _ b2) = b1 == b2
  (StringConst _ s1) == (StringConst _ s2) = s1 == s2
  (IntConst _ i1) == (IntConst _ i2) = i1 == i2
  (FloatConst _ f1) == (FloatConst _ f2) = f1 == f2
  (UnaryOp _ s1 e1) == (UnaryOp _ s2 e2) = s1 == s2 && e1 == e2
  (BinaryOp _ s e1 e2) == (BinaryOp _ s' e1' e2') = s == s' && e1 == e1' && e2 == e2'
  (FunCall _ s1 l1) == (FunCall _ s2 l2) = s1 == s2 && l1 == l2
  (Variable _ s1) == (Variable _ s2) = s1 == s2
  (PythonLambda _ l1 e1) == (PythonLambda _ l2 e2) = l1 == l2 && e1 == e2
  (HaskellLambda _ l1 e1) == (HaskellLambda _ l2 e2) = l1 == l2 && e1 == e2

instance Eq Type where 
  (TInt _) == (TInt _) = True
  (TFloat _) == (TFloat _) = True
  (TString _) == (TString _) = True
  (TBool _) == (TBool _) = True
  (TVoid _) == (TVoid _) = True

instance Eq FunDef where 
  (FunDef _ t1 s1 l1 st1) == (FunDef _ t2 s2 l2 st2) =
    t1 == t2 && s1 == s2 && l1 == l2 && st1 == st2

instance Eq Stmt where 
  (Seq _ st1) == (Seq _ st2) = st1 == st2
  (Assign _ s1 e1) == (Assign _ s2 e2) = 
    s1 == s2 && e1 == e2
  (If _ e stm1 stm2) == (If _ e' stm1' stm2') = 
    e == e' && stm1 == stm1' && stm2 == stm2'
  (While _ e1 stm1) == (While _ e2 stm2) =
    e1 == e2 && stm1 == stm2
  (Return _ e1) == (Return _ e2) = e1 == e2
  (Decl _ l1) == (Decl _ l2) = l1 == l2 
  (Print _ e1) == (Print _ e2) = e1 == e2
  (Break _) == (Break _) = True
  (Continue _) == (Continue _ ) = True
  (SExpr _ e1) == (SExpr _ e2) = e1 == e2
  (SNop) == (SNop) = True

instance Eq Block where
  (FunDefBlock _ f1) == (FunDefBlock _ f2) = f1 == f2
  (StmtBlock _ stm1) == (StmtBlock _ stm2) = stm1 == stm2

instance Eq Argument where 
  (Argument _ t1 s1) == (Argument _ t2 s2) = 
    t1 == t2 && s1 == s2 