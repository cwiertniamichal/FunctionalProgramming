module AST (Argument(..), Expr(..), Type(..), FunDef(..), Stmt(..), Block(..), Program(..), LambdaArg(..)) where

data LambdaArg = LambdaArg String deriving (Show, Eq)

-- | Data struvture for function's arguments
data Argument = Argument Int Type String 

-- | Data structure for expressions
data Expr = Condition Int Expr Expr Expr |    -- ^ This expression looks like: 
            BoolConst Int Bool |             -- ^ Expression representing Boolean constant. Arguments: boolean value.
            StringConst Int String |         -- ^ Expression representing String constant. Arguments: string value.
            IntConst Int Integer |           -- ^ Expression representing Integer constant. Arguments: integer value.
            FloatConst Int Double |          -- ^ Expression representing Double constatnt. Arguments: double value. 
            UnaryOp Int String Expr |        -- ^ Expression representing unary operation. Arguments: operator, expression.
            BinaryOp Int String Expr Expr |  -- ^ Expression representing binary operation. Arguments: operator, left expression, right expression. 
            FunCall Int String [Expr] |      -- ^ Expression representing function call. Arguments: function's name, list of expression that will be passed as arguments.
            Variable Int String |
            PythonLambda Int [LambdaArg] Expr |
            HaskellLambda Int [LambdaArg] Expr 

-- | Data structure for types
data Type = TInt Int |    -- ^ int type
            TFloat Int |  -- ^ float type
            TBool Int |   -- ^ bool type
            TString Int | -- ^ string type
            TVoid Int   -- ^ void type

-- | Data structure for function definition 
data FunDef = FunDef Int Type String [Argument] Stmt            -- ^ Arguments for this constructor: function's type, function's name, list of arguments,
                                                            -- function's body   


-- | Data structure for statements
data Stmt = Seq Int [Stmt] |                          -- ^ Sequence of statements
            Assign Int String Expr |                  -- ^ Assignment statement
            If Int Expr Stmt Stmt |                  -- ^ If statement. Arguments: 
            While Int Expr Stmt |                     -- ^ While statement. Arguments: 
            Return Int Expr |                         -- ^ Return statement. Arguments: expression which result will be returned
            Decl Int [(Type, String, Maybe Expr)] |   -- ^ Declaration statement. Arguments: list of tuples which represents declarations. Single declaration 
                                                -- includes: type, identifier, expression or nothing
            SNop |                                -- ^ SNop represents empty statement
            Print Int Expr |
            Break Int |
            Continue Int |
            SExpr Int Expr

data Block = FunDefBlock Int FunDef | 
             StmtBlock Int Stmt


data Program = Program [Block] 

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