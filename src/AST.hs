module AST (Argument(..), Expr(..), Type(..), FunDef(..), Stmt(..), Block(..), Program(..), LambdaArg(..)) where

data LambdaArg = LambdaArg String deriving (Show, Eq)

-- | Data struvture for function's arguments
data Argument = Argument Int Type String 
                deriving(Eq)

instance Show Argument where
  show (Argument indent atype name) = replicate indent '|' ++
    "ARG " ++ name ++ "\n" 


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
            deriving(Eq)

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


-- | Data structure for types
data Type = TInt Int |    -- ^ int type
            TFloat Int |  -- ^ float type
            TBool Int |   -- ^ bool type
            TString Int | -- ^ string type
            TVoid Int   -- ^ void type
            deriving(Eq)

instance Show Type where
    show (TInt indent) = "int\n" -- (replicate indent '|') ++ "int\n"
    show (TFloat indent) = "float\n" -- (replicate indent '|') ++ "float\n"
    show (TBool indent) = "bool\n" -- (replicate indent '|') ++ "bool\n"
    show (TString indent) = "string\n" -- (replicate indent '|') ++ "string\n"
    show (TVoid indent) = "void\n" -- (replicate indent '|') ++ "void\n"


-- | Data structure for function definition 
data FunDef = FunDef Int Type String [Argument] Stmt            -- ^ Arguments for this constructor: function's type, function's name, list of arguments,
                                                            -- function's body   
              deriving(Eq)

instance Show FunDef where
  show (FunDef indent ftype name args body) = 
    "FunDef \n" ++ replicate (indent + 1) '|' ++ name ++ "\n" ++
    replicate (indent + 1) '|' ++ "RET " ++ show ftype ++ 
    printArgs args ++
    show body
    where printArgs [] = ""
          printArgs (a:args) = show a ++ printArgs args    


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
            deriving (Eq)

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


data Block = FunDefBlock Int FunDef | 
             StmtBlock Int Stmt
             deriving(Eq)

instance Show Block where
  show (FunDefBlock _ funDef ) = show funDef
  show (StmtBlock _ stmt ) = show stmt


data Program = Program [Block]

instance Show Program where
  show (Program bs) = printBlocks bs
    where printBlocks [] = ""
          printBlocks (b:bs) = show b ++ printBlocks bs 