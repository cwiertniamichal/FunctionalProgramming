module AST (Argument(..), Expr(..), Type(..), FunDef(..), Stmt(..), Block(..)) where

indent = 0
-- TODO: HOW TO PRINT AST???
-- | Data struvture for function's arguments
data Argument = Argument Type String deriving(Show, Eq)

-- instance Show Argument where
--     show (Argument ttype id) = (show ttype) ++ " " ++ id 

-- | Data structure for expressions
data Expr = Condition Expr Expr Expr    -- ^ This expression looks like: 
           | BoolConst Bool             -- ^ Expression representing Boolean constant. Arguments: boolean value.
           | StringConst String         -- ^ Expression representing String constant. Arguments: string value.
           | IntConst Integer           -- ^ Expression representing Integer constant. Arguments: integer value.
           | FloatConst Double          -- ^ Expression representing Double constatnt. Arguments: double value. 
           | UnaryOp String Expr        -- ^ Expression representing unary operation. Arguments: operator, expression.
           | BinaryOp String Expr Expr  -- ^ Expression representing binary operation. Arguments: operator, left expression, right expression. 
           | Not Expr                   -- ^ Expression representing.
           | Neg Expr                   -- ^ Expression representing negation. 
           | FunCall String [Expr]      -- ^ Expression representing function call. Arguments: function's name, list of expression that will be passed as arguments.
           | Variable String
           deriving(Show, Eq)

-- instance Show Expr where
--     show (BoolConst val) = (show val) ++ "\n"
--     show (IntConst val) = (show val) ++ "\n"
--     show (FloatConst val) = (show val) ++ "\n"
--     show (StringConst val) = (show val) ++ "\n"
--     show (UnaryOp op val) = op ++ "\n" ++
--         ("| " ++ (show val) ++ "\n")
--     show (BinaryOp op val1 val2) = op ++ "\n" ++
--         ("| " ++ (show val1)) ++
--         ("| " ++ (show val2))
--     show (FunCall id args) = id ++ "\n" ++
--         case args of
--             (a:[]) -> ("| " ++ (show a))
--             (a:as) -> ("| " ++ (show a) ++ "\n" ++ (show as))
--     -- show (Condition e1 e2 e3) =   

-- | Data structure for types
data Type =  TInt     -- ^ int type
           | TFloat   -- ^ float type
           | TBool    -- ^ bool type
           | TString  -- ^ string type
           | TVoid    -- ^ void type
          deriving(Show, Eq)
-- instance Show Type where
--     show TInt = "int"
--     show TFloat = "float"
--     show TBool = "bool"
--     show TString = "string"
--     show TVoid = "void"

-- | Data structure for function definition 
data FunDef = FunDef Type String [Argument] Stmt            -- ^ Arguments for this constructor: function's type, function's name, list of arguments,
                                                            -- function's body   
            deriving(Show, Eq)
-- instance Show FunDef where
--     show (FunDef ttype name args body) = 
--         "\nFunDef \n" ++ 
--         ((replicate indent '|') ++ name ++ "\n") ++ 
--         ((replicate indent '|') ++ " RET " ++ (show $ ttype) ++ "\n") ++ 
--         ((replicate indent '|') ++ " ARG " ++ (show $ args) ++ "\n") ++
--         ((show $ body))  

-- | Data structure for statements
data Stmt = Seq [Stmt]                          -- ^ Sequence of statements
          | Assign String Expr                  -- ^ Assignment statement
          | If Expr Stmt Stmt                   -- ^ If statement. Arguments: 
          | While Expr Stmt                     -- ^ While statement. Arguments: 
          | Return Expr                         -- ^ Return statement. Arguments: expression which result will be returned
          | Decl [(Type, String, Maybe Expr)]   -- ^ Declaration statement. Arguments: list of tuples which represents declarations. Single declaration 
                                                -- includes: type, identifier, expression or nothing
          | SNop                                -- ^ SNop represents empty statement
          | Print Expr
          | Break
          | Continue
          | SExpr Expr
          deriving (Show, Eq)

-- instance Show Stmt where
--     show (Seq (x:xs)) = (show x) ++ (show xs)  
--     show (Assign id expr) = id ++ (show $ expr) 
--     show (If expr stmt1 stmt2) = "IF \n" ++
--         ("| " ++ (show expr) ++ "\n") ++
--         ("| " ++ (show stmt1)) ++ case stmt2 of
--             SNop -> ""
--             _ -> "\n" ++ (show stmt2) ++ "\n"
--     show (While expr stmt) = "While \n" ++
--         ("| " ++ (show expr) ++ "\n") ++
--         ("| " ++ (show stmt) ++ "\n")
--     show (Return expr) = "Return \n" ++
--         ("| " ++ (show expr) ++ "\n")
--     show (Print expr)  = "Print \n" ++
--         ("| " ++ (show expr) ++ "\n")
--     show Break = "Break \n"
--     show Continue = "Continue \n"
--     show SNop = ""   

data Block = FunDefBlock FunDef 
           | StmtBlock Stmt
           deriving(Show, Eq)