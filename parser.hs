module CParser where
import System.IO
import Control.Monad
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Expr
import Text.ParserCombinators.Parsec.Language
import Lexer 

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
           deriving(Show)

-- | Data structure for types
data Type = TAuto     -- ^ auto type
           | TInt     -- ^ int type
           | TFloat   -- ^ float type
           | TBool    -- ^ bool type
           | TString  -- ^ string type
           | TVoid    -- ^ void type
           deriving(Show)

-- | Data structure for function definition 
data FunDef = FunDef Type String Type [(Type, String)] Stmt -- ^ Arguments for this constructor: function's type, function's name, list of arguments,
                                                            -- function's body   
            deriving(Show)

-- | Data structure for statements
data Stmt = Seq [Stmt]                          -- ^ Sequence of statements
          | Assign String Expr                  -- ^ Assignment statement
          | If Expr Stmt Stmt                   -- ^ If statement. Arguments: 
          | While Expr Stmt                     -- ^ While statement. Arguments: 
          | Return Expr                         -- ^ Return statement. Arguments: expression which result will be returned
          | Decl [(Type, String, Maybe Expr)]   -- ^ Declaration statement. Arguments: list of tuples which represents declarations. Single declaration 
                                                -- includes: type, identifier, expression or nothing
          | SNop                                -- ^ SNop represents empty statement
          deriving (Show)

{-|
== /Description:/
Function parsing condition expression.

== /Arguments:/
- expression  
-}
parseCondition ex = do
  reservedOp "?"
  a <- parseExpr
  reservedOp ":"
  b <- parseExpr
  return $ Condition ex a b

{-|
== /Description:/
Function parsing single expression.

== /Arguments:/
- parser   
-}
parseExpr :: Parser Expr
parseExpr = do
  ex <- buildExpressionParser exprOps exprTerms <?> "expression"
  try (parseCondition ex) <|> return ex

{-|
== /Description:/
List of operators and their assocs
-}
exprOps = [
  [Prefix (reservedOp "-" >> return (UnaryOp "-"))],

  [Prefix (reservedOp "!" >> return (UnaryOp "!"))],

  [Infix (reservedOp "*" >> return (BinaryOp "*")) AssocLeft],
  [Infix (reservedOp "/" >> return (BinaryOp "/")) AssocLeft],
  [Infix (reservedOp "%" >> return (BinaryOp "%")) AssocLeft],

  [Infix (reservedOp "+" >> return (BinaryOp "+")) AssocLeft],
  [Infix (reservedOp "-" >> return (BinaryOp "-")) AssocLeft],

  [Infix (reservedOp ">=" >> return (BinaryOp ">=")) AssocLeft],
  [Infix (reservedOp "<=" >> return (BinaryOp "<=")) AssocLeft],
  [Infix (reservedOp "<" >> return (BinaryOp "<")) AssocLeft],
  [Infix (reservedOp ">" >> return (BinaryOp ">")) AssocLeft],

  [Infix (reservedOp "==" >> return (BinaryOp "==")) AssocLeft],
  [Infix (reservedOp "!=" >> return (BinaryOp "!=")) AssocLeft],

  [Infix (reservedOp "||" >> return (BinaryOp "||")) AssocLeft],
  [Infix (reservedOp "&&" >> return (BinaryOp "&&")) AssocLeft]

          ]

{-|
== /Description:/
 
-}
exprTerms = parens parseExpr
  <|> (do
        id <- identifier
        try ( do
            list <- (parens $ sepBy parseExpr $ reservedOp ",")
            return $ FunCall id list
            )
      )
  <|> liftM StringConst identifier
  <|> (do
        x <- naturalOrFloat
        case x of
          Left v -> return $ IntConst v
          Right v -> return $ FloatConst v
      )
  <|> (reserved "true" >> return (BoolConst True))
  <|> (reserved "false" >> return (BoolConst False))

parseArray ttype = do
  return ttype

{-|
== /Description:/
Function parsing declaration statement.

== /Arguments:/
- parser   
-}
parseDecl :: Parser Stmt
parseDecl = do
  dtype <- parseType
  list <- sepBy(
      do
        try  (
            do 
              name <- identifier
              ttype <- parseArray dtype
              reservedOp "="
              expr <- parseExpr
              return (ttype, name, Just expr)
          )
        <|> (do
            name <- identifier
            ttype <- parseArray dtype
            return (ttype, name, Nothing)
          )
      )
      (reservedOp ",")
  semi
  return $ Decl list
 
{-|
== /Description:/
Function parsing assign statement.

== /Arguments:/
- parser   
-}
parseAssignStmt :: Parser Stmt
parseAssignStmt = do 
     var  <- identifier
     reservedOp "="
     expr <- parseExpr
     semi
     return $ Assign var expr

{-|
== /Description:/
Function parsing return statement.

== /Arguments:/
- parser   
-}
parseReturnStmt :: Parser Stmt
parseReturnStmt = do
  reserved "return"
  expr <- parseExpr
  semi
  return $ Return expr

{-|
== /Description:/
Function parsing if statement.

== /Arguments:/
- parser   
-}
parseIfStmt :: Parser Stmt
parseIfStmt = do 
     reserved "if"
     cond  <- (parens parseExpr)
     stmt1 <- parseStatement
     stmt2 <- (try (reserved "else" >> parseStatement) <|> (do return SNop))
     return $ If cond stmt1 stmt2
 
{-|
== /Description:/
Function parsing while statement.

== /Arguments:/
- parser   
-}
parseWhileStmt :: Parser Stmt
parseWhileStmt = do 
     reserved "while"
     cond <- (parens parseExpr)
     stmt <- parseStatement
     return $ While cond stmt

{-|
== /Description:/
Function parsing single statement.

== /Arguments:/
- parser   
-}
parseStatement :: Parser Stmt
parseStatement = parseIfStmt
           <|> parseWhileStmt
           <|> parseReturnStmt
           <|> sequenceOfStmt
           <|> parseDecl
           <|> parseAssignStmt

{-|
== /Description:/
Function parsing sequence of statements.

== /Arguments:/   
-}
sequenceOfStmt = braces $ do
       list <- (many parseStatement)
       return $ if length list == 0 then SNop else Seq list

{-|
== /Description:/
Function parsing function's arguments.

== /Arguments:/
- parser   
-}
parseFunArgs :: Parser [(Type, String)]
parseFunArgs = do
  sepBy (do
      atype <- parseType
      name <- identifier
      return (atype, name)
    )
    (reservedOp ",")

{-|
== /Description:/
Function parsing types.

== /Arguments:/   
-}
parseType = do 
    x <- try ( do
       c <- lower 
       cs <- many (identLetter languageDef)
       return (c:cs) 
       <?> "identifier" )
    whiteSpace
    case x of
      "int" -> return TInt
      "float" -> return TFloat
      "bool"  -> return TBool
      "string"-> return TString
      "void"  -> return TVoid
      _  -> fail "Wrong type" 


{-|
== /Description:/
Function parsing function definitions.

== /Arguments:/
- parser   
-}
parseFun :: Parser FunDef
parseFun = do 
  ftype <- parseType
  name <- identifier
  args <- (parens parseFunArgs)
  (rtype, stmt) <- (do
                      stmt <- sequenceOfStmt
                      return (TAuto, stmt)
    )

  return $ FunDef ftype name rtype args stmt

{-|
== /Description:/
Function that starts actual parsing. It gets rid of preceding whitespaces.

== /Arguments:/
- parser   
-}
parser :: Parser [FunDef]
-- we have to deal with spaces before program's source code
parser = whiteSpace >> many parseFun

parseString :: String -> [FunDef]
parseString str =
  case parse parser "CParser" str of
    Left e  -> error $ show e
    Right r -> r

{-| 
== /Description:/
Function loading program's source code from file.

== /Arguments:/ 
- path to a file.
-} 
parseProgram :: String -> IO [FunDef]
parseProgram file =
  do program  <- readFile file
     case parse parser "CParser" program of
       Left e  -> print e >> fail "Parse error"
       Right r -> return r