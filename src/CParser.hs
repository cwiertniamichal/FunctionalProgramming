module CParser(parseProgram) where
import System.IO
import Control.Monad
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Expr
import Text.ParserCombinators.Parsec.Language
import Lexer 
import AST

{-|
== /Description:/
Function parsing condition expression.

== /Arguments:/
- expression  
-}
parseCondition ex indent = do
  reservedOp "?"
  a <- (parseExpr indent)
  reservedOp ":"
  b <- (parseExpr indent)
  return $ Condition indent ex a b

{-|
== /Description:/
Function parsing single expression.

== /Arguments:/
- parser   
-}
parseExpr :: Int -> Parser Expr
parseExpr indent = do
  ex <- buildExpressionParser (exprOps indent) (exprTerms indent) <?> "expression"
  try (parseCondition ex indent) <|> return ex

{-|
== /Description:/
List of operators and their assocs
-}
exprOps indent = [
  [Prefix (reservedOp "-" >> return (UnaryOp indent "-"))],

  [Prefix (reservedOp "!" >> return (UnaryOp indent "!"))],

  [Infix (reservedOp "*" >> return (BinaryOp indent "*")) AssocLeft],
  [Infix (reservedOp "/" >> return (BinaryOp indent "/")) AssocLeft],
  [Infix (reservedOp "%" >> return (BinaryOp indent "%")) AssocLeft],

  [Infix (reservedOp "+" >> return (BinaryOp indent "+")) AssocLeft],
  [Infix (reservedOp "-" >> return (BinaryOp indent "-")) AssocLeft],

  [Infix (reservedOp ">=" >> return (BinaryOp indent ">=")) AssocLeft],
  [Infix (reservedOp "<=" >> return (BinaryOp indent "<=")) AssocLeft],
  [Infix (reservedOp "<" >> return (BinaryOp indent "<")) AssocLeft],
  [Infix (reservedOp ">" >> return (BinaryOp indent ">")) AssocLeft],

  [Infix (reservedOp "==" >> return (BinaryOp indent "==")) AssocLeft],
  [Infix (reservedOp "!=" >> return (BinaryOp indent "!=")) AssocLeft],

  [Infix (reservedOp "||" >> return (BinaryOp indent "||")) AssocLeft],
  [Infix (reservedOp "&&" >> return (BinaryOp indent "&&")) AssocLeft]
  ]

{-|
== /Description:/
 
-}
exprTerms indent = parens (parseExpr indent)
  <|> (do
    try (do 
      id <- identifier
      args <- (parens $ sepBy (parseExpr $ indent) (reservedOp ","))
      return $ FunCall indent id args
      )    
    )
  <|>(do
        value <- stringLiteral
        return $ StringConst indent value
    )
  <|> (do
        x <- naturalOrFloat
        case x of
          Left v -> return $ IntConst indent v
          Right v -> return $ FloatConst indent v
      )
  <|> (reserved "true" >> return (BoolConst indent True))
  <|> (reserved "false" >> return (BoolConst indent False))
  <|> (identifier >>= \name ->  return (Variable indent name)) 

{-|
== /Description:/
Function parsing declaration statement.

== /Arguments:/
- parser   
-}
parseDecl :: Int -> Parser Stmt
parseDecl indent = do
  dtype <- (parseType $ indent + 1)
  list <- sepBy(
    do
      try (do 
        name <- identifier
        reservedOp "="
        expr <- (parseExpr $ 0)
        return (dtype, name, Just expr)
        )
      <|> (do
        name <- identifier
        return (dtype, name, Nothing)
        )
      ) (reservedOp ",")
  semi
  return $ Decl indent list
 
{-|
== /Description:/
Function parsing assign statement.

== /Arguments:/
- parser   
-}
parseAssignStmt :: Int -> Parser Stmt
parseAssignStmt indent = do 
  try (do
    var <- identifier
    reservedOp "="
    expr <- (parseExpr $ indent + 1)
    semi
    return $ Assign indent var expr
    )

{-|
== /Description:/
Function parsing return statement.

== /Arguments:/
- parser   
-}
parseReturnStmt :: Int -> Parser Stmt
parseReturnStmt indent = do
  reserved "return"
  expr <- (parseExpr $ indent + 1)
  semi
  return $ Return indent expr

{-|
== /Description:/
Function parsing if statement.

== /Arguments:/
- parser   
-}
parseIfStmt :: Int -> Parser Stmt
parseIfStmt indent = do 
  reserved "if"
  cond  <- (parens $ parseExpr $ indent + 1)
  stmt1 <- (parseStatement $ indent + 1)
  stmt2 <- (try (reserved "else" >> (parseStatement $ indent + 1)) <|> (do return SNop))
  return $ If indent cond stmt1 stmt2
 
{-|
== /Description:/
Function parsing while statement.

== /Arguments:/
- parser   
-}
parseWhileStmt :: Int -> Parser Stmt
parseWhileStmt indent = do 
  reserved "while"
  cond <- (parens $ parseExpr $ indent + 1)
  stmt <- (parseStatement $ indent + 1)
  return $ While indent cond stmt

{-|
== /Description:/
Function parsing print statement.

== /Arguments:/
- parser   
-}
parsePrintStmt :: Int -> Parser Stmt
parsePrintStmt indent = do
  reserved "print"
  expr <- parseExpr $ indent + 1
  semi
  return $ Print indent expr 
 
parseBreakStmt :: Int ->  Parser Stmt
parseBreakStmt indent = do
  reserved "break"
  semi
  return $ Break indent

parseContinueStmt :: Int -> Parser Stmt
parseContinueStmt indent = do
    reserved "continue"
    semi
    return $ Continue indent

parseSingleExpr :: Int -> Parser Stmt
parseSingleExpr indent = do
  try (do
    expr <- (parseExpr indent)
    semi
    return $ SExpr indent expr
    )

parseEmptyStmt :: Parser Stmt
parseEmptyStmt = do
  semi
  return SNop
{-|
== /Description:/
Function parsing single statement.

== /Arguments:/
- parser   
-}
parseStatement :: Int -> Parser Stmt
parseStatement indent = parseBreakStmt indent
  <|> parseContinueStmt indent 
  <|> parsePrintStmt indent
  <|> parseIfStmt indent
  <|> parseWhileStmt indent
  <|> parseReturnStmt indent
  <|> parseAssignStmt indent
  <|> parseSingleExpr indent
  <|> parseDecl indent
  <|> parseSequenceOfStmt indent
  <|> parseEmptyStmt 
{-|
== /Description:/
Function parsing sequence of statements.

== /Arguments:/   
-}
parseSequenceOfStmt indent = braces $ do
  list <- (many $ parseStatement indent)
  return $ if length list == 0 then SNop else Seq indent list
{-|
== /Description:/
Function parsing function's arguments.

== /Arguments:/
- parser   
-}
parseFunArgs :: Int -> Parser [Argument]
parseFunArgs indent = do
  sepBy (do
    atype <- (parseType indent)
    name <- identifier
    return $ Argument indent atype name
    ) (reservedOp ",")

{-|
== /Description:/
Function parsing types.

== /Arguments:/   
-}
parseType :: Int -> Parser Type
parseType indent = do 
  x <- try (do
    cs <- many $ identLetter languageDef
    return cs 
    <?> "identifier" 
    )
  whiteSpace
  case x of
    "int" -> return $ TInt indent
    "float" -> return $ TFloat indent 
    "bool"  -> return $ TBool indent 
    "string"-> return $ TString indent 
    "void"  -> return $ TVoid indent 
    _  -> fail "Wrong type" 


{-|
== /Description:/
Function parsing function definitions.

== /Arguments:/
- parser   
-}

{-
    Example function definition:
    void foo1(int a, float b){

    }
-}
parseFun :: Int -> Parser FunDef
parseFun indent = do 
  ftype <- parseType $ indent + 1
  name <- identifier
  args <- (parens $ parseFunArgs $ indent + 1)
  body <- (do
    body <- (parseSequenceOfStmt $ indent + 1)
    return body
    )

  return $ FunDef indent ftype name args body

parseBlock :: Int -> Parser Block
parseBlock indent = do
  try (do 
    fun <- parseFun indent
    return $ FunDefBlock indent fun  
    )
  <|> (do
    stmt <- parseStatement indent
    return $ StmtBlock indent stmt
    )
      
{-|
== /Description:/
Function that starts actual parsing. It gets rid of preceding whitespaces.

== /Arguments:/
- parser   
-}
parser :: Parser Program
-- we have to deal with spaces before program's source code
parser = do 
          whiteSpace
          blocks <- many $ parseBlock 0
          return $ Program blocks

{-| 
== /Description:/
Function loading program's source code from file.

== /Arguments:/ 
- path to a file.
-} 

parseProgram :: String -> IO Program
parseProgram file =
  do program  <- readFile file
     case parse parser "CParser" program of
       Left e  -> print e >> fail "Parse error"
       Right r -> return r 