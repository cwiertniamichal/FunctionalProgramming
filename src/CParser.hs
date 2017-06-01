module CParser(parseProgram) where
import System.IO
import Control.Monad
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Expr
import Text.ParserCombinators.Parsec.Language
import Lexer 
import AST

-- TODO main porgram can be only set of functions now

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
    try (
        do 
          id <- identifier
          list <- (parens (sepBy (parseExpr) (reservedOp ",")))
          return $ FunCall id list
        )    
      )
  <|> liftM StringConst stringLiteral
  <|> (do
        x <- naturalOrFloat
        case x of
          Left v -> return $ IntConst v
          Right v -> return $ FloatConst v
      )
  <|> (reserved "true" >> return (BoolConst True))
  <|> (reserved "false" >> return (BoolConst False))
  <|> (identifier >>= \name ->  return (Variable name)) 
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
              reservedOp "="
              expr <- parseExpr
              return (dtype, name, Just expr)
          )
        <|> (do
            name <- identifier
            return (dtype, name, Nothing)
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
  try ( do
     var <- identifier
     reservedOp "="
     expr <- parseExpr
     semi
     return $ Assign var expr
     )

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
Function parsing print statement.

== /Arguments:/
- parser   
-}
parsePrintStmt :: Parser Stmt
parsePrintStmt = do
    reserved "print"
    expr <- parseExpr
    semi
    return $ Print expr 
 
parseBreakStmt :: Parser Stmt
parseBreakStmt = do
    reserved "break"
    semi
    return $ Break

parseContinueStmt :: Parser Stmt
parseContinueStmt = do
    reserved "continue"
    semi
    return $ Continue

parseSingleExpr = do
  try ( do
    expr <- parseExpr
    --id <- identifier
    --list <- (parens (sepBy (parseExpr) (reservedOp ",")))
    semi
    return $ SExpr $ expr
    )

parseEmptyStmt = do
  semi
  return SNop
{-|
== /Description:/
Function parsing single statement.

== /Arguments:/
- parser   
-}
parseStatement :: Parser Stmt
parseStatement = parseBreakStmt
           <|> parseContinueStmt 
           <|> parsePrintStmt
           <|> parseIfStmt
           <|> parseWhileStmt
           <|> parseReturnStmt
           <|> parseAssignStmt
           <|> parseSingleExpr
           <|> parseDecl 
           <|> parseSequenceOfStmt
           <|> parseEmptyStmt
{-|
== /Description:/
Function parsing sequence of statements.

== /Arguments:/   
-}
parseSequenceOfStmt = braces $ do
       list <- (many parseStatement)
       return $ if length list == 0 then SNop else Seq list

{-|
== /Description:/
Function parsing function's arguments.

== /Arguments:/
- parser   
-}
parseFunArgs :: Parser [Argument]
parseFunArgs = do
  sepBy (do
      atype <- parseType
      name <- identifier
      return $ Argument atype name
    )
    (reservedOp ",")

{-|
== /Description:/
Function parsing types.

== /Arguments:/   
-}
parseType = do 
      x <- try (do
        cs <- many (identLetter languageDef)
        return (cs) 
        <?> "identifier" 
               )
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

{-
    Example function definition:
    void foo1(int a, float b){

    }
-}
parseFun :: Parser FunDef
parseFun = do 
  ftype <- parseType
  name <- identifier
  args <- (parens parseFunArgs)
  (body) <- (do
              body <- parseSequenceOfStmt
              return (body)
            )

  return $ FunDef ftype name args body

parseBlock = do
        try  (
            do 
              fun <- parseFun
              return $ FunDefBlock fun 
          )
        <|> (do
            stmt <- parseStatement
            return $ StmtBlock stmt
          )
      
{-|
== /Description:/
Function that starts actual parsing. It gets rid of preceding whitespaces.

== /Arguments:/
- parser   
-}
parser :: Parser [Block]
-- we have to deal with spaces before program's source code
parser = do 
          whiteSpace
          many parseBlock

-- parseString :: String -> [FunDef]
-- parseString str =
--   case parse parser "CParser" str of
--     Left e  -> error $ show e
--     Right r -> r

{-| 
== /Description:/
Function loading program's source code from file.

== /Arguments:/ 
- path to a file.
-} 

parseProgram :: String -> IO [Block]
parseProgram file =
  do program  <- readFile file
     case parse parser "CParser" program of
       Left e  -> print e >> fail "Parse error"
       Right r -> return r 