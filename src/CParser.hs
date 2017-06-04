{-|
Module      : CParser
Description : parser module for C like language
Maintainer  : Robert Bielas, Michal Cwiertnia
-}
module CParser(parseProgram, parseString) where
import System.IO
import Control.Monad
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Expr
import Text.ParserCombinators.Parsec.Language
import Lexer 
import AST


{-| 
== /Description:/
This function loads program from file and pass it to parse function. Parse is Parsec's function. It returns
Either ParseError Program in our case. If there was an error during parsing program, the return value of parse function
will be Left with the error, otherwise it will be Right with the result.  

== /Arguments:/ 
- path to a file with program's source code.

== /Return:/
If there was an error during parsing program, the return value  error,
 otherwise it will be Right with the result.
-} 
parseProgram :: String -> IO Program
parseProgram file =
  do program  <- readFile file
     case parse parser "CParser" program of
       Left e  -> print e >> return (Program [])-- >> fail "Parse error"
       Right r -> return r 

{-|
== /Description:/
This function allows pass program as string.   
-}
parseString programContent = parse parser "Cparser" programContent

      
{-|
== /Description:/
This function parses program. We define program as something that consists of many blocks.
This function also gets rid of preceding spaces.   

== /Return:/

-}
parser :: Parser Program
-- we have to deal with spaces before program's source code
parser = do 
          whiteSpace
          blocks <- many $ parseBlock 0
          return $ Program blocks


{-|
== /Description:/
This function parses single block. We define block to be a function definition or a statement.

== /Arguments:/
- intdent - level of indentation, used during printing AST 
-}
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
This function parses function definition. Function definition consists of: result's type, 
function's name, arguments and function's body.

== /Arguments:/
- intdent - level of indentation, used during printing AST 
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


{-|
== /Description:/
This function parses type. Type is one of the following: int, float, bool, string, void.

== /Arguments:/
- intdent - level of indentation, used during printing AST    
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
This function parses functions's arguments. Single argument consists of type and name. Arguments are separated with a comma. 

== /Arguments:/
- intdent - level of indentation, used during printing AST 
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
This function parses sequence of statements. 

== /Arguments:/
- intdent - level of indentation, used during printing AST 

== /Return:/
If function successfully parsed some statement it returns Seq with list of statements. Otherwise it returns SNop. 
-}
parseSequenceOfStmt indent = braces $ do
  list <- (many $ parseStatement indent)
  return $ if length list == 0 then SNop else Seq indent list


{-|
== /Description:/
This function parses single statement. We define statement as one of the following: break statement, continue statement, print statement,
if-else statement, while statement, return statement, assignment statement, single expression, declaration, sequence of statement, empty
statement.

== /Arguments:/
- intdent - level of indentation, used during printing AST  
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
This function parses break statement. Break statement consists of "break" keyword and semicolon. 

== /Arguments:/
- intdent - level of indentation, used during printing AST  
-}
parseBreakStmt :: Int ->  Parser Stmt
parseBreakStmt indent = do
  reserved "break"
  semi
  return $ Break indent


{-|
== /Description:/
This function parses continue statement. Continue statement consists of "continue" keyword and semicolon. 

== /Arguments:/
- intdent - level of indentation, used during printing AST    
-}
parseContinueStmt :: Int -> Parser Stmt
parseContinueStmt indent = do
    reserved "continue"
    semi
    return $ Continue indent


{-|
== /Description:/
This function parses print statement. Print statement consists of "print" keyword, expression, which result will be printed, and semicolon. 

== /Arguments:/
- intdent - level of indentation, used during printing AST   
-}
parsePrintStmt :: Int -> Parser Stmt
parsePrintStmt indent = do
  reserved "print"
  expr <- parseExpr $ indent + 1
  semi
  return $ Print indent expr 
 

{-|
== /Description:/
This function parses if-else statement. If-else statement consists of "if" keyword, condition, if part body and optional else part.
Condition is just expression. If part body is just another statement. Else part consists of "else" keyword and else part body which is just
another statement.

== /Arguments:/
- intdent - level of indentation, used during printing AST   
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
This function parses while statement. While statement consists of "while" keyword, condition and body. Condition is just expression.
Body is just another statement.

== /Arguments:/
- intdent - level of indentation, used during printing AST    
-}
parseWhileStmt :: Int -> Parser Stmt
parseWhileStmt indent = do 
  reserved "while"
  cond <- (parens $ parseExpr $ indent + 1)
  stmt <- (parseStatement $ indent + 1)
  return $ While indent cond stmt


{-|
== /Description:/
This function parses return statement. Return statement consists of "return" keyword, expression, which result will be returned, and
semicolon.

== /Arguments:/
- intdent - level of indentation, used during printing AST    
-}
parseReturnStmt :: Int -> Parser Stmt
parseReturnStmt indent = do
  reserved "return"
  expr <- (parseExpr $ indent + 1)
  semi
  return $ Return indent expr


{-|
== /Description:/
This function parses assignment statement. Assignment statement consists of variable's name, to which we want assign value, "=" operator,
expression, which result will be assign to variable and semicolon. 

== /Arguments:/
- intdent - level of indentation, used during printing AST   
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
This function parses single expression. Single expression consists of expression and semicolon.

== /Arguments:/
- intdent - level of indentation, used during printing AST   
-}
parseSingleExpr :: Int -> Parser Stmt
parseSingleExpr indent = do
  try (do
    expr <- (parseExpr indent)
    semi
    return $ SExpr indent expr
    )

{-|
== /Description:/
This function parses declarations. Declarations are separated with comma. Single declaration has two forms:
1) type, variable's name, "=" operator, expression, which result will be value of variable, and semicolon,
2) just type, variable's name and semicolon,

== /Arguments:/
- intdent - level of indentation, used during printing AST   
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
This function parses empty statement. Empty statement is just semicolon.
-}
parseEmptyStmt :: Parser Stmt
parseEmptyStmt = do
  semi
  return SNop


{-|
== /Description:/
This function parses python lambda. Python lambda consists of "lambda" keyword, arguments, ":" operator and expression.

== /Arguments:/
- intdent - level of indentation, used during printing AST   
-}
parsePythonLambda :: Int -> Parser Expr 
parsePythonLambda indent = do 
  reservedOp "lambda"
  args <- parseLambdaArgs
  reservedOp ":"
  expr <- (parseExpr (indent + 1))
  return $ PythonLambda indent args expr


{-|
== /Description:/
This function parses haskell lambda. Haskell lambda consists of "\" operator, arguments, "->" operator and expression.

== /Arguments:/
- intdent - level of indentation, used during printing AST   
-}
parseHaskellLambda :: Int -> Parser Expr 
parseHaskellLambda indent = do 
  reservedOp "\\"
  args <- parseLambdaArgs
  reservedOp "->"
  expr <- (parseExpr (indent + 1))
  return $ HaskellLambda indent args expr


{-|
== /Description:/
This function parses expression. We used here Parsec's buildExpressionParser which takes two parameters: table and terms.
It builds an expression parser for terms with operators from table, taking the associativity and precedence specified in table into account.

== /Arguments:/
- intdent - level of indentation, used during printing AST   
-}
parseExpr :: Int -> Parser Expr
parseExpr indent = do
  try (parsePythonLambda indent) <|> 
    try (parseHaskellLambda indent) <|>
      do
        ex <- buildExpressionParser (exprOps indent) (exprTerms indent) <?> "expression"
        try (parseCondition ex indent) <|> return ex


parseLambdaArgs :: Parser [LambdaArg]
parseLambdaArgs = do 
  sepBy (
      do 
        name <- identifier
        return $ LambdaArg name
    )
    (reservedOp ",")


{-|
== /Description:/
This function parses condition expression. Condition expression consists of "?" operator, expression, "-:" operator and another expression.

== /Arguments:/
- ex - it is an expression that was already parsed. We are using it here to check if it's a part of condition expression.
- intdent - level of indentation, used during printing AST   
-}
parseCondition ex indent = do
  reservedOp "?"
  a <- (parseExpr indent)
  reservedOp ":"
  b <- (parseExpr indent)
  return $ Condition indent ex a b


{-|
== /Description:/
List with operator precedence, associativity and what constructors to use in each case. In case of Prefix operators it is enough
to specify which one should be parsed and what is the associated data constructor. Infix operators are defined similarly, but it's 
necessary to add information about associativity. Operator precedence depends only on the order of the elements in the list.
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
