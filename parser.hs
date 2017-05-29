module CParser where
import System.IO
import Control.Monad
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Expr
import Text.ParserCombinators.Parsec.Language
import Lexer 

data Expr = Condition Expr Expr Expr
           | BoolConst Bool
           | StringConst String
           | IntConst Integer
           | FloatConst Double
           | UnaryOp String Expr
           | BinaryOp String Expr Expr 
           | Not Expr 
           | Neg Expr 
           | FunCall String [Expr] 
           deriving(Show)

data Type = TAuto
           | TInt
           | TFloat
           | TBool
           | TString
           | TVoid 
           deriving(Show)

data FunDef = FunDef Type String Type [(Type, String)] Stmt 
            deriving(Show)

-- Statements
data Stmt = Seq [Stmt]
          | Assign String Expr
          | If Expr Stmt Stmt
          | While Expr Stmt
          | Return Expr
          | Decl [(Type, String, Maybe Expr)]
          | SNop
          deriving (Show)

parseCondition ex = do
  reservedOp "?"
  a <- parseExpr
  reservedOp ":"
  b <- parseExpr
  return $ Condition ex a b

parseExpr :: Parser Expr
parseExpr = do
  ex <- buildExpressionParser exprOps exprTerms <?> "expression"
  try (parseCondition ex) <|> return ex

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
 
parseAssignStmt :: Parser Stmt
parseAssignStmt = do 
     var  <- identifier
     reservedOp "="
     expr <- parseExpr
     semi
     return $ Assign var expr

parseReturnStmt :: Parser Stmt
parseReturnStmt = do
  reserved "return"
  expr <- parseExpr
  semi
  return $ Return expr

parseIfStmt :: Parser Stmt
parseIfStmt = do 
     reserved "if"
     cond  <- (parens parseExpr)
     stmt1 <- parseStatement
     stmt2 <- (try (reserved "else" >> parseStatement) <|> (do return SNop))
     return $ If cond stmt1 stmt2
 
parseWhileStmt :: Parser Stmt
parseWhileStmt = do 
     reserved "while"
     cond <- (parens parseExpr)
     stmt <- parseStatement
     return $ While cond stmt

parseStatement :: Parser Stmt
parseStatement = parseIfStmt
           <|> parseWhileStmt
           <|> parseReturnStmt
           <|> sequenceOfStmt
           <|> parseDecl
           <|> parseAssignStmt

sequenceOfStmt = braces $ do
       list <- (many parseStatement)
       return $ if length list == 0 then SNop else Seq list

parseFunArgs :: Parser [(Type, String)]
parseFunArgs = do
  sepBy (do
      atype <- parseType
      name <- identifier
      return (atype, name)
    )
    (reservedOp ",")

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

parser :: Parser [FunDef]
-- we have to deal with spaces before program's source code
parser = whiteSpace >> many parseFun

parseString :: String -> [FunDef]
parseString str =
  case parse parser "CParser" str of
    Left e  -> error $ show e
    Right r -> r
 
parseProgram :: String -> IO [FunDef]
parseProgram file =
  do program  <- readFile file
     case parse parser "CParser" program of
       Left e  -> print e >> fail "Parse error"
       Right r -> return r