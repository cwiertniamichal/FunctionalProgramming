module Lexer (languageDef, lexer, identifier, reserved, reservedOp, parens, braces, integer, semi, whiteSpace, naturalOrFloat) where
import qualified Text.ParserCombinators.Parsec.Token as Token
import Text.ParserCombinators.Parsec.Language
import Text.ParserCombinators.Parsec.Expr
import Text.ParserCombinators.Parsec

languageDef = 
    emptyDef { 
               -- comments
               Token.commentStart       = "/*",
               Token.commentEnd         = "*/",
               Token.commentLine        = "//",

               -- identifiers starts with letter and end with aphanumeric characters
               Token.identStart         = lower,
               Token.identLetter        = alphaNum <|> (char '_'),
               
               -- reserved names and operators 
               Token.reservedNames      = [ "if",
                                            "then",
                                            "else",
                                            "while",
                                            "for",
                                            "return",
                                            "do",
                                            "true",
                                            "false"
                                            ],
                Token.reservedOpNames = [
                                         "++", "--",
                                         "+", "-", "*", "/", "%", 
                                         "=",
                                         "<=", "<", ">=", ">", "==", "!=", 
                                         "&&", "||", "!",
                                         ":", "?"],
                Token.caseSensitive = True
              }         

-- create lexer
lexer = Token.makeTokenParser languageDef

-- parse identifier
identifier = Token.identifier lexer

-- parse reserved name
reserved   = Token.reserved   lexer

-- parse operator
reservedOp = Token.reservedOp lexer

-- parse parens
parens     = Token.parens     lexer

-- parse braces
braces = Token.braces lexer

-- parse integer
integer    = Token.integer    lexer

naturalOrFloat = Token.naturalOrFloat lexer

-- parse semicolon
semi       = Token.semi       lexer

-- parse whitespace
whiteSpace = Token.whiteSpace lexer