{-|
Module      : Lexer
Description : Lexer module for C like language
Maintainer  : Michal Cwiertnia

-}
module Lexer (languageDef, lexer, identifier, reserved, reservedOp, parens, braces, integer, semi, whiteSpace, naturalOrFloat, stringLiteral) where
import qualified Text.ParserCombinators.Parsec.Token as Token
import Text.ParserCombinators.Parsec.Language
import Text.ParserCombinators.Parsec.Expr
import Text.ParserCombinators.Parsec

-- | Language's tokens 
languageDef = 
    emptyDef { 
               -- comments
               Token.commentStart       = "/*",
               Token.commentEnd         = "*/",
               Token.commentLine        = "//",

               -- identifiers starts with letter in lowercase and end with aphanumeric characters or _
               Token.identStart         = lower,
               Token.identLetter        = alphaNum <|> (char '_'),
               
               -- reserved names and operators 
               Token.reservedNames      = [ "if",
                                            "then",
                                            "else",
                                            "while",
                                            "for",
                                            "return",
                                            "print",
                                            "true",
                                            "false",
                                            "break",
                                            "continue"
                                            ],
                Token.reservedOpNames = [
                                         "+", "-", "*", "/", "%", 
                                         "=",
                                         "<=", "<", ">=", ">", "==", "!=", 
                                         "&&", "||", "!",
                                         ":", "?"],
                Token.caseSensitive = True
              }         

-- | Lexer
lexer = Token.makeTokenParser languageDef

-- | Identifier
identifier = Token.identifier lexer

-- | Reserved names
reserved   = Token.reserved   lexer

-- | Reserved operators
reservedOp = Token.reservedOp lexer

-- | Parenthesis - ()
parens     = Token.parens     lexer

-- | Braces - {}
braces = Token.braces lexer

-- | Integers
integer    = Token.integer    lexer

-- | Integers or Doubles
naturalOrFloat = Token.naturalOrFloat lexer

-- | Semicolon
semi       = Token.semi       lexer

-- | WhiteSpace
whiteSpace = Token.whiteSpace lexer

stringLiteral = Token.stringLiteral lexer