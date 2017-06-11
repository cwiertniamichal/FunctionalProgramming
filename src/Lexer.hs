{-|
Module      : Lexer
Description : Lexer module for C like language
Maintainer  : Robert Bielas, Michal Cwiertnia
-}
module Lexer (languageDef, lexer, identifier, reserved, reservedOp, parens, braces, semi, whiteSpace, naturalOrDouble, stringLiteral) where
import qualified Text.ParserCombinators.Parsec.Token as Token
import Text.ParserCombinators.Parsec.Language
import Text.ParserCombinators.Parsec.Expr
import Text.ParserCombinators.Parsec

-- | This creates the language definition. We used here Parsec's emptyDef constructor.  
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
                                         ":", "?",
                                         "lambda", "|",
                                         "\\", "->"],
                Token.caseSensitive = True
              }         

-- | Lexer that is based on language definition we defined earlier. It contains number of lexical parsers, that we can use to parse identifiers,
-- reserved words/operators, etc.
lexer = Token.makeTokenParser languageDef

-- | Lexical parser used to parse identifiers
identifier = Token.identifier lexer

-- | Lexical parser used to parse reserved words
reserved = Token.reserved lexer

-- | Lexical parser used to parse reserved operators
reservedOp = Token.reservedOp lexer

-- | Lexical parser used to parse surrounding parenthesis
parens = Token.parens lexer

-- | Lexical parser used to parse surrounding braces
braces = Token.braces lexer

-- | Lexical parser used to parse integers and doubles
naturalOrDouble = Token.naturalOrFloat lexer

-- | Lexical parser used to parse semicolons
semi = Token.semi lexer

-- | Lexical parser used to parse whitespaces
whiteSpace = Token.whiteSpace lexer

-- | Lexical parser used to parse string literals
stringLiteral = Token.stringLiteral lexer