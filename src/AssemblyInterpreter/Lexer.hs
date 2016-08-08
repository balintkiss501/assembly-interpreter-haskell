module AssemblyInterpreter.Lexer
( identifier
, reserved
, symbol
, integer
, hexadecimal
, comma
, whiteSpace
) where

import System.IO
import Control.Monad
import Data.Functor.Identity
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Expr
import Text.ParserCombinators.Parsec.Language
import qualified Text.ParserCombinators.Parsec.Token as Token

-- LEXER --
-- Language definition
reservedRegisterNames :: [String]
reservedRegisterNames   = [ "eax", "ebx", "ecx", "edx" ]

reservedInstructions :: [String]
reservedInstructions    = [ "mov", "add", "inc", "jmp", "int" ]

def = emptyDef  { Token.commentLine     = ";"
                , Token.identStart      = letter        -- identifiers start with letter
                , Token.identLetter     = alphaNum      -- identifiers end with alphanumeric char
                , Token.reservedNames   = foldr (++) [] [reservedRegisterNames, reservedInstructions]
                }

-- Create lexer from language definition
lexer = Token.makeTokenParser def

-- Extract lexical parsers
identifier  = Token.identifier lexer
reserved    = Token.reserved lexer
symbol      = Token.symbol lexer
integer     = Token.integer lexer
hexadecimal = Token.hexadecimal lexer
comma       = Token.comma lexer
whiteSpace  = Token.whiteSpace lexer
