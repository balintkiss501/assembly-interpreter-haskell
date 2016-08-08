module AssemblyInterpreter.Parser
( mainParser
, mainFileParser
) where

import System.IO
import Control.Monad
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Expr
import Text.ParserCombinators.Parsec.Language
import qualified Text.ParserCombinators.Parsec.Token as Token

import qualified AssemblyInterpreter.Grammar as Grammar
import qualified AssemblyInterpreter.Lexer as Lexer

-- EXPRESSION PARSER --
-- Line parser
programSeqParser :: Parser Grammar.ProgramLine
programSeqParser = programParser
                <|> progSequence

programParser :: Parser Grammar.ProgramLine
programParser    = (instruction >>= \ins -> return (Grammar.InstructionLine ins))
                <|> (labelname >>= \l -> Lexer.symbol ":" >> return (Grammar.LabelLine l))

progSequence = do
    ls <- sepBy1 programParser (char '\n')
    return (if length ls == 1 then head ls else Grammar.Seq ls)

-- Instruction parsing
instruction :: Parser Grammar.Instruction
instruction =  (Lexer.reserved "mov" >> Lexer.whiteSpace >> registername >>= \r -> Lexer.comma >> parameter >>= \p -> return (Grammar.InstructionMov r p))
           <|> (Lexer.reserved "add" >> Lexer.whiteSpace >> registername >>= \r -> Lexer.comma >> parameter >>= \p -> return (Grammar.InstructionAdd r p))
           <|> (Lexer.reserved "inc" >> Lexer.whiteSpace >> registername >>= \r -> return (Grammar.InstructionInc r ))
           <|> (Lexer.reserved "jmp" >> Lexer.whiteSpace >> labelname >>= \l -> return (Grammar.InstructionJmp l))
           <|> (Lexer.reserved "int" >> Lexer.whiteSpace >> Lexer.symbol "0" >> interruptcode >>= \i -> return (Grammar.InstructionInt i))

-- Parse primitive components and terms
labelname :: Parser Grammar.Label
labelname = Lexer.identifier >>= \l -> return (Grammar.Lab l)

parameter :: Parser Grammar.Parameter
parameter =  (registername >>= \r -> return (Grammar.ParamReg r))
         <|> (constant >>= \c -> return (Grammar.ParamConst c))
         <|> (variable >>= \v -> return (Grammar.ParamVar v))

registername :: Parser Grammar.RegisterName
registername = (Lexer.reserved "eax" >> return Grammar.EAX)
            <|> (Lexer.reserved "ebx" >> return Grammar.EBX)
            <|> (Lexer.reserved "ecx" >> return Grammar.ECX)
            <|> (Lexer.reserved "edx" >> return Grammar.EDX)

variable :: Parser Grammar.Variable
variable = Lexer.identifier >>= \v -> return (Grammar.Var v)

constant :: Parser Grammar.Constant
constant = Lexer.integer >>= \c -> return (Grammar.Const c)

interruptcode :: Parser Grammar.InterruptCode
interruptcode = Lexer.hexadecimal >>= \i -> return (Grammar.IntCode i)

-- EXECUTING PARSER --
mainParser :: String -> Grammar.ProgramLine
mainParser str =
    case tryparse of
        Left err -> error (show err)
        Right result -> result
    where
        tryparse :: Either ParseError Grammar.ProgramLine   -- with this, you don't have to extract from Either
        tryparse = parse programSeqParser "" str

mainFileParser :: String -> IO [Grammar.ProgramLine]
mainFileParser filename = do
    f <- readFile filename
    return $ map mainParser (lines f)
