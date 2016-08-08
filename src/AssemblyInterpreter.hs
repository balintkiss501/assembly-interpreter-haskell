import System.IO
import Control.Monad
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Expr
import Text.ParserCombinators.Parsec.Language
import qualified Text.ParserCombinators.Parsec.Token as Token

import qualified AssemblyInterpreter.Evaluator as Evaluator
import AssemblyInterpreter.Parser (mainParser, mainFileParser)

{- Usages:
  # Create expression tree
  mainParser "mov ecx, 5"
  mainParser "mov eax, edx"
  mainParser "mov edx, variable"
  mainParser "int 0x80"

  # Create expression tree from file
  mainFileParser "example.asm"

  # Evaluate single instruction
  evaluateExec "mov ecx, 5"

  # Evaluate lines in file
  mainPrint "example.asm"
-}

prompt :: IO ()
prompt = do
    putStr "x86> "
    hFlush stdout
    str <- getLine
    case str of
        "exit"  -> void (putStrLn "Okay, bye.")
        _       -> putStrLn (evaluateExec str)
    prompt

mainPrint filename = do
  file <- readFile filename
  mapM_ (putStrLn . evaluateExec) (lines file)

evaluateExec :: String -> String
evaluateExec str = Evaluator.evaluate2 (mainParser str) Evaluator.emptyCPUState

main :: IO ()
main = prompt
