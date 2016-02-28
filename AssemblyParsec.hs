import System.IO
import Control.Monad
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Expr
import Text.ParserCombinators.Parsec.Language
import qualified Text.ParserCombinators.Parsec.Token as Token

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

-- BNF GRAMMAR --
data ProgramLine    = Seq [ProgramLine]
                    | InstructionLine Instruction 
                    | LabelLine Label
                    deriving (Show)

data Instruction    = InstructionMov RegisterName Parameter
                    | InstructionAdd RegisterName Parameter
                    | InstructionInc RegisterName
                    | InstructionJmp Label
                    | InstructionInt IntCode
                    deriving (Show)

data Parameter      = ParamReg RegisterName | ParamVar Variable | ParamConst Constant deriving (Show)

data RegisterName   = EAX   -- Primary accumulator
                    | EBX   -- Base register
                    | ECX   -- Count register
                    | EDX   -- Data register
                    deriving (Show)

data Variable       = Var String deriving (Show)
data Constant       = Const Integer deriving (Show)
data Label          = Lab String deriving (Show)
data IntCode        = IntCode Integer deriving (Eq, Show)

-- LEXER --
-- Language definition
reservedRegisterNames   = [ "eax", "ebx", "ecx", "edx" ]
reservedInstructions    = [ "mov", "add", "inc", "jmp", "int" ]
def = emptyDef  { Token.commentLine     = ";"
                , Token.identStart      = letter        -- identifiers start with letter
                , Token.identLetter     = alphaNum      -- identifiers end with alphanumeric char
                , Token.reservedNames   = foldr (++) [] [reservedRegisterNames, reservedInstructions]   -- Clean has flatten. Haskell hasn't.
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

-- EXPRESSION PARSER --
-- Line parser
programSeqParser :: Parser ProgramLine
programSeqParser = programParser
                <|> progSequence

programParser :: Parser ProgramLine
programParser    = (instruction >>= \ins -> return (InstructionLine ins))
                <|> (labelname >>= \l -> symbol ":" >> return (LabelLine l))

progSequence = do
    ls <- (sepBy1 programParser (char '\n'))
    return (if length ls == 1 then head ls else Seq ls)

-- Instruction parsing
instruction :: Parser Instruction
instruction =  (reserved "mov" >> whiteSpace >> registername >>= \r -> comma >> parameter >>= \p -> return (InstructionMov r p)) 
           <|> (reserved "add" >> whiteSpace >> registername >>= \r -> comma >> parameter >>= \p -> return (InstructionAdd r p))
           <|> (reserved "inc" >> whiteSpace >> registername >>= \r -> return (InstructionInc r ))
           <|> (reserved "jmp" >> whiteSpace >> labelname >>= \l -> return (InstructionJmp l))
           <|> (reserved "int" >> whiteSpace >> symbol "0" >> interruptcode >>= \i -> return (InstructionInt i))

-- Parse primitive components and terms
labelname :: Parser Label
labelname = identifier >>= \l -> return (Lab l)

parameter :: Parser Parameter
parameter =  (registername >>= \r -> return (ParamReg r))
         <|> (constant >>= \c -> return (ParamConst c))
         <|> (variable >>= \v -> return (ParamVar v))

registername :: Parser RegisterName
registername = (reserved "eax" >> return (EAX))
            <|> (reserved "ebx" >> return (EBX))
            <|> (reserved "ecx" >> return (ECX))
            <|> (reserved "edx" >> return (EDX))

variable :: Parser Variable
variable = identifier >>= \v -> return (Var v)

constant :: Parser Constant
constant = integer >>= \c -> return (Const c)

interruptcode :: Parser IntCode
interruptcode = hexadecimal >>= \i -> return (IntCode i)

-- EXECUTING PARSER --
mainParser :: String -> ProgramLine
mainParser str =
    case tryparse of
        Left err -> error (show err)
        Right result -> result
    where
        tryparse :: Either ParseError ProgramLine   -- with this, you don't have to extract from Either
        tryparse = parse programSeqParser "" str

mainFileParser :: String -> IO [ProgramLine]
mainFileParser filename = do
    f <- readFile filename
    return $ map mainParser (lines f)

prompt :: IO ()
prompt = do
    putStr "x86> "
    hFlush stdout
    str <- getLine
    case str of
        "exit"  -> putStrLn "Okay, bye." >> return ()
        _       -> putStrLn (evaluateExec str)
    prompt

main :: IO ()
main = do
    prompt

mainPrint filename = do
  file <- readFile filename
  mapM_ putStrLn (map evaluateExec (lines file))

-- EVALUATE EXPRESSION TREE --
data CPUState = CPUState { eax   :: String
                         , ebx   :: String
                         , ecx   :: String
                         , edx   :: String
                         } deriving (Eq, Show)

emptyCPUState :: CPUState
emptyCPUState = CPUState
    { eax = ""
    , ebx = ""
    , ecx = ""
    , edx = ""
    }

evaluateExec :: String -> String
evaluateExec str = evaluate2 (mainParser str) emptyCPUState

-- TODO: rename
evaluate2 :: ProgramLine -> CPUState -> String
evaluate2 line cpu = case line of
    (InstructionLine (InstructionMov reg param))    -> "Move " ++ extractParam param ++ " into register " ++ show reg ++ ". The cpu state is " ++ show (modcpu reg (extractParam param) cpu)
    (InstructionLine (InstructionJmp label))        -> "Jump to label " ++ extractLabel label ++ "."
    (InstructionLine (InstructionInt code))         -> debug_exitsystemcall 
    (LabelLine label)                               -> "Label line. It has the label " ++ extractLabel label
    _                                               -> "This is an unknown line."
    where
        extractParam :: Parameter -> String
        extractParam (ParamConst (Const c))     = show c
        extractParam (ParamVar (Var v))         = show v
        extractParam (ParamReg r)               = show r
        extractLabel (Lab l)                    = show l
        modcpu :: RegisterName -> String -> CPUState -> CPUState
        modcpu reg p cp = case reg of
            EAX -> cp { eax = p }
            EBX -> cp { ebx = p }
            ECX -> cp { ecx = p }
            EDX -> cp { edx = p }
        debug_hellosystemcall   = interrupt (IntCode 0x80) $ modcpu EAX "4" $ modcpu ECX "Hello World!" cpu
        debug_exitsystemcall    = interrupt (IntCode 0x80) $ modcpu EAX "1" $ cpu


interrupt :: IntCode -> CPUState -> String
interrupt (IntCode code) cpu
    | code == 0x80 && (eax cpu) == "4"      = (ecx cpu)      -- TODO: handle message length (edx), file descriptor (ebx)
    | code == 0x80 && (eax cpu) == "1"      = "System exit call"
    | code == 0x80                          = "This is a system call interrupt, but I have no idea what are you doing."
    | otherwise                             = "This is an unknown interrupt."

evaluate :: ProgramLine -> String
evaluate line = case line of
    (InstructionLine (InstructionMov reg param))    -> "Move " ++ extractParam param ++ " into register " ++ show reg ++ "."
    (InstructionLine (InstructionJmp label))        -> "Jump to label " ++ extractLabel label ++ "."
    (InstructionLine (InstructionInt code)) -> case code of
        IntCode 0x80    -> "This is a system call interrupt."
        _               -> "This is an unkown interrupt."
    (LabelLine label)                               -> "Label line. It has the label " ++ extractLabel label
    _                                               -> "This is an unknown line."     -- TODO: make it fail
    where
        extractParam :: Parameter -> String
        extractParam (ParamConst (Const c)) = "value " ++ show c
        extractParam (ParamVar (Var v))     = "variable " ++ show v
        extractParam (ParamReg r)           = "register " ++ show r
        extractLabel (Lab l)                = "label " ++ show l
