module AssemblyInterpreter.Evaluator
( emptyCPUState
, evaluate2
) where

import qualified AssemblyInterpreter.Grammar as Grammar

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

-- TODO: rename
evaluate2 :: Grammar.ProgramLine -> CPUState -> String
evaluate2 line cpu = case line of
    (Grammar.InstructionLine (Grammar.InstructionMov reg param))    -> "Move " ++ extractParam param ++ " into register " ++ show reg ++ ". The cpu state is " ++ show (modcpu reg (extractParam param) cpu)
    (Grammar.InstructionLine (Grammar.InstructionJmp label))        -> "Jump to label " ++ extractLabel label ++ "."
    (Grammar.InstructionLine (Grammar.InstructionInt code))         -> debug_exitsystemcall
    (Grammar.LabelLine label)                                       -> "Label line. It has the label " ++ extractLabel label
    _                                                               -> "This is an unknown line."
    where
        extractParam :: Grammar.Parameter -> String
        extractParam (Grammar.ParamConst (Grammar.Const c))     = show c
        extractParam (Grammar.ParamVar (Grammar.Var v))         = show v
        extractParam (Grammar.ParamReg r)                       = show r
        extractLabel (Grammar.Lab l)                            = show l
        modcpu :: Grammar.RegisterName -> String -> CPUState -> CPUState
        modcpu reg p cp = case reg of
            Grammar.EAX -> cp { eax = p }
            Grammar.EBX -> cp { ebx = p }
            Grammar.ECX -> cp { ecx = p }
            Grammar.EDX -> cp { edx = p }
        debug_hellosystemcall   = interrupt (Grammar.IntCode 0x80) $ modcpu Grammar.EAX "4" $ modcpu Grammar.ECX "Hello World!" cpu
        debug_exitsystemcall    = interrupt (Grammar.IntCode 0x80) $ modcpu Grammar.EAX "1" cpu


interrupt :: Grammar.InterruptCode -> CPUState -> String
interrupt (Grammar.IntCode code) cpu
    | code == 0x80 && eax cpu == "4"      = ecx cpu      -- TODO: handle message length (edx), file descriptor (ebx)
    | code == 0x80 && eax cpu == "1"      = "System exit call"
    | code == 0x80                        = "This is a system call interrupt, but I have no idea what are you doing."
    | otherwise                           = "This is an unknown interrupt."

evaluate :: Grammar.ProgramLine -> String
evaluate line = case line of
    (Grammar.InstructionLine (Grammar.InstructionMov reg param))    -> "Move " ++ extractParam param ++ " into register " ++ show reg ++ "."
    (Grammar.InstructionLine (Grammar.InstructionJmp label))        -> "Jump to label " ++ extractLabel label ++ "."
    (Grammar.InstructionLine (Grammar.InstructionInt code)) -> case code of
        Grammar.IntCode 0x80    -> "This is a system call interrupt."
        _                       -> "This is an unkown interrupt."
    (Grammar.LabelLine label)                       -> "Label line. It has the label " ++ extractLabel label
    _                                               -> "This is an unknown line."     -- TODO: make it fail
    where
        extractParam :: Grammar.Parameter -> String
        extractParam (Grammar.ParamConst (Grammar.Const c)) = "value " ++ show c
        extractParam (Grammar.ParamVar (Grammar.Var v))     = "variable " ++ show v
        extractParam (Grammar.ParamReg r)                   = "register " ++ show r
        extractLabel (Grammar.Lab l)                        = "label " ++ show l
