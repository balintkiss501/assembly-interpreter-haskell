module AssemblyInterpreter.Grammar
( ProgramLine(..)
, Instruction(..)
, Parameter(..)
, RegisterName(..)
, Variable(..)
, Constant(..)
, Label(..)
, InterruptCode(..)
) where

-- BNF GRAMMAR --
data ProgramLine    = Seq [ProgramLine]
                    | InstructionLine Instruction
                    | LabelLine Label
                    deriving (Show)

data Instruction    = InstructionMov RegisterName Parameter
                    | InstructionAdd RegisterName Parameter
                    | InstructionInc RegisterName
                    | InstructionJmp Label
                    | InstructionInt InterruptCode
                    deriving (Show)

data Parameter      = ParamReg RegisterName
                    | ParamVar Variable
                    | ParamConst Constant deriving (Show)

data RegisterName   = EAX   -- Primary accumulator
                    | EBX   -- Base register
                    | ECX   -- Count register
                    | EDX   -- Data register
                    deriving (Show)

data Variable       = Var String deriving (Show)
data Constant       = Const Integer deriving (Show)
data Label          = Lab String deriving (Show)
data InterruptCode  = IntCode Integer deriving (Eq, Show)
