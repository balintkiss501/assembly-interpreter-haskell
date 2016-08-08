A simple Intel x86 Assembly interpreter written in Haskell using Parsec.

Usage:
```haskell
  -- Create expression tree
  mainParser "mov ecx, 5"
  mainParser "mov eax, edx"
  mainParser "mov edx, variable"
  mainParser "int 0x80"

  -- Create expression tree from file
  mainFileParser "example.asm"

  -- Evaluate single instruction
  evaluateExec "mov ecx, 5"

  -- Evaluate lines in file
  mainPrint "example.asm"

  -- Opens up a prompt to write assembly commands in it
  propmt
```
