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
```

TODOs
- make it more modular
- convert tokenizers to lexemes
- implement variable defining (DB, section .data)
- implement section .text (instructions) and .data (variables)
- EQU directive
- pass CPU state
