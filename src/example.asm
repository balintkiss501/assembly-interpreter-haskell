start:
mov     edx, length     ; length of string to print
mov     ecx, message    ; string to print
mov     ebx, 1          ; file descriptor to write (stdout)
mov     eax, 4          ; call number (sys_write)
int     0x80            ; system interrupt: call kernel
jmp     start
