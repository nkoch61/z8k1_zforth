; zf_defs.inc

RETURN_STACK_SIZE       equ     1024
PARA_STACK_SIZE         equ     512

RETURN_STACK_TOP        =       0fffeh
RETURN_STACK_BOTTOM     =       RETURN_STACK_TOP-RETURN_STACK_SIZE
PARA_STACK_TOP          =       RETURN_STACK_BOTTOM
PARA_STACK_BOTTOM       =       PARA_STACK_TOP-PARA_STACK_SIZE

F_IMMED                 equ     80h
F_HIDDEN                equ     20h
F_LENMASK               equ     1fh

IMMEDIATE_MODE          equ     0
COMPILE_MODE            equ     1
