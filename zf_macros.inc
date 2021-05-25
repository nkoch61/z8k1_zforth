; zf_macros.inc


EXEC            macro
        pop     X, @W
        jp      @X
        endm


NEXT            macro
        pop     W, @IP
        EXEC
        endm


DEFHEAD         macro   name, label, flags=0
	align   2
lfa_label       =       $
	dw      LFA
LFA             :=      lfa_label
        db      flags+STRLEN(name)
        db      name
        align   2
        dw      lfa_label
cfa_label       =       $
        endm


DEFWORD         macro   name, label, flags=0
        DEFHEAD         name, label, flags
	dw      docol
        endm


DEFCODE         macro   name, label, flags=0
        DEFHEAD         name, label, flags
	dw      $+2
code_label      =       $
        endm


DEFCONST        macro   name, label, value, flags=0
        DEFHEAD         name, label, flags
        dw      docon
        dw      value
        endm


DEF2CONST       macro   name, label, value, flags=0
        DEFHEAD         name, label, flags
        dw      do2con
        dd      value
        endm


DEFVAR          macro   name, label, initial, flags=0
        DEFHEAD         name, label, flags
        dw      dovar
var_label       =       $
        dw      initial
        endm


DEF2VAR         macro   name, label, initial, flags=0
        DEFHEAD         name, label, flags
        dw      dovar
var_label       =       $
        dd      initial
        endm


FORTH           macro
        align   2
        irp     label, ALLARGS
        dw      cfa_label
        endm
        endm


FORTHLIT        macro   n
        FORTH   LIT
        dw      n
        endm


FORTHLITF       macro   n
        FORTH   LIT
        FORTH   n
        endm


FORTHLITSTRING  macro   string
        FORTH   LITSTRING
        dw      strlen(string)
        db      string
        align   2
        endm


FORTHBRANCH     macro   dest
        FORTH           BRANCH
        dw              dest-$
        ENDM


FORTH0BRANCH    macro   dest
        FORTH           ZBRANCH
        dw              dest-$
        ENDM


TOFLAG          macro   arg1, cmpop, arg2=#0
        cp      arg1, arg2
        tcc     cmpop, TOS
        endm
