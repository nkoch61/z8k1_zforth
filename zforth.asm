        cpu     z8002
        supmode off

        include ../zbios/z8k_sc.inc
        include ../zbios/z8k_scmacros.inc
        include zf_defs.inc
        include zf_macros.inc
        include zf_regs.inc


ZFORTH_VERSION  =       20210615
DEBUG           =       0
DEBUG_TICK      =       0
DEBUG_EXEC      =       1


; **************************************
; * Cold entry                         *
; **************************************

        org     0


cold_init:
        ld      var_HERE, #FIRST_FREE
        ld      var_LATEST, #LAST_lfa
        jr      warm_init


; **************************************
; * Warm entry                         *
; **************************************

        org     20h


warm_init:
        ld      RSP, #RETURN_STACK_TOP
        ld      PSP, #PARA_STACK_TOP
        push    @PSP, #0
        ld      var_S0, PSP
        ld      var_STATE, #IMMEDIATE_MODE
        ld      keybufptr, #keybuf
        ld      keybuftop, #keybuf
        ld      var_DPL, #-1

        clr     TOS
        lda     IP, COLD
        NEXT

COLD:
	FORTH   QUIT

; **************************************
; * Low level functions                *
; **************************************

docol:
        push    @RSP, IP
        ld      IP, W
        NEXT

; **************************************

docon:
        push    @PSP, TOS
        ld      TOS, @W
        NEXT

; **************************************

do2con:
        push    @PSP, TOS
        ld      TOS, W(#2)
        push    @PSP, TOS
        ld      TOS, @W
        NEXT

; **************************************

dovar:
        push    @PSP, TOS
        ld      TOS, W
        NEXT

; **************************************

dodoes;
        push    @PSP, TOS
        ld      TOS, W          ; PFA
        ex      @RSP, IP
        NEXT

; **************************************
; Forth words begin here               *
; **************************************

LFA     :=      0

; **************************************
; **************************************

        DEFVAR  "STATE", state, 0
        DEFVAR  "HERE", here, FIRST_FREE
        DEFVAR  "LATEST", latest, LAST_lfa
        DEFVAR  "S0", s0, PARA_STACK_TOP
	DEFVAR  "BASE", base, 10
	DEFVAR  "DPL", dpl, -1

; **************************************

        DEFCODE '<BREAK>', breakpoint
        syscall_BREAKPOINT
        NEXT

; **************************************

        DEFCODE '<TRACE>', trace
        syscall_TRACE   IP
        NEXT

; **************************************

        ; n1 n2 -- n1
        DEFCODE 'DROP', drop
        pop     TOS, @PSP
        NEXT

; **************************************

        ; n1 n2 -- n2 n1
        DEFCODE 'SWAP', swap
        ex      TOS, @PSP
        NEXT

; **************************************

        ; n1 -- n1 n1
        DEFCODE 'DUP', dup
        push    @PSP, TOS
        NEXT

; **************************************

        ; n1 n2 -- n1 n2 n1
        DEFCODE 'OVER', over
        push    @PSP, TOS
        ld      TOS, PSP(#2)
        NEXT

; **************************************

n1      :=      r0
n2      :=      r1
n3      :=      TOS

        ; n1 n2 n3 -- n2 n3 n1
        DEFCODE 'ROT', rot
        pop     n2, @PSP        ; n2
        pop     n1, @PSP        ; n1
        push    @PSP, n2        ; n2
        push    @PSP, n3        ; n3
        ld      TOS, n1         ; n1
        NEXT

; **************************************

n1      :=      r0
n2      :=      r1
n3      :=      TOS

        ; n1 n2 n3 -- n3 n1 n2
        DEFCODE '-ROT', nrot
        pop     n2, @PSP        ; n2
        pop     n1, @PSP        ; n1
        push    @PSP, n3        ; n3
        push    @PSP, n1        ; n1
        ld      TOS, n2         ; n2
        NEXT

; **************************************

        ; n1 n2 --
        DEFCODE '2DROP', twodrop
        pop     TOS, @PSP
        pop     TOS, @PSP
        NEXT

; **************************************

        ; n1 n2 -- n1 n2 n1 n2
        DEFCODE '2DUP', twodup
        ld      TMP, @PSP       ; n1 n2
        push    @PSP, TOS       ; n1 n2 n2
        push    @PSP, TMP       ; n1 n2 n1 n2
        NEXT

; **************************************

        ; n1 n2 n3 n4 -- n3 n4 n2 n1
        DEFCODE '2SWAP', twoswap
        pop     TMP, @PSP       ; n3 | n1 n2 n4
        pop     TMP2, @PSP      ; n2 | n1 n4
        pop     TMP3, @PSP      ; n1 | n4
        push    @PSP, TMP       ; n3 n4
        push    @PSP, TOS       ; n3 n4 n4
        push    @PSP, TMP2      ; n3 n4 n2 n4
        ld      TOS, TMP3       ; n3 n4 n2 n1
        NEXT

; **************************************

        ; n1 -- (n1 != 0) ? n1 n1 : n1
        DEFCODE '?DUP', qdup
        test    TOS
        jr      z, .done

        push    @PSP, TOS

.done:
        NEXT

; **************************************

        ; n -- n+1
	DEFCODE '1+', incr
        inc      TOS, #1
	NEXT

; **************************************

        ; n -- n-1
	DEFCODE '1-', decr
        dec     TOS, #1
	NEXT

; **************************************

        ; n -- n+2
	DEFCODE '2+', incr2
        inc     TOS, #2
	NEXT

; **************************************

        ; n -- n-2
	DEFCODE '2-', decr2
        dec     TOS, #2
	NEXT

; **************************************

        ; n1 n2 -- n1+n2
        DEFCODE '+', add
        add     TOS, @PSP
        inc     PSP, #2
	NEXT

; **************************************

        ; n1 n2 -- n1-n2
	DEFCODE '-', sub
        pop     TMP, @PSP
        sub     TMP, TOS
        ld      TOS, TMP
	NEXT

; **************************************

        ; n1 n2 -- n1*n2
	DEFCODE '*', mul
        exts    TOS.D
        pop     TMP, @PSP
        mult    TOS.D, TMP
	NEXT

; **************************************

        ; n1 n2 -- n1%n2 n1/n2
	DEFCODE '/MOD', divmod
        pop     TMP, @PSP
        exts    TMP.D
        div     TMP.D, TOS
        push    @PSP, TMP.HW
        ld      TOS, TMP
	NEXT

; **************************************

        ; n1 n2 -- n1==n2
        DEFCODE '=', equ
        pop     TMP, @PSP
        TOFLAG  TMP, eq, TOS
	NEXT

; **************************************

        ; n1 n2 -- n1!=n2
        DEFCODE '<>', nequ
        pop     TMP, @PSP
        TOFLAG  TMP, ne, TOS
	NEXT

; **************************************

        ; n1 n2 -- n1<n2
        DEFCODE '<', lt
        pop     TMP, @PSP
        TOFLAG  TMP, lt, TOS
	NEXT

; **************************************

        ; n1 n2 -- n1>n2
        DEFCODE '>', gt
        pop     TMP, @PSP
        TOFLAG  TMP, gt, TOS
	NEXT

; **************************************

        ; n1 n2 -- n1<=n2
        DEFCODE '<=', le
        pop     TMP, @PSP
        TOFLAG  TMP, le, TOS
	NEXT

; **************************************

        ; n1 n2 -- n1>=n2
        DEFCODE '>=', ge
        pop     TMP, @PSP
        TOFLAG  TMP, ge, TOS
	NEXT

; **************************************

        ; n -- n==0
        DEFCODE '0=', zequ
        TOFLAG  TOS, eq
	NEXT

; **************************************

        ; n -- n!=0
        DEFCODE '0<>', znequ
        TOFLAG  TOS, ne
	NEXT

; **************************************

        ; n -- n<0
        DEFCODE '0<', zlt
        TOFLAG  TOS, lt
	NEXT

; **************************************

        ; n -- n>0
        DEFCODE '0>', zgt
        TOFLAG  TOS, gt
	NEXT

; **************************************

        ; n -- n<=0
        DEFCODE '0<=', zle
        TOFLAG  TOS, le
	NEXT

; **************************************

        ; n -- n>=0
        DEFCODE '0>=', zge
        TOFLAG  TOS, ge
	NEXT

; **************************************

        ; n1 n2 -- n1&n2
        DEFCODE 'AND', and
        and     TOS, @PSP
        inc     PSP, #2
	NEXT

; **************************************

        ; n1 n2 -- n1|n2
        DEFCODE 'OR', or
        or      TOS, @PSP
        inc     PSP, #2
	NEXT

; **************************************

        ; n1 n2 -- n1^n2
        DEFCODE 'XOR', xor
        xor     TOS, @PSP
        inc     PSP, #2
	NEXT

; **************************************

        ; n -- ~n
        DEFCODE 'INVERT', invert
        com     TOS
	NEXT

; **************************************

        DEFCODE 'EXIT', exit
        pop     IP, @RSP
        NEXT

; **************************************

        DEFCODE 'LIT', lit
        push    @PSP, TOS
        pop     TOS, @IP
        NEXT

; **************************************

        DEFCODE '2LIT', twolit
        push    @PSP, TOS
        pop     TOS, @IP
        pop     TMP, @IP
        push    @PSP, TMP
        NEXT

; **************************************

        ; n a --
        DEFCODE '!', store
        pop     TMP, @PSP       ; n
        ld      @TOS, TMP
        pop     TOS, @PSP
	NEXT

; **************************************

        ; a -- [a]
        DEFCODE '@', fetch
        ld      TOS, @TOS
	NEXT

; **************************************

        ; n a --
        DEFCODE '+!', addstore
        pop     TMP, @PSP       ; n
        add     TMP, @TOS
        ld      @TOS, TMP
        pop     TOS, @PSP
	NEXT

; **************************************

        ; n a --
        DEFCODE '-!', substore
        pop     TMP, @PSP       ; n
        ld      TMP2, @TOS      ; [a]
        sub     TMP2, TMP
        ld      @TOS, TMP2
        pop     TOS, @PSP
	NEXT

; **************************************

        DEFCODE 'C!', storebyte
        pop     TMP, @PSP       ; n
        ld      @TOS, TMP.LB
        pop     TOS, @PSP
	NEXT

; **************************************

        DEFCODE 'C@', fetchbyte
        ld      TOS.LB, @TOS
        clr     TOS.HB
	NEXT

; **************************************

        ; src dst -- src dst
        DEFCODE 'C@C!', ccopy
        pop     TMP, @PSP       ; src
        ldib    @TOS, @TMP, TMP2
        push    @PSP, TMP
	NEXT

; **************************************

        ; src dst n --
        DEFCODE 'CMOVE', cmove
        pop     TMP, @PSP       ; dst
        pop     TMP2, @PSP      ; src
        test    TOS
        jr      z, .nocopy

        ldirb   @TMP, @TMP2, TOS

.nocopy:
        pop     TOS, @PSP
	NEXT

; **************************************
; **************************************

	DEF2CONST       "VERSION", VERSION, ZFORTH_VERSION
	DEFCONST        "R0", RZ, RETURN_STACK_TOP
	DEFCONST        "DOCOL", __DOCOL, docol
	DEFCONST        "DOCON", __DOCON, docon
	DEFCONST        "DOVAR", __DOVAR, dovar
	DEFCONST        "F_IMMED", __F_IMMED, F_IMMED
	DEFCONST        "F_HIDDEN", __F_HIDDEN, F_HIDDEN
	DEFCONST        "F_LENMASK", __F_LENMASK, F_LENMASK
	DEFCONST        "GET-BRK", __getbrk, PARA_STACK_BOTTOM-2

; **************************************
; **************************************

	DEFCODE ">R", TOR
        push    @RSP, TOS
        pop     TOS, @PSP
	NEXT

; **************************************

	DEFCODE "R>", FROMR
        push    @PSP, TOS
        pop     TOS, @RSP
	NEXT

; **************************************

	DEFCODE "RSP@", rspfetch
	push    @PSP, TOS
	ld      TOS, RSP
	NEXT

; **************************************

	DEFCODE "RSP!", rspstore
        ld      RSP, TOS
        pop     TOS, @PSP
	NEXT

; **************************************

	DEFCODE "RDROP", rdrop
        inc     RSP, #2
	NEXT

; **************************************

        DEFCODE "DSP@", dspfetch
        push    @PSP, TOS
        ld      TOS, PSP
        NEXT

; **************************************

        DEFCODE "DSP!", dspstore
        ld      PSP, TOS
        pop     TOS, @PSP
        NEXT

; **************************************

PTR     :=      r3
N       :=      r1
RES     :=      r0

IPTR    :=      r2

        ; addr n -- n
	DEFCODE "ACCEPT", accept
        pop     PTR, @PSP
        ld      N, TOS
        call    _accept
        ld      TOS, RES
	NEXT

_accept:
        ld      IPTR, initbufptr
        cp      IPTR, #init_forth_end
        jr      uge, .terminal

        clr     RES
        test    N
        ret     z

.loop:
        cpb     @IPTR, #'\n'
        jr      eq, .eol

        test    do_echo_init
        jr      z, .no_echo

        sub     RSP, #4*2
        ldm     @RSP, r0, #4
        ld      rl0, @IPTR
        ldb     emitbuf+0, rl0
        syscall_TERMOUT emitbuf, #1
        ldm     r0, @RSP, #4
        add     RSP, #4*2

.no_echo:
        inc     RES
        ldib    @PTR, @IPTR, N
        jr      ov, .done

        cp      IPTR, #init_forth_end
        jr      ult, .loop

        jr      .done

.eol:
        inc     IPTR

.done:
        ld      initbufptr, IPTR
        ldb     emitbuf+0, #'\r'
        ldb     emitbuf+1, #'\n'

        push    @RSP, RES
        syscall_TERMOUT emitbuf, #2
        pop     RES, @RSP
        ret

.terminal:
        sc      #sys_termreadline
        ret


	DEFCODE "-ECHO-INIT", echo_init_off
        ld      do_echo_init, #0
	NEXT


	DEFCODE "+ECHO-INIT", echo_init_on
        ld      do_echo_init, #1
	NEXT


; **************************************

  if DEBUG_EXEC

TMPD    :=      r6
TMPD.LB :=      rh7
TMPD.HB :=      rh6
TMPC    :=      r5
TMPC.LB :=      rl5
TMPC.HB :=      rh5
TMPB    :=      r4
TMPB.LB :=      rl4
TMPB.HB :=      rh4
IPTR    :=      r4
PTR     :=      r3
N       :=      r1
N.lb    :=      rl1
TMPA    :=      r0
TMPA.LB :=      rl0
TMPA.HB :=      rh0
RES     :=      r0


emit_inline:
        pop     IPTR, @r15

.loop:
        ld      TMP.LB, @IPTR
        ldib    @PTR, @IPTR, N
        test    TMP.LB
        jr      nz, .loop

        INC     iptr
        AND     IPTR, #0FFFEh
        jp      @IPTR


emit_tos:
        ld      TMPA, TOS


emit_tmpa:
        ld      TMPC, #4

.loop:
        ld      TMPB, TMPA
        sll     TMPA, #4
        srl     TMPB, #12
        and     TMPB.LB, #0Fh
        add     TMPB.LB, #'0'
        cp      TMPB.LB, #'9'
        jr      ule, .out

        add     TMPB.LB, #'A'-'9'-1

.out:
        ld      @PTR, TMPB.LB
        inc     PTR
        djnz    TMPC, .loop

        ret


EXEC_TRACE_OFF          equ     0
EXEC_TRACE_ON           equ     1
EXEC_TRACE_DEFER_ON     equ     -1


exec_trace:     ; @xt in W, xxx_code in X
        cp      X, #code_exec_trace_on
        ret     eq

        test    do_exec_trace
        ret     z

        ld      TMPA, #EXEC_TRACE_ON
        ex      TMPA, do_exec_trace
        cp      TMPA, #EXEC_TRACE_ON
        ret     ne

        ld      PTR, #emitbuf

        call    emit_inline
        db      " \e[96m SP:", 0
        align   2

        ld      TMPA, PSP
        call    emit_tmpa

        call    emit_inline
        db      " NOS:", 0
        align   2

        ld      TMPA, @PSP
        call    emit_tmpa

        call    emit_inline
        db      " TOS:", 0
        align   2

        call    emit_tos

        call    emit_inline
        db      " HERE:", 0
        align   2

        ld      TMPA, var_HERE
        call    emit_tmpa

        call    emit_inline
        db      " LAST:", 0
        align   2

        ld      TMPA, var_LATEST
        call    emit_tmpa

        call    emit_inline
        db      " ", 0
        align   2

        ld      IPTR, W(#-4)
        ld      N.lb, IPTR(#2)
        and     N, #F_LENMASK
        inc     IPTR, #3
        ldirb   @PTR, @IPTR, N

        call    emit_inline
        db      "\e[97m\r\n", 0
        align   2

        ld      N, PTR
        ld      PTR, #emitbuf
        sub     N, PTR
        sc      sys_TERMOUT

        ret


no_exec_trace:
        ret


	DEFCODE "+EXEC-TRACE", set_exec_trace_handler
        ld      exec_trace_handler, #exec_trace
	NEXT


	DEFCODE "-EXEC-TRACE", clr_exec_trace_handler
        ld      exec_trace_handler, #no_exec_trace
	NEXT


	DEFCODE "EXEC-TRACE[", exec_trace_on
        ld      do_exec_trace, #EXEC_TRACE_DEFER_ON
	NEXT


	DEFCODE "]EXEC-TRACE", exec_trace_off
        ld      do_exec_trace, #EXEC_TRACE_OFF
	NEXT

  endif

; **************************************

        ; -- <key>
	DEFCODE "KEY", key
        push    @PSP, TOS
        call    _key
        clr     TOS.HB
	NEXT

_key:   ; -> TOS.LB
        ld      TMP, keybufptr
        cp      TMP, keybuftop
        jr      uge, .read_line

        ld      TOS.LB, @TMP
        inc     keybufptr
	ret

.read_line:
        cp      var_STATE, #IMMEDIATE_MODE
        jr      ne, .no_prompt

        ldb     emitbuf+0, #'O'
        ldb     emitbuf+1, #'K'
        ldb     emitbuf+2, #' '
        syscall_TERMOUT emitbuf, #3

.no_prompt:
        ld      PTR, #keybuf
        ld      N, #keybuflen-1
        call    _accept
        test    r0
        jr      pl, .line_ok

        syscall_BREAKPOINT
        jr      .read_line

.line_ok:
        ld      TMP, r0
        add     TMP, #keybuf
        ldb     @TMP, #'\n'
        inc     TMP
        ld      keybuftop, TMP
        ld      keybufptr, #keybuf
        jr      _key

; **************************************

        ; <char> --
	DEFCODE "EMIT", emit
        cp      TOS.LB, #'\n'
        jr      ne, .emit1

        ldb     emitbuf+0, #'\r'
        ldb     emitbuf+1, #'\n'

        syscall_TERMOUT emitbuf, #2
        jr      .done

.emit1:
        ld      emitbuf, TOS.LB
        syscall_TERMOUT emitbuf, #1

.done:
        pop     TOS, @PSP
	NEXT

; **************************************

        ; -- buf len
	DEFCODE "WORD", word
        push    @PSP, TOS
        call    _word

  if DEBUG
        ldb     emitbuf+0, #'"'
        syscall_TERMOUT emitbuf, #1
        ld      TOS, wordbufptr
        sub     TOS, #wordbuf
        syscall_TERMOUT wordbuf, TOS
        ldb     emitbuf+1, #'\r'
        ldb     emitbuf+2, #'\n'
        syscall_TERMOUT emitbuf, #3
  endif

        push    @PSP, #wordbuf
        ld      TOS, wordbufptr
        sub     TOS, #wordbuf
	NEXT

_word:
        ld      wordbufptr, #wordbuf

.skip_ws:
        call    _key
        cp      TOS.LB, #'\\'
        jr      eq, .skip_comment

        cp      TOS.LB, #' '
        jr      ule, .skip_ws

.read_nws:
        ld      TMP, wordbufptr
        ld      @TMP, TOS.LB
        inc     wordbufptr
        cp      wordbufptr, #wordbufend
        jr      uge, .skip_nws

        call    _key
        cp      TOS.LB, #' '
        jr      ugt, .read_nws

        ret

.skip_nws:
        call    _key
        cp      TOS.LB, #' '
        jr      ugt, .skip_nws

        ret

.skip_comment:
        call    _key
        cp      TOS.LB, #'\n'
        jr      ne, .skip_comment

        jr      .skip_ws

; **************************************

NUMBER.Q        :=      rq0
NUMBER.D        :=      rr2
NUMBER.HW       :=      r2
NUMBER.W        :=      r3
DIGIT.D         :=      rr4
DIGIT.HW        :=      r4
DIGIT.W         :=      r5
DIGIT.LB        :=      rl5
DIGIT.HB        :=      rh5
BUFPTR          :=      r8
BUFEND          :=      r9

        ; buf len - number left
	DEFCODE "NUMBER", number
        pop     BUFPTR, @PSP    ; buf
        ld      BUFEND, BUFPTR
        add     BUFEND, TOS     ; len
        call    _number
        push    @PSP, NUMBER.W  ; number
        ld      TOS, BUFEND
        sub     TOS, BUFPTR     ; left
        cp      var_DPL, #-1
        jr      eq, .next

        push    @PSP, NUMBER.HW

.next:
	NEXT


_number:
        ld      NUMBER.D, #0
        ld      var_DPL, #-1

        cp      BUFPTR, BUFEND
        ret     uge

        ld      DIGIT.D, #0
        cpb     @BUFPTR, #'-'
        jr      ne, .parse_loop

        call    .skip_sign
        ld      DIGIT.D, #0
        sub     DIGIT.D, NUMBER.D
        ld      NUMBER.D, DIGIT.D
        ret

.skip_sign:
        inc     BUFPTR, #1
        cp      BUFPTR, BUFEND
        jr      ult, .parse_loop

        dec     BUFPTR, #1
        ret

.parse_loop:
        ld      DIGIT.LB, @BUFPTR
        cp      DIGIT.LB, #'.'
        jr      ne, .check_digits

        ld      var_DPL, #0


.check_digits:
        cp      DIGIT.LB, #'0'
        ret     ult

        cp      DIGIT.LB, #'9'
        jr      ugt, .not_digit

        sub     DIGIT.LB, #'0'
        jr      .legal_char

.not_digit:
        cp      DIGIT.LB, #'A'
        ret     ult

        cp      DIGIT.LB, #'Z'
        jr      ugt, .not_upper

        sub     DIGIT.LB, #'A'-10
        jr      .legal_char

.not_upper:
        cp      DIGIT.LB, #'a'
        ret     ult

        cp      DIGIT.LB, #'z'
        ret     ugt

        sub     DIGIT.LB, #'a'-10

.legal_char:
        cp      DIGIT.W, var_BASE
        ret     uge

        push    @PSP, DIGIT.D
        ld      DIGIT.W, var_BASE
        clr     DIGIT.HW
        mult    NUMBER.Q, DIGIT.D
        pop     DIGIT.D, @PSP
        add     NUMBER.D, DIGIT.D

        cp      var_DPL, #-1
        jr      eq, .no_inc

        inc     var_DPL

.no_inc:
        inc     BUFPTR
        cp      BUFPTR, BUFEND
        jr      ult, .parse_loop

        ret

; **************************************

WORDLEN         :=      r0
WORDLEN.LB      :=      rl0
BUFLEN.LB       :=      rl1
LINKFIELD       :=      TOS
BUFPTR          :=      r3
WORDPTR         :=      r4
BUFFER          :=      r5

        ; buf len - address-or-0
	DEFCODE "FIND", find
        pop     BUFFER, @PSP            ; buf
        ld      BUFLEN.LB, TOS.LB       ; len
        call    _find
	NEXT


_find:
        ld      LINKFIELD, var_LATEST

.find_loop:
        ld      WORDLEN.LB, LINKFIELD(#2)
        and     WORDLEN, #F_LENMASK
        cp      WORDLEN.LB, BUFLEN.LB
        jr      ne, .next

        lda     WORDPTR, LINKFIELD(#3)
        ld      BUFPTR, BUFFER

.loop:
        cpsirb  @WORDPTR, @BUFPTR, WORDLEN, ne
        jr      z, .next

        ret

.next:
        ld      LINKFIELD, @LINKFIELD
        test    LINKFIELD
        ret     z

        jr      .find_loop

; **************************************

        ; LFA -- CFA
	DEFCODE ">CFA", tocfa
        ld      TMP.LB, TOS(#2)
        and     TMP, #F_LENMASK
        add     TOS, TMP
        inc     TOS, #2+2+1+1   ; lf+backlink+length byte + opt. alignment
        and     TOS, #0FFFEh    ; make even
	NEXT

; **************************************

        ; CFA -- LFA
	DEFCODE "CFA>", fromcfa
        ld      TOS, TOS(#-2)
	NEXT

; **************************************

        ; LFA -- DFA
	DEFCODE ">DFA", todfa
        ld      TMP.LB, TOS(#2)
        and     TMP, #F_LENMASK
        add     TOS, TMP
        inc     TOS, #2+2+1+1+2 ; lf+backlink+length byte + opt. alignment + cf
        and     TOS, #0FFFEh    ; make even
	NEXT

; **************************************

BASE    :=      TMP
PTR     :=      TMP2
WORD    :=      TMP3

        ; address length --
	DEFCODE "CREATE", create
        ld      BASE, var_HERE
        inc     BASE
        and     BASE, #0FFFEh
        LD      PTR, var_LATEST
        ld      @BASE, PTR      ; LFA
        lda     PTR, BASE(#2)
        ld      @PTR, TOS.LB
        inc     PTR
        pop     WORD, @PSP
        test    TOS.LB
        jr      z, .nocopy

        test    WORD
        jr      z, .nocopy

        ldirb   @PTR, @WORD, TOS

.nocopy:
        inc     PTR
        and     PTR, #0FFFEh
        ld      @PTR, BASE      ; backlink LFA
        inc     PTR, #2         ; CFA
        ld      var_HERE, PTR
        ld      var_LATEST, BASE
        pop     TOS, @PSP
	NEXT

; **************************************

        ; word --
	DEFCODE ",", comma
        ld      TMP, var_HERE
        ld      @TMP, TOS
        inc     var_HERE, #2
        pop     TOS, @PSP
	NEXT

; **************************************

        DEFCODE "[", lbrac, F_IMMED
        ld      var_STATE, #IMMEDIATE_MODE
        NEXT

; **************************************

        DEFCODE "]", rbrac
        ld      var_STATE, #COMPILE_MODE
        NEXT

; **************************************

        DEFWORD ":", colon
        FORTH           WORD
        FORTH           CREATE
        FORTH           __DOCOL, COMMA
        FORTH           LATEST, FETCH, HIDDEN
	FORTH           RBRAC
        FORTH           EXIT

; **************************************

        DEFWORD ";", semicolon, F_IMMED
        FORTHLITF       EXIT
        FORTH           COMMA
        FORTH           LATEST, FETCH, HIDDEN
	FORTH           LBRAC
        FORTH           EXIT

; **************************************

        DEFCODE "IMMEDIATE", immediate, F_IMMED
        ld      TMP2, var_LATEST
        ld      TMP.LB, TMP2(#2)
        xor     TMP.LB, #F_IMMED
        ld      TMP2(#2), TMP.LB
	NEXT

; **************************************

        DEFCODE "HIDDEN", hidden
        inc     TOS, #2
        ld      TMP.LB, @TOS
        xor     TMP.LB, #F_HIDDEN
        ld      @TOS, TMP.LB
        pop     TOS, @PSP
        NEXT

; **************************************

	DEFWORD "HIDE", hide
	FORTH           WORD, FIND, HIDDEN
	FORTH           EXIT

; **************************************

        DEFWORD "'", tick, F_IMMED
        FORTH           STATE, FETCH
        FORTH0BRANCH    .immediate

.compiling:
  if DEBUG_TICK
  FORTHLITSTRING 'tick compile\r\n'
  FORTH TELL
  endif
        FORTHLITF       LIT
        FORTH           COMMA
        FORTHBRANCH     .exit

.immediate:
  if DEBUG_TICK
  FORTHLITSTRING 'tick immediate\r\n'
  FORTH TELL
  endif
	FORTH           WORD, FIND, QDUP
        FORTH0BRANCH    .exit

        FORTH           TOCFA

.exit:
	FORTH           EXIT

; **************************************

	DEFCODE "BRANCH", branch
        add     IP, @IP
	NEXT

; **************************************

	DEFCODE "0BRANCH", zbranch
	test    TOS
        pop     TOS, @PSP
        jr      z, code_BRANCH

        inc     IP, #2
	NEXT

; **************************************

        ; -- address length
	DEFCODE "LITSTRING", litstring
        push    @PSP, TOS
        ld      TOS, @IP        ; length
        inc     IP, #2
        push    @PSP, IP        ; address
        add     IP, TOS
        inc     IP
        and     IP, #0FFFEh
	NEXT

; **************************************

        ; address length --
	DEFCODE "TELL", tell
        pop     TMP, @PSP
        syscall_TERMOUT TMP(#0), TOS
        pop     TOS, @PSP
	NEXT

; **************************************
; **************************************

	DEFWORD "QUIT", quit
.forever:
  if DEBUG
  FORTHLITSTRING 'QUIT:\r\n'
  FORTH TELL
  endif
	FORTH           RZ, RSPSTORE
	FORTH           INTERPRET
	FORTHBRANCH     .forever

; **************************************

        DEFWORD "INTERPRET", interpret
.forever:
  if DEBUG
  FORTHLITSTRING 'INTERPRET:\r\n'
  FORTH TELL
  endif
        FORTH           WORD                    ; <word-addr> <len>
        FORTH           TWODUP                  ; <word-addr> <len> <word-addr <len>>
        FORTH           FIND                    ; <word-addr> <len> <dict-addr or 0>
        FORTH           QDUP                    ; <word-addr> <len> <dict-addr or 0> <dict-addr or 0>?
        FORTH0BRANCH    .not_in_dict            ; <word-addr> <len>

        FORTH           SWAP                    ; <word-addr> <dict-addr> <len>
        FORTH           DROP                    ; <word-addr> <dict-addr>
        FORTH           SWAP                    ; <dict-addr> <word-addr>
        FORTH           DROP                    ; <dict-addr>
        FORTH           STATE, FETCH            ; <dict-addr> STATE
        FORTH0BRANCH    .immediate              ; <dict-addr>

        FORTH           DUP                     ; <dict-addr> <dict-addr>
        FORTH           INCR2, FETCHBYTE, __F_IMMED, AND, ZEQU
        FORTH0BRANCH    .immediate              ; <dict-addr>

.compile:                                       ; <dict-addr>
        FORTH           TOCFA, COMMA
  if DEBUG
  FORTHLITSTRING 'word compiled\r\n'
  FORTH TELL
  endif
        FORTHBRANCH     .exit

.immediate:                                     ; <dict-addr>
        FORTH           TOCFA
  if DEBUG_EXEC
        FORTH           EXEC_TRACE_ON
  endif
        FORTH           EXECUTE
  if DEBUG_EXEC
        FORTH           EXEC_TRACE_OFF
  endif
  if DEBUG
  FORTHLITSTRING 'word executed\r\n'
  FORTH TELL
  endif
        FORTHBRANCH     .exit

.not_in_dict:                                   ; <word-addr> <len>
        FORTH           NUMBER                  ; <number> <chars left>
        FORTH           ZEQU                    ; <number> <chars left==0>
        FORTH0BRANCH    .parse_error            ; <number>

        FORTH           STATE, FETCH            ; <number> STATE
        FORTH0BRANCH    .exit

.compile_number:
        FORTH           DPL, FETCH, ZGE
        FORTH0BRANCH    .compile_single

.compile_double:
        FORTHLITF       TWOLIT                  ; <number> <cfa_TWOLIT>
        FORTH           COMMA, COMMA, COMMA
  if DEBUG
  FORTHLITSTRING 'double number compiled\r\n'
  FORTH TELL
  endif
        FORTHBRANCH     .exit

.compile_single:
        FORTHLITF       LIT                     ; <number> <cfa_LIT>
        FORTH           COMMA, COMMA
  if DEBUG
  FORTHLITSTRING 'number compiled\r\n'
  FORTH TELL
  endif
        FORTHBRANCH     .exit

.parse_error:                                   ; <number>
        FORTH           DROP
        FORTHLITSTRING  '\e[91m***PARSE ERROR:\r\n'
        FORTH           TELL
        FORTHLIT        keybufptr
        FORTH           FETCH                   ; keybufptr
        FORTHLIT        keybuf                  ; keybufptr keybuf
        FORTH           SUB                     ; keybufptr-keybuf
        FORTHLIT        40                      ; keybufptr-keybuf 40
        FORTH           MIN                     ; n=min(keybufptr-keybuf, 40)
        FORTH           DUP                     ; n n
        FORTHLIT        keybufptr
        FORTH           FETCH                   ; n n keybufptr
        FORTH           SWAP                    ; n keybufptr n
        FORTH           SUB                     ; n keybufptr-n
        FORTH           SWAP                    ; keybufptr-n n
        FORTH           TELL
        FORTHLITSTRING  '\e[97m\r\n'
        FORTH           TELL
        FORTHLIT        IMMEDIATE_MODE
        FORTH           STATE, STORE
        FORTHLIT        keybuf
        FORTHLIT        keybuftop
        FORTH           STORE
        FORTHLIT        init_forth_end
        FORTHLIT        initbufptr
        FORTH           STORE

.exit:
	FORTH           EXIT

; **************************************
; **************************************

	DEFCODE "CHAR", char
        push    @PSP, TOS
	call    _word
        clr     TOS
        ld      TOS.LB, wordbuf
	NEXT

; **************************************

        DEFCODE 'EXECUTE', execute
        ld      W, TOS
        pop     TOS, @PSP
        EXEC

; **************************************
; **************************************

        DEFCODE 'BYE', bye
        syscall_EXIT    #0

; **************************************
; **************************************

        ; n1 n2 -- max(n1, n2)
	DEFCODE 'MAX', max
        pop     TMP, @PSP
        cp      TOS, TMP
        jr      ge, .done

        ld      TOS, TMP

.done:
	NEXT

; **************************************

        ; n1 n2 -- min(n1, n2)
	DEFCODE 'MIN', min
        pop     TMP, @PSP
        cp      TOS, TMP
        jr      le, .done

        ld      TOS, TMP

.done:
	NEXT

; **************************************

	DEFWORD "(;CODE)", semi_code
        FORTH           FROMR
        FORTH           LATEST, FETCH, TOCFA, STORE
	FORTH           EXIT

; **************************************

	DEFWORD "DOES>", does, F_IMMED
        FORTHLITF       SEMI_CODE
        FORTH           COMMA
        FORTHLIT        5F00h   ; call
        FORTH           COMMA
        FORTHLIT        dodoes
        FORTH           COMMA   ; call/jp dodoes
	FORTH           EXIT

; **************************************
; **************************************

keybuf                  db      128 dup(?)
keybufend               =       $
keybuflen               =       keybufend-keybuf
        align   2
keybufptr               dw      keybuf
keybuftop               dw      keybuf

initbufptr              dw      init_forth

wordbuf                 db      keybuflen dup(?)
wordbufend              =       $
wordbuflen              =       wordbufend-wordbuf
        align   2
wordbufptr              dw      wordbuf

emitbuf                 db      128 dup (?)
        align   2

  if DEBUG_EXEC
do_exec_trace           dw      0
exec_trace_handler      dw      exec_trace
  endif
do_echo_init            dw      1

LAST_lfa                =       LFA
FIRST_FREE              =       $

; **************************************
; **************************************

        rorg            8192

init_forth:
        binclude        zforth.f
init_forth_end:

; **************************************
; **************************************


        end
