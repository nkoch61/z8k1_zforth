        cpu     z8002
        supmode off

        include ../zbios/z8k_sc.inc
        include ../zbios/z8k_scmacros.inc
        include zf_defs.inc
        include zf_macros.inc
        include zf_regs.inc


ZFORTH_VERSION  =       20210525


        org     0


; **************************************
; * Entry                              *
; **************************************

begin:
        lda     RSP, RETURN_STACK_TOP
        lda     PSP, PARA_STACK_TOP
        push    @PSP, #0
        ld      var_S0, PSP
        ld      var_STATE, #IMMEDIATE_MODE
        lda     r0, keybuf
        ld      keybufptr, r0
        ld      keybuftop, r0

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
; Forth words begin here               *
; **************************************

LFA     :=      0

; **************************************

        DEFCODE '<BREAKPOINT>', breakpoint
        syscall_BREAKPOINT
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

        ; n1 n2 n3 -- n1 n2 n3 n2
        DEFCODE 'OVER', over
        push    @PSP, TOS
        ld      TOS, PSP(#4)
        NEXT

; **************************************

        ; n1 n2 n3 -- n2 n3 n1
        DEFCODE 'ROT', rot
        pop     TMP, @PSP       ; n2
        pop     TMP2, @PSP      ; n1
        push    @PSP, TMP       ; n2
        push    @PSP, TOS       ; n3
        ld      TOS, TMP2       ; n1
        NEXT

; **************************************

        ; n1 n2 n3 -- n3 n1 n2
        DEFCODE '-ROT', nrot
        pop     TMP, @PSP       ; n2
        pop     TMP2, @PSP      ; n1
        push    @PSP, TOS       ; n3
        push    @PSP, TMP2      ; n1
        ld      TOS, TMP        ; n2
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
        ldirb   @TMP, @TMP2, TOS
        pop     TOS, @PSP
	NEXT

; **************************************
; **************************************

        DEFVAR  "STATE", state, 0
        DEFVAR  "HERE", here, FIRST_FREE
        DEFVAR  "LATEST", latest, LAST_lfa
        DEFVAR  "S0", s0, PARA_STACK_TOP
	DEFVAR  "BASE", base, 10

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

;DEFCODE "DSP@", dspfetch
;       ld      TMP, PSP
;       push    @PSP, TOS
;       ld      TOS, TMP
;       NEXT

; **************************************

;       DEFCODE "DSP!", dspstore
;       lda     PSP, TOS
;       pop     TOS, @PSP
;       NEXT

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
        inc     keybufptr, #1
	ret

.read_line:
        syscall_TERMREADLINE    keybuf, #keybuflen-1
        test    r0
        jr      pl, .line_ok

        syscall_BREAKPOINT
        jr      .read_line

.line_ok:
        ld      TMP, r0
        lda     TMP, keybuf(TMP)
        ld      @TMP, #'\n'
        inc     TMP, #1
        ld      keybuftop, TMP
        ld      keybufptr, #keybuf
        jr      _key

; **************************************

        ; <char> --
	DEFCODE "EMIT", emit
        cp      TOS.LB, #'\n'
        jr      ne, .emit1

        ld      emitbuf, #'\r'
        ld      emitbuf+1, #'\n'

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
        inc     wordbufptr, #1
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

        jr      _word

; **************************************

NUMBER.D        :=      rr0
NUMBER.W        :=      r1
DIGIT.LB        :=      rl2
DIGIT.HB        :=      rh2
DIGIT           :=      r2
BUFPTR          :=      r3
BUFEND          :=      r4

        ; buf len - number left
	DEFCODE "NUMBER", number
        pop     BUFPTR, @PSP    ; buf
        ld      BUFEND, BUFPTR
        add     BUFEND, TOS     ; len
        call    _number
        push    @PSP, NUMBER.W  ; number
        ld      TOS, BUFEND
        sub     TOS, BUFPTR     ; left
	NEXT


_number:
        ld      NUMBER.D, #0

        cp      BUFPTR, BUFEND
        ret     uge

        clr     DIGIT.HB
        cpb     @BUFPTR, #'-'
        jr      ne, .plus_check

        call    .with_sign
        neg     NUMBER.W
        ret

.plus_check:
        cpb     @BUFPTR, #'+'
        jr      ne, .parse_loop

.with_sign:
        inc     BUFPTR, #1
        cp      BUFPTR, BUFEND
        jr      ult, .parse_loop

        dec     BUFPTR, #1
        ret

.parse_loop:
        ld      DIGIT.LB, @BUFPTR
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
        cp      DIGIT, var_BASE
        ret     uge

        mult    NUMBER.D, #10
        add     NUMBER.W, DIGIT

        inc     BUFPTR, #1
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

        jr      nov, .loop

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
	DEFCODE ">DFA", topfa
        ld      TMP.LB, TOS(#2)
        and     TMP, #F_LENMASK
        add     TOS, TMP
        inc     TOS, #2+2+1+1+2 ; lf+backlink+length byte + opt. alignment + cf
        and     TOS, #0FFFEh    ; make even
	NEXT

; **************************************

        ; address length --
	DEFCODE "CREATE", create
        ld      TMP, var_HERE
        LD      TMP2, var_LATEST
        ld      @TMP, TMP2      ; LFA
        lda     TMP2, TMP(#2)
        ld      @TMP2, TOS.LB
        inc     TMP2, #1
        pop     TMP3, @PSP
        ldirb   @TMP2, @TMP3, TOS
        inc     @TMP2
        and     TMP2, #0FFFEh
        ld      @TMP2, TMP      ; backlink LFA
        inc     TMP2, #2
        ex      var_HERE, TMP2
        ld      var_LATEST, TMP2
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

        DEFCODE "[", lbrac
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
        FORTHLIT        DOCOL
        FORTH           COMMA
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

        DEFWORD "'", tick
	FORTH           WORD, FIND, QDUP
        FORTH0BRANCH    .not_found

        FORTH           TOCFA

.not_found:
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
        inc     IP, #1
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
	FORTH           RZ, RSPSTORE
	FORTH           INTERPRET
	FORTHBRANCH     .forever

; **************************************

        DEFWORD "INTERPRET", interpret
        FORTH           WORD                    ; <word-addr> <len>
        FORTH           TWODUP                  ; <word-addr> <len> <word-addr <len>>
        FORTH           FIND                    ; <word-addr> <len> <dict-addr or 0>
        FORTH           DUP                     ; <word-addr> <len> <dict-addr or 0> <dict-addr or 0>
        FORTH0BRANCH    .not_in_dict            ; <word-addr> <len> <dict-addr or 0>

        FORTH           SWAP                    ; <word-addr> <dict-addr> <len>
        FORTH           DROP                    ; <word-addr> <dict-addr>
        FORTH           SWAP                    ; <dict-addr> <word-addr>
        FORTH           DROP                    ; <dict-addr>
        FORTH           STATE, FETCH            ; <dict-addr> STATE
        FORTH0BRANCH    .immediate              ; <dict-addr>

        FORTH           DUP                     ; <dict-addr> <dict-addr>
        FORTH           INCR2, FETCHBYTE, __F_IMMED, AND, ZEQU
        FORTH0BRANCH    .immediate              ; <dict-addr>

.compile:
        FORTH           BREAKPOINT
        FORTH           TOCFA, COMMA
	FORTH           EXIT

.immediate:
        FORTH           BREAKPOINT
        FORTH           TOCFA, EXECUTE
	FORTH           EXIT

.not_in_dict:                                   ; <word-addr> <len> 0
        FORTH           DROP                    ; <word-addr> <len>
        FORTH           NUMBER                  ; <number> <chars left>
        FORTH           DUP                     ; <number> <chars left> <chars left>
        FORTH           ZEQU                    ; <number> <chars left> <chars left==0>
        FORTH0BRANCH    .parse_error            ; <number> <chars left>

        FORTH           DROP                    ; <number>
        FORTH           STATE, FETCH            ; <number> STATE
        FORTH0BRANCH    .number                 ; <number>

.compile_lit:
        FORTH           TICK, LIT, COMMA

.number:
        FORTH           BREAKPOINT
        FORTH           EXIT

.parse_error:
        FORTH           BREAKPOINT
        FORTHLITSTRING  'PARSE ERROR:\r\n'
        FORTH           TELL
        FORTHLIT        keybufptr               ; keybufptr
        FORTH           DUP                     ; keybufptr keybufptr
        FORTHLIT        keybuf                  ; keybufptr keybufptr keybuf
        FORTH           SUB                     ; keybufptr keybufptr-keybuf
        FORTHLIT        40                      ; keybufptr keybufptr-keybuf 40
        FORTH           MAX                     ; keybufptr n=max(keybufptr-keybuf, 40)
        FORTH           DUP                     ; keybufptr n n
        FORTH           ROT                     ; n keybufptr n
        FORTH           SUB                     ; n keybufptr-n
        FORTH           SWAP                    ; keybufptr-n n
        FORTH           TELL
        FORTHLITSTRING  '\r\n'
        FORTH           TELL
	FORTH           EXIT

; **************************************
; **************************************

	DEFCODE "CHAR",char
	call    _word
        push    @PSP, TOS
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

        ex      TMP, TOS

.done:
	NEXT

; **************************************

        ; n1 n2 -- min(n1, n2)
	DEFCODE 'MIN', min
        pop     TMP, @PSP
        cp      TOS, TMP
        jr      le, .done

        ex      TMP, TOS

.done:
	NEXT

; **************************************
; **************************************

keybuf                  db      128 dup(?)
keybufend               =       $
keybuflen               =       keybufend-keybuf
        align   2
keybufptr               dw      keybuf
keybuftop               dw      keybuf

wordbuf                 db      keybuflen dup(?)
wordbufend              =       $
wordbuflen              =       wordbufend-wordbuf
        align   2
wordbufptr              dw      wordbuf

emitbuf                 db      2 dup (?)
        align   2

LAST_lfa                =       LFA
FIRST_FREE              =       $


        end
