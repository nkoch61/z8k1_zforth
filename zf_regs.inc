; zf_regs.inc


; *******************************************************
; *     r0              rr0             rq0             *
; *     r1                                              *
; *     r2              rr2                             *
; *     r3                                              *
; *     r4              rr4             rq4             *
; *     r5                                              *
; *     r6              rr6                             *
; *     r7                                              *
; *     r8              rr8             rq8             *
; *     r9                                              *
; *     r10             rr10                            *
; *     r11                                             *
; *     r12             rr12            rq12            *
; *     r13                                             *
; *     r14             rr14                            *
; *     r15                                             *
; *******************************************************

RSP     reg     r15
PSP     reg     r14
W       reg     r13
IP      reg     r12
UP      reg     r11
X       reg     r10

TOS.LB  reg     rl7
TOS.HB  reg     rh7
TOS     reg     r7
TOS.HW  reg     r6
TOS.D   reg     rr6
TMP.LB  reg     rl5
TMP.HB  reg     rh5
TMP     reg     r5
TMP.HW  reg     r4
TMP.D   reg     rr4
TMP2    reg     r3
TMP3    reg     r2
TMP4    reg     r1
TMP5    reg     r0
