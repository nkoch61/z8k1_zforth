\	A sometimes minimal FORTH compiler and tutorial for Linux / i386 systems. -*- asm -*-
\	By Richard W.M. Jones <rich@annexia.org> http://annexia.org/forth
\	This is PUBLIC DOMAIN (see public domain release statement below).

-EXEC-TRACE
-ECHO-INIT

: / /MOD SWAP DROP ;
: MOD /MOD DROP ;

: '\n' 10 ;
: BL   32 ; \ BL (BLank) is a standard FORTH word for space.

: CR '\n' EMIT ;

: SPACE BL EMIT ;

: NEGATE 0 SWAP - ;

: TRUE  1 ;
: FALSE 0 ;
: NOT   0= ;

\ LITERAL takes whatever is on the stack and compiles LIT <foo>
: LITERAL IMMEDIATE
	' LIT ,
	,
	;

: ':' [ CHAR : ] LITERAL ;
: ';' [ CHAR ; ] LITERAL ;
: '(' [ CHAR ( ] LITERAL ;
: ')' [ CHAR ) ] LITERAL ;
: '"' [ CHAR " ] LITERAL ;
: 'A' [ CHAR A ] LITERAL ;
: '0' [ CHAR 0 ] LITERAL ;
: '-' [ CHAR - ] LITERAL ;
: '.' [ CHAR . ] LITERAL ;

: [COMPILE] IMMEDIATE
	WORD
	FIND
	>CFA
	,
;

: RECURSE IMMEDIATE
	LATEST @
	>CFA
	,
;

: IF IMMEDIATE
	' 0BRANCH ,
	HERE @
	0 ,
;

: THEN IMMEDIATE
	DUP
	HERE @ SWAP -
	SWAP !
;

: ELSE IMMEDIATE
	' BRANCH ,
	HERE @
	0 ,
	SWAP
	DUP
	HERE @ SWAP -
	SWAP !
;

: BEGIN IMMEDIATE
	HERE @
;

: UNTIL IMMEDIATE
	' 0BRANCH ,
	HERE @ -
	,
;

: AGAIN IMMEDIATE
	' BRANCH ,
	HERE @ -
	,
;

: WHILE IMMEDIATE
	' 0BRANCH ,
	HERE @
	0 ,
;

: REPEAT IMMEDIATE
	' BRANCH ,
	SWAP
	HERE @ - ,
	DUP
	HERE @ SWAP -
	SWAP !
;

: UNLESS IMMEDIATE
	' NOT ,
	[COMPILE] IF
;

: ( IMMEDIATE
	1
	BEGIN
		KEY
		DUP '(' = IF
			DROP
			1+
		ELSE
			')' = IF
				1-
			THEN
		THEN
	DUP 0= UNTIL
	DROP
;

: CELLS ( n -- n ) 2 * ;

: NIP ( x y -- y ) SWAP DROP ;
: TUCK ( x y -- y x y ) SWAP OVER ;
: PICK ( x_u ... x_1 x_0 u -- x_u ... x_1 x_0 x_u )
        1 +
	CELLS
	DSP@ +
	@
;

: SPACES	( n -- )
	BEGIN
		DUP 0>
	WHILE
		SPACE
		1-
	REPEAT
	DROP
;

: DECIMAL ( -- ) 10 BASE ! ;
: HEX ( -- ) 16 BASE ! ;

: U.		( u -- )
	BASE @ /MOD	( width rem quot )
	?DUP IF			( if quotient <> 0 then )
		RECURSE		( print the quotient )
	THEN

	( print the remainder )
	DUP 10 < IF
		'0'		( decimal digits 0..9 )
	ELSE
		10 -		( hex and beyond digits A..Z )
		'A'
	THEN
	+
	EMIT
;

: .S		( -- )
	DSP@		( get current stack pointer )
	BEGIN
		DUP S0 @ <
	WHILE
		DUP @ U.	( print the stack element )
		SPACE
		1 CELLS +	( move up )
	REPEAT
	DROP
;

: UWIDTH	( u -- width )
	BASE @ /	( rem quot )
	?DUP IF		( if quotient <> 0 then )
		RECURSE 1+	( return 1+recursive call )
	ELSE
		1		( return 1 )
	THEN
;

: U.R		( u width -- )
	SWAP		( width u )
	DUP		( width u u )
	UWIDTH		( width u uwidth )
	ROT		( u uwidth width )
	SWAP -		( u width-uwidth )
	SPACES
	U.
;

: .R		( n width -- )
	SWAP		( width n )
	DUP 0< IF
		NEGATE		( width u )
		1		( save a flag to remember that it was negative | width n 1 )
		SWAP		( width 1 u )
		ROT		( 1 u width )
		1-		( 1 u width-1 )
	ELSE
		0		( width u 0 )
		SWAP		( width 0 u )
		ROT		( 0 u width )
	THEN
	SWAP		( flag width u )
	DUP		( flag width u u )
	UWIDTH		( flag width u uwidth )
	ROT		( flag u uwidth width )
	SWAP -		( flag u width-uwidth )

	SPACES		( flag u )
	SWAP		( u flag )

	IF			( was it negative? print the - character )
		'-' EMIT
	THEN

	U.
;

: . 0 .R SPACE ;

: U. U. SPACE ;

: ? ( addr -- ) @ . ;

: WITHIN
	-ROT		( b c a )
	OVER		( b c a c )
	<= IF
		> IF		( b c -- )
			TRUE
		ELSE
			FALSE
		THEN
	ELSE
		2DROP		( b c -- )
		FALSE
	THEN
;

: DEPTH		( -- n )
	S0 @ DSP@ -
;

: ALIGNED	( addr -- addr )
	1 + 1 INVERT AND	( (addr+1) & ~1 )
;

: ALIGN HERE @ ALIGNED HERE ! ;

: C,
	HERE @ C!	( store the character in the compiled image )
	1 HERE +!	( increment HERE pointer by 1 byte )
;

: S" IMMEDIATE		( -- addr len )
	STATE @ IF	( compiling? )
		' LITSTRING ,	( compile LITSTRING )
		HERE @		( save the address of the length word on the stack )
		0 ,		( dummy length - we don't know what it is yet )
		BEGIN
			KEY 		( get next character of the string )
			DUP '"' <>
		WHILE
			C,		( copy character )
		REPEAT
		DROP		( drop the double quote character at the end )
		DUP		( get the saved address of the length word )
		HERE @ SWAP -	( calculate the length )
		1 CELLS -	( subtract 2 (because we measured from the start of the length word) )
		SWAP !		( and back-fill the length location )
		ALIGN		( round up to next multiple of 2 bytes for the remaining code )
	ELSE		( immediate mode )
		HERE @		( get the start address of the temporary space )
		BEGIN
			KEY
			DUP '"' <>
		WHILE
			OVER C!		( save next character )
			1+		( increment address )
		REPEAT
		DROP		( drop the final " character )
		HERE @ -	( calculate the length )
		HERE @		( push the start address )
		SWAP 		( addr len )
	THEN
;

: ." IMMEDIATE		( -- )
	STATE @ IF	( compiling? )
		[COMPILE] S"	( read the string, and compile LITSTRING, etc. )
		' TELL ,	( compile the final TELL )
	ELSE
		( In immediate mode, just read characters and print them until we get
		  to the ending double quote. )
		BEGIN
			KEY
			DUP '"' = IF
				DROP	( drop the double quote character )
				EXIT	( return from this function )
			THEN
			EMIT
		AGAIN
	THEN
;

: CONSTANT
	WORD		( get the name (the name follows CONSTANT) )
	CREATE		( make the dictionary entry )
	DOCON ,		( append DOCON )
	,		( append the value on the top of the stack )
	' EXIT ,	( append the codeword EXIT )
;

: ALLOT		( n -- addr )
	HERE @ SWAP	( here n )
	HERE +!		( adds n to HERE, after this the old value of HERE is still on the stack )
;

: VARIABLE
	WORD CREATE	( make the dictionary entry (the name follows VARIABLE) )
	DOVAR ,		( append DOVAR )
        0 ,
	' EXIT ,	( append the codeword EXIT )
;

: VALUE		( n -- )
	WORD CREATE	( make the dictionary entry (the name follows VALUE) )
	DOCON ,		( append DOCON )
	,		( append the initial value )
	' EXIT ,	( append the codeword EXIT )
;

: TO IMMEDIATE	( n -- )
	WORD		( get the name of the value )
	FIND		( look it up in the dictionary )
	>DFA		( get a pointer to the first data field (the 'LIT') )
	STATE @ IF	( compiling? )
		' LIT ,		( compile LIT )
		,		( compile the address of the value )
		' ! ,		( compile ! )
	ELSE		( immediate mode )
		!		( update it straightaway )
	THEN
;

( x +TO VAL adds x to VAL )
: +TO IMMEDIATE
	WORD		( get the name of the value )
	FIND		( look it up in the dictionary )
	>DFA		( get a pointer to the first data field (the 'LIT') )
	STATE @ IF	( compiling? )
		' LIT ,		( compile LIT )
		,		( compile the address of the value )
		' +! ,		( compile +! )
	ELSE		( immediate mode )
		+!		( update it straightaway )
	THEN
;

: ID.
	1 CELLS +	( skip over the link pointer )
	DUP C@		( get the flags/length byte )
	F_LENMASK AND	( mask out the flags - just want the length )

	BEGIN
		DUP 0>		( length > 0? )
	WHILE
		SWAP 1+		( addr len -- len addr+1 )
		DUP C@		( len addr -- len addr char | get the next character)
		EMIT		( len addr char -- len addr | and print it)
		SWAP 1-		( len addr -- addr len-1    | subtract one from length )
	REPEAT
	2DROP		( len addr -- )
;

: ?HIDDEN
	1 CELLS +	( skip over the link pointer )
	C@		( get the flags/length byte )
	F_HIDDEN AND	( mask the F_HIDDEN flag and return it (as a truth value) )
;

: ?IMMEDIATE
	1 CELLS +	( skip over the link pointer )
	C@		( get the flags/length byte )
	F_IMMED AND	( mask the F_IMMED flag and return it (as a truth value) )
;

: WORDS
	LATEST @	( start at LATEST dictionary entry )
	BEGIN
		?DUP		( while link pointer is not null )
	WHILE
		DUP ?HIDDEN NOT IF	( ignore hidden words )
			DUP ID.		( but if not hidden, print the word )
			SPACE
		THEN
		@		( dereference the link pointer - go to previous word )
	REPEAT
	CR
;

: FORGET
	WORD FIND	( find the word, gets the dictionary entry address )
	DUP @ LATEST !	( set LATEST to point to the previous word )
	HERE !		( and store HERE with the dictionary address )
;

: DUMP		( addr len -- )
	BASE @ -ROT		( save the current BASE at the bottom of the stack )
	HEX			( and switch to hexadecimal mode )

	BEGIN
		?DUP		( while len > 0 )
	WHILE
		OVER 8 U.R	( print the address )
		SPACE

		( print up to 16 words on this line )
		2DUP		( addr len addr len )
		1- 15 AND 1+	( addr len addr linelen )
		BEGIN
			?DUP		( while linelen > 0 )
		WHILE
			SWAP		( addr len linelen addr )
			DUP C@		( addr len linelen addr byte )
			2 .R SPACE	( print the byte )
			1+ SWAP 1-	( addr len linelen addr -- addr len addr+1 linelen-1 )
		REPEAT
		DROP		( addr len )

		( print the ASCII equivalents )
		2DUP 1- 15 AND 1+ ( addr len addr linelen )
		BEGIN
			?DUP		( while linelen > 0)
		WHILE
			SWAP		( addr len linelen addr )
			DUP C@		( addr len linelen addr byte )
			DUP 32 128 WITHIN IF	( 32 <= c < 128? )
				EMIT
			ELSE
				DROP '.' EMIT
			THEN
			1+ SWAP 1-	( addr len linelen addr -- addr len addr+1 linelen-1 )
		REPEAT
		DROP		( addr len )
		CR

		DUP 1- 15 AND 1+ ( addr len linelen )
		TUCK		( addr linelen len linelen )
		-		( addr linelen len-linelen )
		>R + R>		( addr+linelen len-linelen )
	REPEAT

	DROP			( restore stack )
	BASE !			( restore saved BASE )
;

: CASE IMMEDIATE
	0		( push 0 to mark the bottom of the stack )
;

: OF IMMEDIATE
	' OVER ,	( compile OVER )
	' = ,		( compile = )
	[COMPILE] IF	( compile IF )
	' DROP ,  	( compile DROP )
;

: ENDOF IMMEDIATE
	[COMPILE] ELSE	( ENDOF is the same as ELSE )
;

: ENDCASE IMMEDIATE
	' DROP ,	( compile DROP )

	( keep compiling THEN until we get to our zero marker )
	BEGIN
		?DUP
	WHILE
		[COMPILE] THEN
	REPEAT
;

: SEE
	WORD FIND	( find the dictionary entry to decompile )

	( Now we search again, looking for the next word in the dictionary.  This gives us
	  the length of the word that we will be decompiling.  (Well, mostly it does). )
	HERE @		( address of the end of the last compiled word )
	LATEST @	( word last curr )
	BEGIN
		2 PICK		( word last curr word )
		OVER		( word last curr word curr )
		<>		( word last curr word<>curr? )
	WHILE			( word last curr )
		NIP		( word curr )
		DUP @		( word curr prev (which becomes: word last curr) )
	REPEAT

	DROP		( at this point, the stack is: start-of-word end-of-word )
	SWAP		( end-of-word start-of-word )

	( begin the definition with : NAME [IMMEDIATE] )
	':' EMIT SPACE DUP ID. SPACE
	DUP ?IMMEDIATE IF ." IMMEDIATE " THEN

	>DFA		( get the data address, ie. points after DOCOL | end-of-word start-of-data )

	( now we start decompiling until we hit the end of the word )
	BEGIN		( end start )
		2DUP >
	WHILE
		DUP @		( end start codeword )

		CASE
		' LIT OF		( is it LIT ? )
			1 CELLS + DUP @		( get next word which is the integer constant )
			.			( and print it )
		ENDOF
		' LITSTRING OF		( is it LITSTRING ? )
			[ CHAR S ] LITERAL EMIT '"' EMIT SPACE ( print S"<space> )
			1 CELLS + DUP @		( get the length word )
			SWAP 1 CELLS + SWAP	( end start+2 length )
			2DUP TELL		( print the string )
			'"' EMIT SPACE		( finish the string with a final quote )
			+ ALIGNED		( end start+2+len, aligned )
			1 CELLS -		( because we're about to add 2 below )
		ENDOF
		' 0BRANCH OF    		( is it 0BRANCH ? )
			." 0BRANCH ( "
			1 CELLS + DUP @		( print the offset )
			.
			." ) "
		ENDOF
		' BRANCH OF		        ( is it BRANCH ? )
			." BRANCH ( "
			1 CELLS + DUP @		( print the offset )
			.
			." ) "
		ENDOF
		' ' OF			        ( is it ' (TICK) ? )
			[ CHAR ' ] LITERAL EMIT SPACE
			1 CELLS + DUP @		( get the next codeword )
			CFA>			( and force it to be printed as a dictionary entry )
			ID. SPACE
		ENDOF
		' EXIT OF		        ( is it EXIT? )
			( We expect the last word to be EXIT, and if it is then we don't print it
			  because EXIT is normally implied by ;.  EXIT can also appear in the middle
			  of words, and then it needs to be printed. )
			2DUP			( end start end start )
			1 CELLS +		( end start end start+2 )
			<> IF			( end start | we're not at the end )
				." EXIT "
			THEN
		ENDOF
					        ( default case: )
			DUP			( in the default case we always need to DUP before using )
			CFA>			( look up the codeword to get the dictionary entry )
			ID. SPACE		( and print it )
		ENDCASE

		1 CELLS +		        ( end start+2 )
	REPEAT

	';' EMIT CR

	2DROP		( restore stack )
;

: :NONAME
	0 0 CREATE	( create a word with no name - we need a dictionary header because ; expects it )
	HERE @		( current HERE value is the address of the codeword, ie. the xt )
	DOCOL ,		( compile DOCOL (the codeword) )
	]		( go into compile mode )
;

: ['] IMMEDIATE
	' LIT ,		( compile LIT )
;

: EXCEPTION-MARKER
	RDROP			( drop the original parameter stack pointer )
	0			( there was no exception, this is the normal return path )
;

: CATCH		( xt -- exn? )
	DSP@ 2+ >R
	' EXCEPTION-MARKER 2+
	>R
	EXECUTE
;

: THROW		( n -- )
	?DUP IF
		RSP@
		BEGIN
			DUP R0 2- <
		WHILE
			DUP @
			' EXCEPTION-MARKER 2+ = IF
				2+
				RSP!

				DUP DUP DUP
				R>
				2-
				SWAP OVER
				!
				DSP! EXIT
			THEN
			2+
		REPEAT

		DROP

		CASE
		0 1- OF	( ABORT )
			." ABORTED" CR
		ENDOF
			( default case )
			." UNCAUGHT THROW "
			DUP . CR
		ENDCASE
		QUIT
	THEN
;

: ABORT		( -- )
	0 1- THROW
;

: PRINT-STACK-TRACE
	RSP@		
	BEGIN
		DUP R0 2- <
	WHILE
		DUP @
		CASE
		' EXCEPTION-MARKER 2+ OF
			." CATCH ( DSP="
			2+ DUP @ U.
			." ) "
		ENDOF
			DUP
			CFA>
			?DUP IF
				2DUP
				ID.
				[ CHAR + ] LITERAL EMIT
				SWAP >DFA 2+ - .
			THEN
		ENDCASE
		2+
	REPEAT
	DROP
	CR
;

: UNUSED	( -- n )
	GET-BRK
	HERE @	
	-
	1 CELLS /
;


: WELCOME
        ." ZFORTH" CR
	UNUSED . ." CELLS REMAINING" CR CR
;

WELCOME
HIDE WELCOME
