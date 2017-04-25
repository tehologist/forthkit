: + UM+  DROP   ; 
: CELLS  DUP +  ; 
: CELL+  2   +  ; 
: CELL- -2   +  ; 
: CELL   2      ; 
: BOOT     0 CELLS ; 
: FORTH    4 CELLS ; 
: DPL      5 CELLS ; 
: SP0      6 CELLS ; 
: RP0      7 CELLS ; 
: '?KEY    8 CELLS ; 
: 'EMIT    9 CELLS ; 
: 'EXPECT 10 CELLS ; 
: 'TAP    11 CELLS ; 
: 'ECHO   12 CELLS ; 
: 'PROMPT 13 CELLS ; 
: BASE    14 CELLS ; 
: tmp     15 CELLS ; 
: SPAN    16 CELLS ; 
: >IN     17 CELLS ; 
: #TIBB   18 CELLS ; 
: TIBB    19 CELLS ; 
: CSP     20 CELLS ; 
: 'EVAL   21 CELLS ; 
: 'NUMBER 22 CELLS ; 
: HLD     23 CELLS ; 
: HANDLER 24 CELLS ; 
: CONTEXT 25 CELLS ; 
: CURRENT 27 CELLS ; 
: CP      29 CELLS ; 
: NP      30 CELLS ; 
: LAST    31 CELLS ; 
: STATE   32 CELLS ; 
: SPP     33 CELLS ; 
: RPP     34 CELLS ; 
: TRUE -1 ;  
: FALSE 0 ; 
: BL 32 ; 
: BS 8 ; 
: =IMMED    3 ; 
: =WORDLIST 2 ; 
: IMMEDIATE =IMMED LAST @ CELL- ! ; 
: HERE CP @ ; 
: ALLOT CP @ + CP ! ; 
: , HERE CELL ALLOT ! ;  
: C, HERE 1 ALLOT C! ;  
: +! SWAP OVER @ + SWAP ! ; 
: COMPILE R> DUP @ , CELL+ >R ; 
: STATE? STATE @ ; 
: LITERAL COMPILE LIT , ; IMMEDIATE 
: [ FALSE STATE ! ; IMMEDIATE 
: ] TRUE STATE ! ; IMMEDIATE 
: IF COMPILE 0BRANCH HERE 0 , ; IMMEDIATE 
: THEN HERE SWAP ! ; IMMEDIATE 
: FOR COMPILE >R HERE ; IMMEDIATE 
: NEXT COMPILE next , ; IMMEDIATE 
: BEGIN HERE ; IMMEDIATE 
: AGAIN COMPILE BRANCH , ; IMMEDIATE 
: UNTIL COMPILE 0BRANCH , ; IMMEDIATE 
: AHEAD COMPILE BRANCH HERE 0 , ; IMMEDIATE 
: REPEAT COMPILE BRANCH , HERE SWAP ! ; IMMEDIATE 
: AFT DROP COMPILE BRANCH HERE 0 ,  HERE SWAP ; IMMEDIATE 
: ELSE COMPILE BRANCH HERE 0 ,  SWAP HERE SWAP !  ; IMMEDIATE 
: WHILE COMPILE 0BRANCH HERE 0 , SWAP ; IMMEDIATE 
: EXECUTE >R ; 
: @EXECUTE @ DUP IF EXECUTE THEN ; 
: R@ R> R> DUP >R SWAP >R ; 
: #TIB #TIBB @ ; 
: TIB TIBB @ ;  
: \ #TIB @ >IN ! ; IMMEDIATE 
: ROT >R SWAP R> SWAP ; 
: -ROT SWAP >R SWAP R> ; 
: NIP SWAP DROP ; 
: TUCK SWAP OVER ; 
: 2>R SWAP R> SWAP >R SWAP >R >R ; 
: 2R> R> R> SWAP R> SWAP >R SWAP ; 
: 2R@ R> R> R@ SWAP >R SWAP R@ SWAP >R ; 
: 2DROP DROP DROP ; 
: 2DUP OVER OVER ; 
: 2SWAP ROT >R ROT R> ; 
: 2OVER >R >R 2DUP R> R> 2SWAP ; 
: 2ROT 2>R 2SWAP 2R> 2SWAP ; 
: -2ROT 2ROT 2ROT ; 
: 2NIP 2SWAP 2DROP ; 
: 2TUCK 2SWAP 2OVER ; 
: NOT DUP NAND ; 
: AND NAND NOT ; 
: OR NOT SWAP NOT NAND ; 
: NOR OR NOT ; 
: XOR 2DUP AND -ROT NOR NOR ; 
: XNOR XOR NOT ; 
: NEGATE NOT 1 + ; 
: - NEGATE + ; 
: 1+ 1 + ; 
: 1- 1 - ; 
: 2+ 2 + ; 
: 2- 2 - ; 
: D+ >R SWAP >R UM+ R> R> + + ; 
: DNEGATE NOT >R NOT 1 UM+ R> + ; 
: D- DNEGATE D+ ; 
: 2! SWAP OVER ! CELL+ ! ; 
: 2@ DUP CELL+ @ SWAP @ ; 
: ?DUP DUP IF DUP THEN ; 
: S>D DUP 0< ; 
: ABS DUP 0< IF NEGATE THEN ; 
: DABS DUP 0< IF DNEGATE THEN ; 
: U< 2DUP XOR 0< IF SWAP DROP 0< EXIT THEN - 0< ; 
: U> SWAP U< ; 
: = XOR IF FALSE EXIT THEN TRUE ; 
: < 2DUP XOR 0< IF DROP 0< EXIT THEN - 0< ; 
: > SWAP < ; 
: 0> NEGATE 0< ; 
: 0<> IF TRUE EXIT THEN FALSE ; 
: 0= 0 = ; 
: <> = 0= ; 
: D0< SWAP DROP 0< ; 
: D0> DNEGATE D0< ; 
: D0= OR 0= ; 
: D= D- D0= ; 
: D< ROT 2DUP XOR IF SWAP 2SWAP 2DROP < ; 
: DU< ROT 2DUP XOR IF SWAP 2SWAP THEN THEN 2DROP U< ; 
: DMIN 2OVER 2OVER 2SWAP D< IF 2SWAP THEN 2DROP ; 
: DMAX 2OVER 2OVER D< IF 2SWAP THEN 2DROP ; 
: M+ S>D D+ ; 
: M- S>D D- ; 
: MIN 2DUP SWAP < IF SWAP THEN DROP ; 
: MAX 2DUP < IF SWAP THEN DROP ; 
: UMIN 2DUP SWAP U< IF SWAP THEN DROP ; 
: UMAX 2DUP U< IF SWAP THEN DROP ; 
: WITHIN OVER - >R - R> U< ; 
: UM/MOD 
    2DUP U< 
    IF NEGATE 
        15 FOR 
            >R DUP UM+ 
                >R >R DUP UM+ 
                R> + DUP R> R@ SWAP 
            >R UM+ 
            R> OR 
                IF >R DROP 1+ R> 
                ELSE DROP 
            THEN R> 
        NEXT DROP SWAP EXIT 
    THEN DROP 2DROP -1 DUP ; 
: M/MOD 
    DUP 0< DUP >R 
        IF NEGATE >R 
            DNEGATE R> 
        THEN >R DUP 0< 
        IF R@ + 
        THEN R> UM/MOD 
    R> 
    IF SWAP NEGATE SWAP THEN ; 
: /MOD OVER 0< SWAP M/MOD ; 
: MOD /MOD DROP ; 
: / /MOD NIP ; 
: UM* 
    0 SWAP 
    15 FOR 
        DUP UM+ >R >R 
        DUP UM+ 
        R> + 
        R> 
        IF >R OVER UM+ 
            R> + 
        THEN 
    NEXT 
    ROT DROP ; 
: * UM* DROP ; 
: M* 
    2DUP XOR 0< >R 
    ABS SWAP ABS UM* 
    R> IF DNEGATE THEN ; 
: */MOD >R M* R> M/MOD ; 
: */ */MOD SWAP DROP ; 
: 2* 2 * ; 
: 2/ 2 / ; 
: MU/MOD >R 0 R@ UM/MOD R> SWAP >R UM/MOD R> ; 
: D2* 2DUP D+ ; 
: DU2/ 2 MU/MOD ROT DROP ; 
: D2/ DUP >R 1 AND DU2/ R> 2/ OR ; 
: ALIGNED DUP 0 2 UM/MOD DROP DUP IF 2 SWAP - THEN + ; 
: parse 
    tmp ! OVER >R DUP 
    IF 
        1- tmp @ BL = 
        IF 
            FOR BL OVER C@ - 0< NOT 
            WHILE 1+ 
                NEXT R> DROP 0 DUP EXIT 
            THEN R> 
        THEN 
            OVER SWAP 
        FOR tmp @ OVER C@ - tmp @ BL = 
            IF 0< THEN 
        WHILE 1+ 
        NEXT DUP >R 
        ELSE R> DROP DUP 1+ >R 
        THEN OVER - R> R> - EXIT 
    THEN OVER R> - ; 
: PARSE >R TIB >IN @ + #TIB C@ >IN @ - R> parse >IN +! ; 
: CHAR BL PARSE DROP C@ ; 
: TX! 1 PUTC ; 
: EMIT 'EMIT @EXECUTE ; 
: TYPE FOR AFT DUP C@ EMIT 1+ THEN NEXT DROP ; 
: ?RX 0 GETC ; 
: ?KEY '?KEY @EXECUTE ; 
: KEY BEGIN ?KEY UNTIL ; 
: COUNT DUP 1+ SWAP C@ ; 
: CMOVE 
    FOR 
        AFT 
            >R DUP C@ R@ C! 1+ R> 1+ 
        THEN 
    NEXT 2DROP ; 
: FILL 
    SWAP 
    FOR SWAP 
        AFT 2DUP C! 1+ THEN 
    NEXT 2DROP ; 
: -TRAILING 
    FOR 
        AFT 
        BL OVER R@ + C@ < 
        IF 
            R> 1+ EXIT 
        THEN 
    THEN 
    NEXT 0 ; 
: PACK$ 
    DUP >R 
        2DUP C! 1+ 2DUP + 0 SWAP ! SWAP CMOVE 
    R> ;  
: WORD PARSE HERE PACK$ ; 
: TOKEN BL PARSE 31 MIN NP @ OVER - 1- PACK$ ; 
: LINK> 3 CELLS - ; 
: CODE> 2 CELLS - ; 
: TYPE> 1 CELLS - ; 
: DATA> CELL+ ; 
: SAME? 
    FOR AFT 
        OVER R@ CELLS + @ 
        OVER R@ CELLS + @ 
        - ?DUP 
        IF R> DROP EXIT THEN 
    THEN 
    NEXT 0 ; 
: find 
    @ BEGIN DUP WHILE 
        2DUP C@ SWAP C@ = IF 
        2DUP 1+ SWAP COUNT ALIGNED CELL / >R SWAP R>  
        SAME? 0= IF 
            2DROP SWAP DROP DUP CODE> @ SWAP -1 EXIT  
        THEN 2DROP THEN 
    LINK> @ REPEAT ; 
: ' TOKEN CONTEXT @ find IF DROP ELSE SWAP DROP 0 THEN ; 
: ! ! ; 
' TX! 'EMIT ! 
' ?RX '?KEY ! 
: ['] COMPILE ' ; IMMEDIATE 
: POSTPONE ' , ; IMMEDIATE 
: [CHAR] CHAR POSTPONE LITERAL ; IMMEDIATE 
: ( [CHAR] ) PARSE 2DROP ; IMMEDIATE 
: :NONAME HERE POSTPONE ] ; 
: OVERT LAST @ CURRENT @ ! ; 
: $,n 
    DUP LAST ! CELL- 
    DUP =WORDLIST 
    SWAP ! 
    CELL- DUP HERE 
    SWAP ! 
    CELL- DUP CURRENT @ @ 
    SWAP ! 
    CELL- NP ! ; 
: : TOKEN $,n POSTPONE ] ; 
: ; COMPILE EXIT POSTPONE [ OVERT ; IMMEDIATE 
: RECURSE LAST @ CURRENT @ ! ; IMMEDIATE 
: doVAR R> ; 
: CREATE TOKEN $,n COMPILE doVAR OVERT ; 
: DOES LAST @ CODE> @ R> SWAP ! ; 
: DOES> COMPILE DOES COMPILE R> ; IMMEDIATE 
: CONSTANT CREATE , DOES> @ ; 
: VARIABLE CREATE 0 , ; 
: 2LITERAL SWAP POSTPONE LITERAL 
    POSTPONE LITERAL ; IMMEDIATE 
: 2CONSTANT CREATE , , DOES> 2@ ; 
: 2VARIABLE CREATE 2 CELLS ALLOT ; 
: SPACE BL EMIT ; 
: SPACES 0 MAX FOR SPACE NEXT ; 
: PAD HERE 80 + ;  
: DECIMAL 10 BASE ! ; 
: HEX 16 BASE ! ; 
: BINARY 2 BASE ! ; 
: OCTAL 8 BASE ! ; 
DECIMAL 
: CHAR- 1- ; 
: CHAR+ 1+ ; 
: CHARS ; 
: >CHAR 127 AND DUP 127 BL WITHIN IF DROP 95 THEN ; 
: DIGIT 9 OVER < 7 AND + [CHAR] 0 + ; 
: <# PAD HLD ! ; 
: HOLD HLD @ CHAR- DUP HLD ! C! ; 
: # 0 BASE @ UM/MOD >R BASE @ UM/MOD SWAP DIGIT HOLD R> ; 
: #S BEGIN # 2DUP OR 0= UNTIL ; 
: SIGN 0< IF [CHAR] - HOLD THEN ; 
: #> 2DROP HLD @ PAD OVER - ; 
: S.R OVER - SPACES TYPE ; 
: D.R >R DUP >R DABS <# #S R> SIGN #> R> S.R ; 
: U.R 0 SWAP D.R ; 
: .R >R S>D R> D.R ; 
: D. 0 D.R SPACE ; 
: U. 0 D. ;  
: . BASE @ 10 XOR IF U. EXIT THEN S>D D. ; 
: ? @ . ; 
: DU.R >R <# #S #> R> S.R ; 
: DU. DU.R SPACE ; 
: do$ R> R@ R> COUNT + ALIGNED >R SWAP >R ; 
: ."| do$ COUNT TYPE ; 
: $," [CHAR] " WORD COUNT + ALIGNED CP ! ; 
: ." COMPILE ."| $," ; IMMEDIATE 
: .( [CHAR] ) PARSE TYPE ; IMMEDIATE 
: $"| do$ ; 
: $" COMPILE $"| $," ; IMMEDIATE 
: s" [CHAR] " PARSE HERE PACK$ ; 
: CR 10 EMIT ;  
: TAP OVER C! 1+ ; 
: KTAP 
    10 XOR 
    IF 
        BL TAP EXIT 
    THEN 
    NIP DUP ; 
: ACCEPT 
    OVER + OVER 
    BEGIN 
        2DUP XOR 
    WHILE 
        KEY 
        DUP BL - 95 U< 
        IF TAP ELSE KTAP THEN 
    REPEAT DROP OVER - ; 
: EXPECT ACCEPT SPAN ! DROP ; 
: QUERY TIB 80 ACCEPT #TIB C! DROP 0 >IN ! ; 
: DIGIT? 
    >R [CHAR] 0 - 
    9 OVER < 
    IF 7 - DUP 10 < OR THEN 
    DUP R> U< ; 
: /STRING DUP >R - SWAP R> + SWAP ; 
: >NUMBER 
    BEGIN DUP 
    WHILE >R DUP >R C@ BASE @ DIGIT? 
    WHILE SWAP BASE @ UM* DROP ROT 
    BASE @ UM* D+ R> CHAR+ R> 1 - 
    REPEAT DROP R> R> THEN ; 
: NUMBER? 
    OVER C@ [CHAR] - = DUP >R IF 1 /STRING THEN 
    >R >R 0 DUP R> R> -1 DPL ! 
    BEGIN >NUMBER DUP 
    WHILE OVER C@ [CHAR] . XOR 
        IF ROT DROP ROT R> 2DROP FALSE EXIT 
    THEN 1 - DPL ! CHAR+ DPL @ 
    REPEAT 2DROP R> IF DNEGATE THEN TRUE ; 
' NUMBER? 'NUMBER ! 
: $INTERPRET 
    CONTEXT @ find 
    IF DROP EXECUTE EXIT THEN 
    COUNT 'NUMBER @EXECUTE IF 
        DPL @ 0< IF DROP THEN EXIT THEN ." ?" TYPE ; 
: $COMPILE 
    CONTEXT @ find 
    IF CELL- @ =IMMED = 
        IF EXECUTE ELSE , THEN EXIT 
    THEN COUNT 'NUMBER @EXECUTE 
    IF 
        DPL @ 0< 
        IF DROP POSTPONE LITERAL 
        ELSE POSTPONE 2LITERAL 
        THEN EXIT 
    THEN ." ?" TYPE ; 
: eval STATE? IF $COMPILE ELSE $INTERPRET THEN ; 
' eval 'EVAL ! 
: EVAL 
    BEGIN TOKEN DUP C@ WHILE 
        'EVAL @EXECUTE 
    REPEAT DROP ; 
: OK CR ." OK." SPACE ; 
' OK 'PROMPT ! 
: QUIT 
    BEGIN 'PROMPT @EXECUTE QUERY 
    EVAL AGAIN ; 
: BYE ." Good Bye " CR HALT ; 
' QUIT BOOT ! 
: SP@ SPP @ ; 
: DEPTH SP@ SP0 @ - CELL / ; 
: PICK CELLS SP@ SWAP - 2 CELLS - @ ; 
: .S CR DEPTH FOR AFT R@ PICK . THEN NEXT SPACE ." <SP " CR ; 
: WORDS 
    CONTEXT @ @ BEGIN DUP WHILE DUP COUNT TYPE LINK> 
    @ SPACE REPEAT DROP CR ;  

VARIABLE FILE 
: OPEN F_OPEN FILE ! ; 
: CLOSE FILE @ F_CLOSE ; 
: FPUT FILE @ PUTC ; 
: FGET FILE @ GETC ; 
: FPUTS COUNT FOR AFT DUP C@ FPUT 1+ THEN NEXT DROP ; 

: SAVEVM $" eforth.img" $" wb" OPEN 0 
    16384 FOR AFT DUP C@ FPUT 1+ THEN NEXT CLOSE DROP ; 

SAVEVM 

