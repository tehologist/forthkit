: + um+  drop   ; 
: cells  dup +  ; 
: cell+  2   +  ; 
: cell- -2   +  ; 
: cell   2      ; 
: boot     0 cells ; 
: forth    4 cells ; 
: dpl      5 cells ; 
: sp0      6 cells ; 
: rp0      7 cells ; 
: '?key    8 cells ; 
: 'emit    9 cells ; 
: 'expect 10 cells ; 
: 'tap    11 cells ; 
: 'echo   12 cells ; 
: 'prompt 13 cells ; 
: base    14 cells ; 
: tmp     15 cells ; 
: span    16 cells ; 
: >in     17 cells ; 
: #tibb   18 cells ; 
: tibb    19 cells ; 
: csp     20 cells ; 
: 'eval   21 cells ; 
: 'number 22 cells ; 
: hld     23 cells ; 
: handler 24 cells ; 
: context 25 cells ; 
: current 27 cells ; 
: cp      29 cells ; 
: np      30 cells ; 
: last    31 cells ; 
: state   32 cells ; 
: spp     33 cells ; 
: rpp     34 cells ; 
: true -1 ;  
: false 0 ; 
: bl 32 ; 
: bs 8 ; 
: =immed    3 ; 
: =wordlist 2 ; 
: immediate =immed last @ cell- ! ; 
: here cp @ ; 
: allot cp @ + cp ! ; 
: , here cell allot ! ;  
: c, here 1 allot c! ;  
: +! swap over @ + swap ! ; 
: compile r> dup @ , cell+ >r ; 
: state? state @ ; 
: literal compile lit , ; immediate 
: [ false state ! ; immediate 
: ] true state ! ; immediate 
: if compile 0branch here 0 , ; immediate 
: then here swap ! ; immediate 
: for compile >r here ; immediate 
: next compile next , ; immediate 
: begin here ; immediate 
: again compile branch , ; immediate 
: until compile 0branch , ; immediate 
: ahead compile branch here 0 , ; immediate 
: repeat compile branch , here swap ! ; immediate 
: aft drop compile branch here 0 ,  here swap ; immediate 
: else compile branch here 0 ,  swap here swap !  ; immediate 
: while compile 0branch here 0 , swap ; immediate 
: execute >r ; 
: @execute @ dup if execute then ; 
: r@ r> r> dup >r swap >r ; 
: #tib #tibb @ ; 
: tib tibb @ ;  
: \ #tib @ >in ! ; immediate 
: rot >r swap r> swap ; 
: -rot swap >r swap r> ; 
: nip swap drop ; 
: tuck swap over ; 
: 2>r swap r> swap >r swap >r >r ; 
: 2r> r> r> swap r> swap >r swap ; 
: 2r@ r> r> r@ swap >r swap r@ swap >r ; 
: 2drop drop drop ; 
: 2dup over over ; 
: 2swap rot >r rot r> ; 
: 2over >r >r 2dup r> r> 2swap ; 
: 2rot 2>r 2swap 2r> 2swap ; 
: -2rot 2rot 2rot ; 
: 2nip 2swap 2drop ; 
: 2tuck 2swap 2over ; 
: not dup nand ; 
: and nand not ; 
: or not swap not nand ; 
: nor or not ; 
: xor 2dup and -rot nor nor ; 
: xnor xor not ; 
: negate not 1 + ; 
: - negate + ; 
: 1+ 1 + ; 
: 1- 1 - ; 
: 2+ 2 + ; 
: 2- 2 - ; 
: d+ >r swap >r um+ r> r> + + ; 
: dnegate not >r not 1 um+ r> + ; 
: d- dnegate d+ ; 
: 2! swap over ! cell+ ! ; 
: 2@ dup cell+ @ swap @ ; 
: ?dup dup if dup then ; 
: s>d dup 0< ; 
: abs dup 0< if negate then ; 
: dabs dup 0< if dnegate then ; 
: u< 2dup xor 0< if swap drop 0< exit then - 0< ; 
: u> swap u< ; 
: = xor if false exit then true ; 
: < 2dup xor 0< if drop 0< exit then - 0< ; 
: > swap < ; 
: 0> negate 0< ; 
: 0<> if true exit then false ; 
: 0= 0 = ; 
: <> = 0= ; 
: d0< swap drop 0< ; 
: d0> dnegate d0< ; 
: d0= or 0= ; 
: d= d- d0= ; 
: d< rot 2dup xor if swap 2swap 2drop < ; 
: du< rot 2dup xor if swap 2swap then then 2drop u< ; 
: dmin 2over 2over 2swap d< if 2swap then 2drop ; 
: dmax 2over 2over d< if 2swap then 2drop ; 
: m+ s>d d+ ; 
: m- s>d d- ; 
: min 2dup swap < if swap then drop ; 
: max 2dup < if swap then drop ; 
: umin 2dup swap u< if swap then drop ; 
: umax 2dup u< if swap then drop ; 
: within over - >r - r> u< ; 
: um/mod 
    2dup u< 
    if negate 
        15 for 
            >r dup um+ 
                >r >r dup um+ 
                r> + dup r> r@ swap 
            >r um+ 
            r> or 
                if >r drop 1+ r> 
                else drop 
            then r> 
        next drop swap exit 
    then drop 2drop -1 dup ; 
: m/mod 
    dup 0< dup >r 
        if negate >r 
            dnegate r> 
        then >r dup 0< 
        if r@ + 
        then r> um/mod 
    r> 
    if swap negate swap then ; 
: /mod over 0< swap m/mod ; 
: mod /mod drop ; 
: / /mod nip ; 
: um* 
    0 swap 
    15 for 
        dup um+ >r >r 
        dup um+ 
        r> + 
        r> 
        if >r over um+ 
            r> + 
        then 
    next 
    rot drop ; 
: * um* drop ; 
: m* 
    2dup xor 0< >r 
    abs swap abs um* 
    r> if dnegate then ; 
: */mod >r m* r> m/mod ; 
: */ */mod swap drop ; 
: 2* 2 * ; 
: 2/ 2 / ; 
: mu/mod >r 0 r@ um/mod r> swap >r um/mod r> ; 
: d2* 2dup d+ ; 
: du2/ 2 mu/mod rot drop ; 
: d2/ dup >r 1 and du2/ r> 2/ or ; 
: aligned dup 0 2 um/mod drop dup if 2 swap - then + ; 
: parse 
    tmp ! over >r dup 
    if 
        1- tmp @ bl = 
        if 
            for bl over c@ - 0< not 
            while 1+ 
                next r> drop 0 dup exit 
            then r> 
        then 
            over swap 
        for tmp @ over c@ - tmp @ bl = 
            if 0< then 
        while 1+ 
        next dup >r 
        else r> drop dup 1+ >r 
        then over - r> r> - exit 
    then over r> - ; 
: parse >r tib >in @ + #tib c@ >in @ - r> parse >in +! ; 
: char bl parse drop c@ ; 
: tx! 1 putc ; 
: emit 'emit @execute ; 
: type for aft dup c@ emit 1+ then next drop ; 
: ?rx 0 getc ; 
: ?key '?key @execute ; 
: key begin ?key until ; 
: count dup 1+ swap c@ ; 
: cmove 
    for 
        aft 
            >r dup c@ r@ c! 1+ r> 1+ 
        then 
    next 2drop ; 
: fill 
    swap 
    for swap 
        aft 2dup c! 1+ then 
    next 2drop ; 
: -trailing 
    for 
        aft 
        bl over r@ + c@ < 
        if 
            r> 1+ exit 
        then 
    then 
    next 0 ; 
: pack$ 
    dup >r 
        2dup c! 1+ 2dup + 0 swap ! swap cmove 
    r> ;  
: word parse here pack$ ; 
: token bl parse 31 min np @ over - 1- pack$ ; 
: link> 3 cells - ; 
: code> 2 cells - ; 
: type> 1 cells - ; 
: data> cell+ ; 
: same? 
    for aft 
        over r@ cells + @ 
        over r@ cells + @ 
        - ?dup 
        if r> drop exit then 
    then 
    next 0 ; 
: find 
    @ begin dup while 
        2dup c@ swap c@ = if 
        2dup 1+ swap count aligned cell / >r swap r>  
        same? 0= if 
            2drop swap drop dup code> @ swap -1 exit  
        then 2drop then 
    link> @ repeat ; 
: ' token context @ find if drop else swap drop 0 then ; 
: ! ! ; 
' tx! 'emit ! 
' ?rx '?key ! 
: ['] compile ' ; immediate 
: postpone ' , ; immediate 
: [char] char postpone literal ; immediate 
: ( [char] ) parse 2drop ; immediate 
: :noname here postpone ] ; 
: overt last @ current @ ! ; 
: $,n 
    dup last ! cell- 
    dup =wordlist 
    swap ! 
    cell- dup here 
    swap ! 
    cell- dup current @ @ 
    swap ! 
    cell- np ! ; 
: : token $,n postpone ] ; 
: ; compile exit postpone [ overt ; immediate 
: recurse last @ code> @ , ; immediate 
: dovar r> ; 
: create token $,n compile dovar overt ; 
: does last @ code> @ r> swap ! ; 
: does> compile does compile r> ; immediate 
: constant create , does> @ ; 
: variable create 0 , ; 
: 2literal swap postpone literal 
    postpone literal ; immediate 
: 2constant create , , does> 2@ ; 
: 2variable create 2 cells allot ; 
: space bl emit ; 
: spaces 0 max for space next ; 
: pad here 80 + ;  
: decimal 10 base ! ; 
: hex 16 base ! ; 
: binary 2 base ! ; 
: octal 8 base ! ; 
decimal 
: char- 1- ; 
: char+ 1+ ; 
: chars ; 
: >char 127 and dup 127 bl within if drop 95 then ; 
: digit 9 over < 7 and + [char] 0 + ; 
: <# pad hld ! ; 
: hold hld @ char- dup hld ! c! ; 
: # 0 base @ um/mod >r base @ um/mod swap digit hold r> ; 
: #s begin # 2dup or 0= until ; 
: sign 0< if [char] - hold then ; 
: #> 2drop hld @ pad over - ; 
: s.r over - spaces type ; 
: d.r >r dup >r dabs <# #s r> sign #> r> s.r ; 
: u.r 0 swap d.r ; 
: .r >r s>d r> d.r ; 
: d. 0 d.r space ; 
: u. 0 d. ;  
: . base @ 10 xor if u. exit then s>d d. ; 
: ? @ . ; 
: du.r >r <# #s #> r> s.r ; 
: du. du.r space ; 
: do$ r> r@ r> count + aligned >r swap >r ; 
: ."| do$ count type ; 
: $," [char] " word count + aligned cp ! ; 
: ." compile ."| $," ; immediate 
: .( [char] ) parse type ; immediate 
: $"| do$ ; 
: $" compile $"| $," ; immediate 
: s" [char] " parse here pack$ ; 
: cr 10 emit ;  
: tap over c! 1+ ; 
: ktap 
    10 xor 
    if 
        bl tap exit 
    then 
    nip dup ; 
: accept 
    over + over 
    begin 
        2dup xor 
    while 
        key 
        dup bl - 95 u< 
        if tap else ktap then 
    repeat drop over - ; 
: expect accept span ! drop ; 
: query tib 80 accept #tib c! drop 0 >in ! ; 
: digit? 
    >r [char] 0 - 
    9 over < 
    if 7 - dup 10 < or then 
    dup r> u< ; 
: /string dup >r - swap r> + swap ; 
: >number 
    begin dup 
    while >r dup >r c@ base @ digit? 
    while swap base @ um* drop rot 
    base @ um* d+ r> char+ r> 1 - 
    repeat drop r> r> then ; 
: number? 
    over c@ [char] - = dup >r if 1 /string then 
    >r >r 0 dup r> r> -1 dpl ! 
    begin >number dup 
    while over c@ [char] . xor 
        if rot drop rot r> 2drop false exit 
    then 1 - dpl ! char+ dpl @ 
    repeat 2drop r> if dnegate then true ; 
' number? 'number ! 
: $interpret 
    context @ find 
    if drop execute exit then 
    count 'number @execute if 
        dpl @ 0< if drop then exit then ." ?" type ; 
: $compile 
    context @ find 
    if cell- @ =immed = 
        if execute else , then exit 
    then count 'number @execute 
    if 
        dpl @ 0< 
        if drop postpone literal 
        else postpone 2literal 
        then exit 
    then ." ?" type ; 
: eval state? if $compile else $interpret then ; 
' eval 'eval ! 
: eval 
    begin token dup c@ while 
        'eval @execute 
    repeat drop ; 
: ok cr ." ok." space ; 
' ok 'prompt ! 
: quit 
    begin 'prompt @execute query 
    eval again ; 
: bye ." good bye " cr halt ; 
' quit boot ! 
: sp@ spp @ ; 
: depth sp@ sp0 @ - cell / ; 
: pick cells sp@ swap - 2 cells - @ ; 
: .s cr depth for aft r@ pick . then next space ." <sp " cr ; 
: words 
    context @ @ begin dup while dup count type link> 
    @ space repeat drop cr ;  
    
: rpick r> rpp @ swap >r + 2- @ ; 
: do compile 2>r postpone begin ; immediate 
: _loop r> r> 1+ dup r@ 1- > swap >r swap >r ; 
: _+loop r> swap r> swap + dup r@ 1- > swap >r swap >r ; 
: leave r> 2r> 2drop >r ; 
: unloop r> r> drop r@ r> r> ; 
: loop compile _loop postpone until compile leave ; immediate 
: +loop compile _+loop postpone until compile leave ; immediate 
: i r> r> tuck >r >r ; 
: j 4 rpick ; 
: k 4 rpick ;

variable file 
: open f_open file ! ; 
: close file @ f_close ; 
: fput file @ putc ; 
: fget file @ getc ; 
: fputs count for aft dup c@ fput 1+ then next drop ; 

: savevm $" eforth.img" $" wb" open 0 
    16384 for aft dup c@ fput 1+ then next close drop ; 

savevm 
