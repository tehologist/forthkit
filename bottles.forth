: MANY ?DUP IF . ELSE ." no more " THEN ; 
: BOTTLES ." bottle" 1- IF ." s" THEN ;
: BEER CR DUP MANY BOTTLES SPACE ." of beer" ;
: WALL SPACE ." on the wall." ;
: DRINK CR ." Take one down and pass it around." ;
: BUY CR ." Go to the store and buy some more. " ;
: ANOTHER ?DUP IF DRINK 1- ELSE BUY 99 THEN ; 
: VERSE DUP BEER WALL DUP BEER ANOTHER BEER WALL CR ;
: VERSES FOR R@ VERSE NEXT ;
99 VERSES
BYE   

