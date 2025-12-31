: DOUBLE ( n -- n ) 2 * ;
: QUADRUPLE ( n -- n ) DOUBLE DOUBLE ;
: HALF ( n -- n ) 2 / ;
: FOURTH ( n -- n ) HALF HALF ;
1 DOUBLE ( 2 ) DOUBLE ( 4 ) DUP . HALF ( 2 ) DUP . FOURTH ( 0 from truncation ) .
