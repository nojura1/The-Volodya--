a[[[228]]]



while (i < 3 && (j = j + 1)) {}
return (i64)(x.y + f(1, 2)[i]);

ExprMul { ( "+" | "-" ) ExprMul } ; 
ExprMul      ::= ExprCast { ( "*" | "/" | "%" ) ExprCast } ;
ExprCast     ::= [ "(" Type ")" ] ExprUnary ;
ExprUnary    ::= ( "-" | "+" | "!" ) ExprUnary | ExprPostfix ;
ExprPostfix  ::= ExprPrimary { PostfixOp } ;
PostfixOp    ::= "[" Expr "]" | "(" [ List ] ")" | "." IDENT ;
List         ::= Expr { "," Expr } ;
ExprPrimary  ::= IDENT | IntLit | STRING_LIT | BOOL_LIT | ARRAY_LIT | "(" Expr ")" ;

"-" IDENT "*" IDENT "(" ")" ;