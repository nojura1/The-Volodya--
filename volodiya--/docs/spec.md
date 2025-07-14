# Specification for the Volodya-- programming language

## 1. Introduction
The Volodya-- programming language is an imperative and procedural (possibly object-oriented?) C-like programming language oriented toward (systems programming?). It is designed to be simple enough, but encourages disciplined programming. The main purpose is to explore the behavior of programming languages more deeply and fully understand all the nuances of compiler building. The secondary purpose is to write a small (program?) using it. The Volodya-- programming language was inspired by C (but organized rather differently?).

The Volodya-- programming language is statically, strongly, nominally, and explicitly typed. Casting has to be explicit both ways. Compilation consists of translating source code into assembly. (Polymorphism?)

The Volodya-- programming language is a relatively low-level language. It includes manual memory management with a del command.

## 2. Lexical structure
| Categories     | Examples                                                                            
|----------------|----------------------------------------------------------------------------------------------------------------
| Keywords       | `fn`, `if`, `else`, `while`, `return`, `struct`, `i32`, `i64`, `u32`, `u64`, `bool`, `string`, `break`, 
|                | `const`, `for`, `voloid` (`continue`, `b32`, `b64`, `char`, `switch`, `case`)
| Operators      | `+ − * / %`, `&& || !`, `== != < > <= >=`, `=` (`| & ~ ^`)                  
| Separators     | `(` `)` `{` `}` `[` `]` `,` `.` `:` `;`                                                         
| Literals       | integers `123`, booleans `true/false`, strings `"text"`, escape sequences `\"`
|                | (characters `'a'`, floating point numbers `1.23`) 
| Identifiers    | `[A-Za-z_][A-Za-z0-9_]*`                                             

## 3. Types
| Type     | Description                    | Size     |
|----------|--------------------------------|----------|
| `i32`    | 32-bit signed integer          | 4 bytes  |
| `i64`    | 64-bit signed integer          | 8 bytes  |
| `u32`    | 32-bit unsigned integer        | 4 bytes  |
| `u64`    | 64-bit unsigned integer        | 8 bytes  |
| `bool`   | Logical (`0` / `1`)            | 1 byte   |
| `string` | Immutable UTF-8 string         | 16 bytes |

## 4.  Syntactic Specification

### 4.1 Notational Conventions
The grammar is written in extended Backus–Naur form (EBNF) with the following meta-symbols:

    ::=                 defines a rule
    |                   separates alternatives
    [...]               marks an optional fragment (zero or one occurrence)
    {...}               marks zero or more repetitions of the enclosed fragment
    (...)               groups
    ...+                marks one or more repetitions
    UPPER_SNAKE         identifiers denote lexical tokens produced by the lexer
    UpperCamelCase      identifiers denote non-terminals
    "..."               a literal character sequence (quotes are not part of the token)
    any_unicode         any Unicode scalar, except U+0022 ("), U+005C (\), U+0000–U+001F, U+007F–U+009F

All meta-symbols are ASCII.

Example:
Expr ::= Term { ("+" | "-") Term }

### 4.2 Syntactic Grammar (EBNF)
Program      ::= { Function | Struct } ;
Struct       ::= "struct" IDENT "{" { StructField } "}" ;
StructField  ::= IDENT ":" Type ";" ;
Function     ::= "fn" ( "voloid" | Type ) IDENT "(" [ Param { "," Param } ] ")" Block ;
Param        ::= IDENT ":" Type ;
Block        ::= "{" { Stmt } "}" ;
Stmt         ::= DeclStmt | AssignStmt | IfStmt | WhileStmt | ForStmt | ReturnStmt | BreakStmt | ExprStmt | Block | ";" ;
DeclStmt     ::= [ "const" ] Type IDENT [ "=" Expr ] ";" ;
Type         ::= PrimaryType { PostfixArray } ;
PrimaryType  ::= BUILTIN_TYPE | IDENT ;
PostfixArray ::= "[" Expr "]" ;
IfStmt       ::= "if" "(" Expr ")" Stmt [ "else" Stmt ] ;
WhileStmt    ::= "while" "(" Expr ")" Stmt ;
ReturnStmt   ::= "return" [ Expr ] ;
ForStmt      ::= "for" "(" [ ForInit ] ";" [ Expr ] ";" [ Expr ] ")" Stmt ;
ForInit      ::= Init { "," init } ;
init         ::= Decl | AssignExpr ;
Decl         ::= [ "const" ] Type IDENT [ "=" Expr ] ;
AssignExpr   ::= IDENT [ "=" Expr ] ;
BreakStmt    ::= "break" ";" ;
AssignStmt   ::= IDENT [ "=" Expr ] ";" ;
ExprStmt     ::= Expr ";" ;
Expr         ::= ExprLogicOr ;
ExprLogicOr  ::= ExprLogicAnd { || ExprLogicAnd } ;
ExprLogicAnd ::= ExprCom { "&&" ExprCom } ;
ExprCom      ::= ExprAdd { ( "<" | "<=" | ">" | ">=" | "!=" | "==" ) ExprAdd } ;
ExprAdd      ::= ExprMul { ( "+" | "-" ) ExprMul } ; 
ExprMul      ::= ExprUnary { ( "*" | "/" | "%" ) ExprUnary } ;
ExprUniry    ::= ( "-" | "+" | "!" ) ExprUniry | ExprPostfix ;
ExprPostfix  ::= ExprPrimary { PostfixOp } ;
PostfixOp    ::= "[" Expr "]" | "(" [ ArgumentList ] ")" | "." IDENT ;
ArgumentList ::= Expr { "," Expr } ;
ExprPrimary  ::= IDENT | INT_LIT | STRING_LIT | BOOL_LIT | ARRAY_LIT ;
BUILTIN_TYPE ::= "i32" | "i64" | "u32" | "u64" | "bool" | "string" ;
INT_LIT      ::= DEC_LIT | HEX_LIT | BIN_LIT | OCT_LIT ;
DecLit       ::= DigitNoZero { Digit | "_" } ;
HexLit       ::= "0" [ "x" | "X" ] HexDigit { HexDigit | "_" } ;
BinLit       ::= "0" [ "b" | "B" ] ( "0" | "1" ) { "0" | "1" | "_" } ;
OctLit       ::= "0" [ "o" | "O" ] OctDigit { OctDigit | "_" } ;
HexDigit     ::= Digit | "a" | "b" | "c" | "d" | "e" | "f" | "A" | "B" | "C" | "D" | "E" | "F" ;
DecDigit     ::= "0" | "1" | "2" | "3" | "4" | "5" | "6" | "7" | "8" | "9" ;
OctDigit     ::= "0" | "1" | "2" | "3" | "4" | "5" | "6" | "7" ;
DigitNoZero  ::= "1" | "2" | "3" | "4" | "5" | "6" | "7" | "8" | "9" ;
IDENT        ::= Letter { Letter | DecDigit | "_" } ;
Letter       ::= "a" | "b" | "c" | "d" | "e" | "f" | "g" | "h" | "i" | "j" | "k" | "l" | "m"
             | "n" | "o" | "p" | "q" | "r" | "s" | "t" | "u" | "v" | "w" | "x" | "y" | "z"
             | "A" | "B" | "C" | "D" | "E" | "F" | "G" | "H" | "I" | "J" | "K" | "L" | "M"
             | "N" | "O" | "P" | "Q" | "R" | "S" | "T" | "U" | "V" | "W" | "X" | "Y" | "Z" ;
STRING_LIT   ::= """ { any_unicode | EscapeSeq } """ ;
ESCAPE_SEQ   ::= "\\" | "\"" | "\n" | "\t" | "\r" ;
BOOL_LIT     ::= "true" | "false" ;
ARRAY_LIT    ::= "[" [ ArgumentList ] "]" ;