# Specification for the Volodya-- programming language

## 1. Introduction
The Volodya-- programming language is an imperative and procedural (possibly object-oriented?) C-like programming language oriented toward (systems programming?). It is designed to be simple enough, but encourages disciplined programming. The main purpose is to explore the behavior of programming languages more deeply and fully understand all the nuances of compiler building. The secondary purpose is to write a small program using it. The Volodya-- programming language was inspired by C (but organized rather differently?).

The Volodya-- programming language is statically, strongly, nominally, and explicitly typed. Casting has to be explicit both ways. Compilation consists of translating source code into assembly. (Polymorphism?)

The Volodya-- programming language is a relatively low-level language. It includes manual memory management with a free and malloc commands.

## 2. Lexical structure
| Categories     | Examples                                                                            
|----------------|-------------------------------------------------------------------------------------------
| Keywords       | `fn`, `if`, `else`, `while`, `return`, `struct`, `i32`, `i64`, `u32`, `u64`, `bool`, `string`, `break`, `const`, `for`,  `voloid` (`continue`, `b32`, `b64`, `char`, `switch`, `case`, `free`, `malloc`)
| Operators      | `+ - * / %`, `&& \|\| !`, `== != < > <= >=`, `=` (`\| & ~ ^`)
| Separators     | `(` `)` `{` `}` `[` `]` `,` `.` `:` `;`
| Literals       | integers `123`, booleans `true/false`, strings `"text"`, escape sequences `\"` (characters `'a'`, floating point numbers `1.23`)
| Identifiers    | `[A-Za-z][A-Za-z0-9_]*`

## 3. Types
| Type     | Description                    | Size     |
|----------|--------------------------------|----------|
| `i32`    | 32-bit signed integer          | 4 bytes  |
| `i64`    | 64-bit signed integer          | 8 bytes  |
| `u32`    | 32-bit unsigned integer        | 4 bytes  |
| `u64`    | 64-bit unsigned integer        | 8 bytes  |
| `bool`   | Logical (`0` / `1`)            | 4 bytes  |
| `string` | Immutable UTF-8 string         | 16 bytes |

## 4. Syntactic Specification
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

Example: Expr ::= Term { ("+" | "-") Term }

### 4.2 Syntactic Grammar (EBNF)
```EBNF
Program      ::= { Function | Struct } ;
Struct       ::= "struct" IDENT "{" { StructField } "}" ;
StructField  ::= IDENT ":" Type ";" ;
Function     ::= "fn" ( "voloid" | Type ) IDENT "(" [ Param { "," Param } ] ")" Block ;
Param        ::= IDENT ":" Type ;
Block        ::= "{" { Stmt } "}" ;
Stmt         ::= DeclStmt | IfStmt | WhileStmt | ForStmt 
               | ReturnStmt | BreakStmt | ExprStmt | Block | ";" ;
DeclStmt     ::= DeclExpr ";" ;
Type         ::= PrimaryType { PostfixArray };
PrimaryType  ::= BUILTIN_TYPE | IDENT ;
PostfixArray ::= "[" Expr "]" ;
IfStmt       ::= "if" "(" Expr ")" Stmt [ "else" Stmt ] ;
WhileStmt    ::= "while" "(" Expr ")" Stmt ;
ReturnStmt   ::= "return" [ Expr ] ";" ;
ForStmt      ::= "for" "(" [ ForInit ] ";" [ Expr ] ";" [ Step ] ")" Stmt ;
ForInit      ::= Init { "," Init } ;
Init         ::= DeclExpr | AssignExpr ;
DeclExpr     ::= [ "const" ] Type IDENT [ "=" Expr ] ;
AssignExpr   ::= LValue "=" Expr;
LValue       ::= IDENT { "[" Expr "]" | "." IDENT } ;
Step         ::= StepItem { "," StepItem } ;
StepItem     ::= AssignExpr | Expr ;
BreakStmt    ::= "break" ";" ;
ExprStmt     ::= Expr ";" ;
Expr         ::= ExprLogicOr ;
ExprLogicOr  ::= ExprLogicAnd { "||" ExprLogicAnd } ;
ExprLogicAnd ::= ExprEq { "&&" ExprEq } ;
ExprEq       ::= ExprRel { ("==" | "!=") ExprRel } ;
ExprRel      ::= ExprAdd { ("<" | "<=" | ">" | ">=") ExprAdd } ;
ExprAdd      ::= ExprMul { ( "+" | "-" ) ExprMul } ; 
ExprMul      ::= ExprCast { ( "*" | "/" | "%" ) ExprCast } ;
ExprCast     ::= [ "(" CastType ")" ] ExprUnary ;
CastType     ::= BUILTIN_TYPE { ArraySuffix } | IDENT { "." IDENT } { ArraySuffix } ;
ArraySuffix  ::= "[" "]" ;
ExprUnary    ::= ( "-" | "+" | "!" ) ExprUnary | ExprPostfix ;
ExprPostfix  ::= ExprPrimary { PostfixOp } ;
PostfixOp    ::= "[" Expr "]" | "(" [ List ] ")" | "." IDENT ;
List         ::= Expr { "," Expr } ;
ExprPrimary  ::= IDENT | IntLit | STRING_LIT | BOOL_LIT | ARRAY_LIT | "(" Expr ")" ;
BUILTIN_TYPE ::= "i32" | "i64" | "u32" | "u64" | "bool" | "string" ;
IntLit       ::= DEC_LIT | HEX_LIT | BIN_LIT | OCT_LIT ;
DEC_LIT      ::= DigitNoZero { DecDigit | "_" } | "0" ;
HEX_LIT      ::= "0" ( "x" | "X" ) HexDigit { HexDigit | "_" } ;
BIN_LIT      ::= "0" ( "b" | "B" ) ( "0" | "1" ) { "0" | "1" | "_" } ;
OCT_LIT      ::= "0" ( "o" | "O" ) OctDigit { OctDigit | "_" } ;
HexDigit     ::= DecDigit | "a" | "b" | "c" | "d" | "e" | "f" | "A" | "B" | "C" | "D" | "E" | "F" ;
DecDigit     ::= "0" | "1" | "2" | "3" | "4" | "5" | "6" | "7" | "8" | "9" ;
OctDigit     ::= "0" | "1" | "2" | "3" | "4" | "5" | "6" | "7" ;
DigitNoZero  ::= "1" | "2" | "3" | "4" | "5" | "6" | "7" | "8" | "9" ;
IDENT        ::= Letter { Letter | DecDigit | "_" } ;
Letter       ::= "a" | "b" | "c" | "d" | "e" | "f" | "g" | "h" | "i" | "j" | "k" | "l" | "m"
               | "n" | "o" | "p" | "q" | "r" | "s" | "t" | "u" | "v" | "w" | "x" | "y" | "z"
               | "A" | "B" | "C" | "D" | "E" | "F" | "G" | "H" | "I" | "J" | "K" | "L" | "M"
               | "N" | "O" | "P" | "Q" | "R" | "S" | "T" | "U" | "V" | "W" | "X" | "Y" | "Z" ;
STRING_LIT   ::= """ { any_unicode | EscapeSeq } """ ;
EscapeSeq    ::= "\\" | "\"" | "\n" | "\t" | "\r" ;
BOOL_LIT     ::= "true" | "false" ;
ARRAY_LIT    ::= "[" [ List ] "]" ;
```

## 6. Examples
### 1. hello.vol
```Volodya--
fn voloid main() {
    print("Hello, Voloid!");
}
```
Simple “Hello World” program. Shows fn declaration, zero‑argument call, and a basic string literal.
### 2. arith.vol
```Volodya--
fn i32 add(a:i32, b:i32) {
    return a + b;
}
```
Pure integer arithmetic. Demonstrates parameterised function, typed parameters, return, infix +, and semicolon terminators.
### 3. logic.vol
```Volodya--
fn bool cmp(x:u64, y:u64) {
    return x >= y && x != 0;
}
```
boolean expressions with relational (>=, !=) and logical (&&) operators. Also shows unsigned integer types.
### 4. literals.vol
```Volodya--
const i32 BIN = 0B1010_0101;
const i64 OCT = 0o7_5_5_;
const u32 DEC = 123___456;
const u64 HEX = 0xDEAD__BEEF;
const bool FLAG1 = true;
const bool FLAG2 = false;
```
Every numeric literal form and type (binary, octal, decimal with underscores, hexadecimal) plus true/false boolean literal.
### 5. control.vol
```Volodya--
fn i64 add(a:i32, b:i64) {
    return (i64)a + b;   // explicit cast from i32 → i64
}

// demo of control‑flow constructs and mutations
fn voloid loop_demo() {
    i32 counter = 0;
    while (true) {
        if (counter == 3) break;
        counter = counter + 1;
    }

    //   init:   i = 10 , j = 10
    //   cond:   k < l
    //   update: k *= 2 , l = l + add(i, j)

    for (i32 i = 10, i64 j = 10;
         i32 k = 1,  i64 l = 50;
         (i64)k < l;
         k = k * 2,   l = l + add(i, j))
    {
        print("inside for");
    }
}
```
Demonstrates advanced control flow:

a while loop with an if + break, mutable variable updates;  
explicit integer type cast (i64)a;  
a for loop that includes multiple initialisers of different types, a compound condition, and two update expressions (k = k * 2, l = l + add(i, j));  
use of the custom voloid return type and a function call with mixed‐size integers.
### 6. voloid.vol
```Volodya--
fn voloid nop() {
    // function with “voloid” return type – the language’s custom void.
    return;
}
```
Highlights the language‑specific voloid keyword and a bare return. Statement without a value.
### 7. strings.vol
```Volodya--
fn voloid demo() {
    print("line1\nline2\tTabbed \"quoted\" text \\ backslash");
}
```
String literal containing valid escape sequences (\n, \t, \", \\). Confirms escape handling and newline prohibition inside strings.
### 8. comments.vol
```Volodya--
// single‑line comment
/* multi
   line */
fn voloid ok() {}
```
Both comment styles (//, /* … */). Lexer discards them entirely.
### 9. structs.vol
```Volodya--
struct Vec3 {
    x:i32;
    y:i32;
    z:i32;
}

fn i32 dot(a:Vec3, b:Vec3) {
    return a.x * b.x + a.y * b.y + a.z * b.z;
}
```
Introduces the struct keyword, field access with the dot operator, and passing user‑defined types by value.
### 10. kitchen_sink.vol
```Volodya--
/* Every feature in one go */
const u64 HEX = 0xDEAD_BEEF;
const u32 BIN = 0b1010_1010;
const bool FLAG = false;

struct Pair { a:i32; b:i32; }

fn voloid main() {
    Pair p = { .a = 1, .b = 2 };
    i32 sum = p.a + p.b;

    for (i32 i = 0; i < 5; i = i + 1) {
        while (true) {
            if (i == 3) break;
            print("i = " + i);
            break;
        }
    }

    // call with cast and logical ops
    bool ok = (i64)sum >= 1 && FLAG == false;
    if (ok) {
        print("everything parsed");
    }
}
```
A “kitchen‑sink” example that throws together constants in every numeral base, a struct, field initialisers, nested loops, casts, string literals, logical and arithmetic operators, and both comment styles.