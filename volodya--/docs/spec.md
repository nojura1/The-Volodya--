# Specification for the Volodya-- programming language

## 1. Introduction

The Volodya-- programming language is an imperative and procedural programming language oriented toward general programming. It is designed to be simple enough, but encourages disciplined programming.

The Volodya-- programming language is statically, strongly, nominally, and explicitly typed. Casting has to be explicit. The compiler operates ahead of time and translates source code into assembly.

## 2. Lexical structure

This section defines the source character set, lexical token categories, literal syntax, translation limits, and lexical error handling.

The following table summarizes the token categories recognized by the lexer. Detailed rules and restrictions are given in the subsequent sections.

| Category         | Examples|
| ---------------- | ------------------------------------------------------------------------------------------------------------------------ |
| Keywords         | `fn`, `if`, `else`, `while`, `return`, `struct`, `i32`, `i64`, `u32`, `u64`, `bool`, `string`, `break`, `const`, `for`, `voloid` |
| Operators        | `+`, `-`, `*`, `/`, `%`, `&&`, `\|\|`, `!`, `==`, `!=`, `<`, `>`, `<=`, `>=`, `=`|
| Separators       | `(`, `)`, `{`, `}`, `[`, `]`, `,`, `.`, `:`, `;`|
| Integer literals | `123`, `0b1010`, `0xFF`, `0o755`|
| Boolean literals | `true`, `false`|
| String literals  | `"text"`, `"line\n"`|
| Identifiers      | `[A-Za-z][A-Za-z0-9_]*`|

### 2.1 Source character set

Outside string literals, only ASCII characters are permitted. Any non-ASCII Unicode character appearing outside a string literal is rejected and reported as a lexical error.

For diagnostic purposes, each valid UTF-8 code point is consumed as a single source character. This ensures that reported line and column positions remain accurate throughout lexical analysis.

UTF-8 validation applies only to source code. At runtime, a value of type `string` is an array of bytes and is not required to contain valid UTF-8.

For example, the following program constructs a string containing the isolated byte `0x80`, which is not a valid UTF-8 sequence:

```vol
fn i32 main() {
    string[2] str;

    str[0] = (u32)128;
    str[1] = (u32)0;

    print(str, "\n");
    return 0;
}
```

The `print` function does not validate or decode UTF-8. It writes the bytes of each string directly to standard output until the first null byte is encountered. Consequently, the way invalid byte sequences are displayed depends on the receiving terminal or output consumer.

### 2.2 Translation limits

No lexeme other than a string literal may exceed 256 source characters. Identifiers, integer literals, and all other non-string lexemes that exceed this limit are consumed in full and then rejected, and a lexical error is reported.

A string literal may occupy at most 4,098 source bytes, including its opening and closing quotation marks. Therefore, at most 4,096 bytes may appear between the quotation marks.

Because UTF-8 characters may occupy more than one byte, the maximum number of characters that can appear in a string literal depends on their UTF-8 encoding. For example, a string consisting entirely of two-byte UTF-8 characters may contain at most 2,048 such characters.

If the byte limit is exceeded, the lexer consumes all remaining input up to the closing quotation mark or the end of the file and then reports a lexical error.

A source file must not exceed 32 MiB. If this limit is exceeded, a fatal error is reported and compilation is aborted immediately.

A source file may produce at most 65,536 tokens. If this limit is exceeded, compilation is aborted after lexical analysis is complete.

### 2.3 Lexical diagnostics

Lexical diagnostics are buffered and reported after lexical analysis is complete or terminated.

At most 16 lexical errors may be collected. If a seventeenth lexical error is encountered, lexical analysis is terminated and compilation is aborted.

### 2.4 Integer literals

There are four forms of integer literals:

* **Decimal literals** have no prefix. The literal `0` must not be followed by another decimal digit; therefore, leading zeros are not permitted.
* **Binary literals** begin with the prefix `0b` or `0B`. Leading zeros after the prefix are permitted.
* **Hexadecimal literals** begin with the prefix `0x` or `0X`. Leading zeros after the prefix are permitted. Hexadecimal digit values from 10 through 15 are represented by the letters `A` through `F` or their lowercase equivalents `a` through `f`.
* **Octal literals** begin with the prefix `0o` or `0O`. Leading zeros after the prefix are permitted. The uppercase prefix `0O` is discouraged because the letter `O` can easily be confused with the digit `0`.

The underscore character (`_`) may be used as a digit separator in an integer literal. One or more underscores may appear between digits or at the end of the literal.

An underscore must not appear at the beginning of a literal or between its radix prefix and its first digit.

Examples:

```text
123_456        valid
123___456      valid
123_           valid
0b1010_0011    valid
0xFF__00_      valid

_123           invalid
0b_1010        invalid
0x_FF          invalid
```

### 2.5 String literals

A string literal begins and ends with a double quotation mark (`"`).

There are five valid escape sequences:

```text
\\
\"
\n
\t
\r
\0
```

They represent a backslash, a double quotation mark, a newline, a horizontal tab, and a carriage return, respectively.

Any other escape sequence is invalid. The lexer reports a lexical error and consumes the invalid escape sequence.

String literals may contain non-ASCII characters encoded as valid UTF-8. An invalid UTF-8 byte sequence appearing anywhere in the source file, including inside a string literal, is rejected and reported as a lexical error.

### 2.6 Whitespace and comments

Whitespace consists of spaces, horizontal tabs, line terminators, and comments. Whitespace does not produce tokens and is ignored during lexical analysis, except that it may separate adjacent tokens.

For example, comments may be used wherever whitespace is permitted:

```vol
i32/* type-name separator */value = 42;
```

This is lexically equivalent to:

```vol
i32 value = 42;
```

The language supports two forms of comments:

* A **line comment** begins with `//` and continues up to the next line terminator or the end of the file.
* A **block comment** begins with `/*` and ends with the next occurrence of `*/`. A block comment may span multiple lines. If the end of the file is reached before the terminating `*/`, a lexical error is reported.

Whitespace is otherwise insignificant and is primarily used for readability or to prevent adjacent characters from being interpreted as a single lexeme.

Consequently, the following program is valid:

```vol
fn/**/i32/**/main(){string[16]/**/buf;i32/**/a=0b0000000000000_;i32_to_string(a,buf);print("\t\r",buf,"\t\n");return/**/0;}//some nonsense
```

It is lexically equivalent to the following formatted program:

```vol
fn i32 main() {
    string[16] buf;
    i32 a = 0b0000000000000_;

    i32_to_string(a, buf);
    print("\t\r", buf, "\t\n");

    return 0;
}
```
## 3. Syntactic specification

This section defines the grammatical structure of programs, parser-level restrictions, syntactic translation limits, and syntax error handling.

### 3.1 Integer literal processing

During parsing, binary, octal, hexadecimal, and decimal integer literals are converted to their corresponding numeric values. The radix used in the source code does not affect the resulting value.

The parsed value must not exceed `2^64 - 1`. If the value is outside this range, a syntax error is reported.

This numeric range check is distinct from the lexical limit of 256 source characters per integer literal. A literal may satisfy the lexical length limit while still representing a value greater than `2^64 - 1`.

The type of an integer literal is assigned during semantic analysis.

### 3.2 Lists and commas

Function parameter lists, function argument lists, array literals, and the initialization and step clauses of for statements use commas to separate adjacent items.

Empty elements, repeated commas, and trailing commas are not permitted.

Examples:

```vol
fn i32 add(a: i32, b: i32) {  // valid
    return a + b;
}

add(1, 2);                  // valid
i32[3] xs = [1, 2, 3];     // valid

add(1,, 2);                 // invalid
add(1, 2,);                 // invalid
i32[3] ys = [1,, 2];        // invalid
i32[3] zs = [1, 2, 3,];     // invalid
```

### 3.3 Types, casts, and array sizes

A cast consists of a type enclosed in parentheses followed by an expression.

At the syntactic stage, any grammatically valid cast type is accepted, including built-in types, structure types, and incomplete array/string types:

```vol
(i32)value
(string[])value
(Pair)value
(Pair[])value
```

Semantic analysis subsequently determines whether a particular cast is permitted.

The size of a local array or string must be specified by a plain integer literal. Arbitrary expressions, variables, and function calls are not permitted as array sizes.

Examples:

```vol
i32[10] values;       // valid
string[256] buffer;   // valid
i32[0x100] table;     // valid

i32[n] values;        // invalid syntax
i32[5 + 5] values;    // invalid syntax
string[size()] text;  // invalid syntax
```

A sized string declaration therefore has the following general form:

```vol
string[size] name;
```

Only one array suffix is permitted. Multidimensional array types are not supported.

Incomplete array types such as `i32[]` and `string[]` may appear in function parameters and cast types.

### 3.4 Statements and control flow

The initialization part of a `for` statement may contain variable declarations and assignments separated by commas.

The condition part may contain any expression. If the condition is omitted, the loop has no syntactic termination condition.

The step part may contain assignments or other expressions separated by commas. The values produced by these expressions are discarded.

All three parts are optional:

```vol
for (;;) {
    break;
}
```

The body of an `if`, `while`, or `for` statement may be either a block or any single statement. Therefore, the following program fragment is valid:

```vol
for (;;) if (true) break;
```

An `else` clause is associated with the nearest preceding unmatched `if`.

Any expression may be used as an expression statement. Its resulting value is discarded:

```vol
calculate();
1 + 2;
value;
```

Expression statements are primarily useful for function calls and expressions that produce side effects, although expressions without side effects are also syntactically valid.

An empty statement consists of a single semicolon:

```vol
;
```

### 3.5 Syntactic translation limits

The parser applies the following limits:

| Construct                              | Maximum |
| -------------------------------------- | ------: |
| Elements in one array literal          |   1,024 |
| Arguments in one function call         |     256 |
| Parameters in one function declaration |     256 |
| Top-level declarations                 |   1,024 |
| Collected syntax errors                |      16 |

If a seventeenth syntax error is encountered, parsing is terminated and compilation is aborted.

A structure must contain at least one field. Empty structure declarations are not permitted.

### 3.6 Notational conventions

The grammar is written in extended Backus–Naur form (EBNF) using the following meta-symbols:

```text
::=                 defines a production
|                   separates alternatives
[...]               marks an optional fragment
{...}               marks zero or more repetitions
(...)               groups a fragment
UPPER_SNAKE         denotes a token produced by the lexer
UpperCamelCase      denotes a syntactic nonterminal
"..."               denotes a literal character sequence
```

The quotation marks used to denote literal character sequences are not part of the resulting token.

All grammar meta-symbols are ASCII.

Example:

```ebnf
ExprAdd ::= ExprMul { ("+" | "-") ExprMul } ;
```

### 3.7 Syntactic grammar

```ebnf
Program         ::= { Function | Struct } ;

Struct          ::= "struct" IDENT "{"
                        StructField { StructField }
                    "}" ;

StructField     ::= IDENT ":" Type ";" ;

Function        ::= "fn" ReturnType IDENT
                    "(" [ ParamList ] ")"
                    Stmt ;

ReturnType      ::= "voloid"
                  | PrimaryType [ ArraySuffix ] 
                  | "string" ArraySuffix ;

ParamList       ::= Param { "," Param } ;

Param           ::= IDENT ":" (PrimaryType [ ArraySuffix ] | "string" ArraySuffix) ;

Block           ::= "{" { Stmt } "}" ;

Stmt            ::= DeclStmt
                  | IfStmt
                  | WhileStmt
                  | ForStmt
                  | AssignStmt
                  | ReturnStmt
                  | BreakStmt
                  | ExprStmt
                  | Block
                  | ";" ;

DeclStmt        ::= DeclExpr ";" ;

AssignStmt      ::= AssignExpr ";" ;

Type            ::= PrimaryType [ SizedArraySuffix ]
                  | "string" SizedArraySuffix ;

PrimaryType     ::= BuiltinType | IDENT ;

BuiltinType     ::= "i32"
                  | "i64"
                  | "u32"
                  | "u64"
                  | "bool" ;

SizedArraySuffix
                ::= "[" IntLit "]" ;

ArraySuffix     ::= "[" "]" ;

IfStmt          ::= "if" "(" Expr ")" Stmt
                    [ "else" Stmt ] ;

WhileStmt       ::= "while" "(" Expr ")" Stmt ;

ForStmt         ::= "for" "("
                        [ ForInit ] ";"
                        [ Expr ] ";"
                        [ Step ]
                    ")"
                    Stmt ;

ForInit         ::= Init { "," Init } ;

Init            ::= DeclExpr | AssignExpr ;

Step            ::= StepItem { "," StepItem } ;

StepItem        ::= AssignExpr | Expr ;

ReturnStmt      ::= "return" [ Expr ] ";" ;

BreakStmt       ::= "break" ";" ;

ExprStmt        ::= Expr ";" ;

DeclExpr        ::= [ "const" ] Type IDENT
                    [ "=" Expr ] ;

AssignExpr      ::= LValue "=" Expr ;

LValue          ::= IDENT
                    { "[" Expr "]" | "." IDENT } ;

Expr            ::= ExprLogicOr ;

ExprLogicOr     ::= ExprLogicAnd
                    { "||" ExprLogicAnd } ;

ExprLogicAnd    ::= ExprEq
                    { "&&" ExprEq } ;

ExprEq          ::= ExprRel
                    { ("==" | "!=") ExprRel } ;

ExprRel         ::= ExprAdd
                    { ("<" | "<=" | ">" | ">=") ExprAdd } ;

ExprAdd         ::= ExprMul
                    { ("+" | "-") ExprMul } ;

ExprMul         ::= ExprCast
                    { ("*" | "/" | "%") ExprCast } ;

ExprCast        ::= [ "(" CastType ")" ] ExprUnary ;

CastType        ::= PrimaryType [ ArraySuffix ] | "string" ArraySuffix ;

ExprUnary       ::= ("-" | "+" | "!") ExprCast
                  | ExprPostfix ;

ExprPostfix     ::= ExprPrimary { PostfixOp } ;

PostfixOp       ::= "[" Expr "]"
                  | "(" [ ExprList ] ")"
                  | "." IDENT ;

ExprList        ::= Expr { "," Expr } ;

ExprPrimary     ::= IDENT
                  | IntLit
                  | STRING_LIT
                  | BOOL_LIT
                  | ArrayLit
                  | "(" Expr ")" ;

ArrayLit        ::= "[" [ ExprList ] "]" ;

IntLit          ::= DEC_LIT
                  | HEX_LIT
                  | BIN_LIT
                  | OCT_LIT ;
```

The lexical forms of `IDENT`, `DEC_LIT`, `HEX_LIT`, `BIN_LIT`, `OCT_LIT`, `STRING_LIT`, and `BOOL_LIT` are defined in Section 2.

## 4. Semantic analysis

Semantic analysis is performed after parsing and before code generation.

During this stage, the compiler:

* resolves identifiers to symbols;
* constructs and validates structure definitions;
* checks types and explicit casts;
* validates assignments, function calls, indexing, and field access;
* enforces control-flow restrictions;
* performs definite-initialization analysis;
* verifies that required return statements are present.

Semantic diagnostics are buffered and emitted after semantic analysis completes or is terminated.

### 4.1 Types, object sizes, and integer literals

| Type           | Description                                                                 |                              Size |
| -------------- | --------------------------------------------------------------------------- | --------------------------------: |
| `voloid`       | Absence of a value; permitted only as a function return type                |                           0 bytes |
| `i32`          | 32-bit signed integer                                                       |                           4 bytes |
| `i64`          | 64-bit signed integer                                                       |                           8 bytes |
| `u32`          | 32-bit unsigned integer                                                     |                           4 bytes |
| `u64`          | 64-bit unsigned integer                                                     |                           8 bytes |
| `bool`         | Boolean value represented as `0` or `1`                                     |                           4 bytes |
| `T[N]`         | Fixed-size array containing `N` objects of type `T`                         |                   `N × sizeof(T)` |
| `string[N]`    | Fixed-size array of `N` bytes, normally containing a null-terminated string |                         `N` bytes |
| Structure type | User-defined aggregate type                                                 | Depends on its fields and padding |

The type `voloid` is not an object type. It may be used only as the return type of a function. Consequently, declarations such as `voloid[]`, `voloid[4]`, or a structure field of type `voloid` are invalid.

An array or string size must:

* be represented by an integer literal;
* have type `i32`;
* be greater than zero.

Zero-length arrays and zero-length strings are not permitted.

Every integer literal in the inclusive range from `0` to `2³¹ - 1` has type `i32`.

The expression `-2147483648` consists of the unary minus operator applied to the literal `2147483648`. The literal itself has type `i64`, and the resulting expression therefore also has type `i64`.

An explicit cast is required to store this value in an `i32` object:

```vol
i32 minimum = (i32)(-2147483648);
```

Integer literals in the inclusive range from `2³¹` to `2⁶³ - 1` have type `i64`.

Integer literals greater than `2⁶³ - 1` and less than or equal to `2⁶⁴ - 1` have type `u64`. Such a literal cannot be assigned to an `i64` object without an explicit cast.

The language does not provide integer-literal suffixes such as `u32` or `u64`. Explicit casts must therefore be used to produce values of those types.

No implicit conversion is performed between different integer types.

The compiler does not perform overflow checking and the language does not define overflow behavior independently of the generated NASM instructions.

### 4.2 Type identity and compatibility

Built-in types are identical only when they are the same built-in type. For example, `i32` and `u32` are distinct types even though they have the same size.

Structure types are nominal. Two different structure declarations describe different types even if their fields have identical names and types.

In ordinary object declarations, initializers and assignments, two array types are identical only if:

* their element types are identical; and
* their lengths are equal.

Two string object types are identical only if their lengths are equal.

Array and string types used in function parameter or return positions are written with empty brackets:

```vol
fn voloid process(values: i32[]);
fn string[] make_text();
```

A size must not be written inside those brackets. The function type records the array element type or the fact that the value is a string, but it does not record a concrete length.

Two function types are identical only if they have:

* the same number of parameters;
* identical corresponding parameter types; and
* identical return types.

### 4.3 Declarations, scopes, and name resolution

Each block introduces a new lexical scope.

An identifier becomes visible at the point of its declaration and remains visible until the end of its scope. An identifier used before it becomes visible is rejected as undefined.

Functions, structure types, parameters, and local variables use the same ordinary identifier namespace.

Two declarations in the same scope must not use the same name, except for compatible declarations of the same function.

A function definition may be preceded by any number of compatible declarations. No further declarations of that function are permitted after its definition.

Structure fields use a separate member namespace belonging to their containing structure. Fields in different structures may therefore have the same name.

Function parameters belong to the outermost block scope of the function body. A local variable declared directly in that block must not have the same name as a parameter.

Top-level variable declarations are not supported. Only function and structure declarations may appear at the top level.

A function definition makes its own name visible inside its body, allowing direct recursion:

```vol
fn i32 factorial(n: i32) {
    if (n <= 1) {
        return 1;
    }

    i32 previous = factorial(n - 1);
    return n * previous;
}
```

A structure type must be visible before it is used.

### 4.4 Casts, operators, and evaluation order

Explicit casts are permitted only between integer types:

```text
i32
i64
u32
u64
```

For example:

```vol
i64 wide = (i64)value;
u32 bits = (u32)wide;
```

Casts involving `bool`, strings, arrays, structures, function types, or `voloid` are not permitted.

A syntactically valid cast may therefore still be rejected during semantic analysis.

The binary arithmetic operators

```text
+  -  *  /  %
```

may be applied only to integer operands. Both operands must have exactly the same type, and the result has that same type.

The `+` operator does not perform string or array concatenation.

The unary operator `+` may be applied to an integer operand.

The unary operator `-` may be applied only to signed integer operands of type `i32` or `i64`. It is not permitted for `u32` or `u64`.

The ordering comparison operators

```text
<  <=  >  >=
```

may be applied only to operands of the same integer type.

The equality operators

```text
==  !=
```

may be applied to operands of the same integer type or to two operands of type `bool`.

Every comparison expression has type `bool`.

The logical operators

```text
&&  ||
```

may be applied only to Boolean operands and produce a Boolean result.

Both logical operators evaluate their left operand first and use short-circuit evaluation:

* the right operand of `&&` is evaluated only if the left operand is `true`;
* the right operand of `||` is evaluated only if the left operand is `false`.

The unary operator `!` may be applied only to a Boolean operand.

Except where explicitly specified, the evaluation order of operands is unspecified. A program must not depend on a particular operand evaluation order.

### 4.5 Lvalues, assignment, and constant objects

The left operand of an assignment must be a modifiable lvalue.

Identifiers referring to variables, including array, string, and structure objects, are lvalues. Array and string elements and structure fields are also lvalues.

Function calls, literals, casts, and the results of arithmetic, comparison, or logical expressions are not lvalues.

The `.` separator may be used only to access a declared field of a structure value:

```vol
pair.first
```

Applying `.` to a non-structure value or naming an unknown field is a semantic error.

A declaration containing the `const` qualifier must provide an initializer:

```vol
const i32 answer = 42;
```

A constant object cannot be assigned a new value after initialization.

If a constant object is an aggregate, its elements and fields are also non-modifiable:

```vol
const i32[3] values = [1, 2, 3];
values[0] = 10;              // semantic error

const Pair pair = make_pair(1, 2);
pair.first = 10;             // semantic error
```

The `const` qualifier does not require a compile-time constant initializer. It only prevents modification after initialization.

### 4.6 Arrays and strings

Only one-dimensional arrays are supported.

An array element type must not itself be an array or a string. Consequently, multidimensional arrays and arrays of strings are not permitted.

An array literal must contain at least one element. Empty array literals are not permitted.

All elements of an array literal must have identical types.

When an array literal initializes an array object, the number of elements in the literal must exactly match the declared array length:

```vol
i32[3] values = [1, 2, 3];       // valid
i32[3] too_few = [1, 2];         // semantic error
i32[3] too_many = [1, 2, 3, 4];  // semantic error
```

A string literal is represented by its encoded bytes followed automatically by a null terminator.

A string literal containing `N` encoded bytes therefore has type `string[N + 1]`.

For example:

```vol
"A"    // string[2]
"abc"  // string[4]
""     // string[1]
"\n"   // string[2]
```

When a string literal initializes a string object, the declared string length must exactly match the size of the literal, including its automatically appended null terminator:

```vol
string[2] digit = "1";    // valid
string[4] word = "abc";   // valid

string[3] invalid1 = "1";   // semantic error
string[3] invalid2 = "abc"; // semantic error
```

Indexing is permitted only for arrays and strings. The index expression must have type `i32`:

```vol
i32 value = values[index];
u32 byte = text[index];
```

Indexing an array produces an expression whose type is the element type of that array.

Indexing a string produces an expression of type `u32`. When a value is stored into a string element, only its least significant eight bits are written to the underlying byte.

The implementation does not perform runtime bounds checking. Accessing an array or string with an invalid index results in undefined behavior.

Strings are fixed-size mutable raw byte arrays. They are not automatically validated or decoded as UTF-8.

### 4.7 Structures and aggregate values

A structure must contain at least one field, and field names must be unique within that structure.

Every structure field is stored by value. Structure size and alignment are determined by the sizes, alignments, and padding requirements of its fields.

A structure must not contain itself through by-value fields. Such a definition would require an object of infinite size.

For example, direct recursion is invalid:

```vol
struct Node {
    next: Node;
}
```

Arrays, strings, and structures are aggregate values.

Structure assignment, parameter passing, and return copy the structure value. Modifying a copied structure parameter does not modify the caller's structure:

```vol
fn voloid modify(Pair pair) {
    pair.first = 100;
}
```

### 4.8 Function declarations, calls, and calling semantics

A function must be declared before it is called.

A function may be declared without immediately providing a body:

```vol
fn voloid process(value: i32);
```

Multiple declarations of the same function are permitted if all declarations have identical function types.

At most one definition may be provided for a function.

Function overloading is not supported. Two functions cannot share a name while differing in parameter or return types.

A declaration without a corresponding definition is accepted by semantic analysis. If such a function is referenced, compilation later fails.

A call must name a declared function. Calling non-function values and method-call syntax are not supported.

Function arguments must match the declared parameter count and types.

Integer, Boolean, and structure parameters are passed by value.

Arrays and strings are represented by pointers when used as function parameters. They refer to the caller's existing storage, so modifications made through an array or string parameter are observable by the caller:

```vol
fn voloid set_first(i32[] values) {
    values[0] = 42;
}
```

Concrete array and string lengths are not encoded in function parameter types. Any object with the matching element type may be passed, regardless of its concrete length.

The called function must not access elements outside the bounds of the actual argument object. Such an access results in undefined behavior.

Concrete lengths are also not encoded in function return types. When an array or string is returned, the returned object and the caller-provided destination must have exactly matching concrete lengths. A mismatch results in undefined behavior and is not required to be diagnosed by the compiler.

Arrays and strings may also be returned through their unsized function return forms. Aggregate returns use caller-provided destination storage, and the returned value is copied into that storage.

An array or string literal cannot be returned directly. It must first initialize a named variable, and that variable may then be returned:

```vol
fn i32[] make_values() {
    i32[3] values = [1, 2, 3];
    return values;
}
```

A call that returns an aggregate must have valid destination storage. Discarding such a result as a standalone expression statement produces undefined behavior because the calling convention expects a location in which to place the returned aggregate.

The result of a function call must first be stored in a variable before it is used in an expression:

```vol
i32 first_value = first();
i32 second_value = second();
i32 result = first_value + second_value;
```

### 4.9 Control flow and return statements

The condition expressions of `if`, `while`, and `for` statements must have type `bool`.

For example:

```vol
if (count > 0) {
    process();
}
```

An integer value cannot be used directly as a condition:

```vol
if (count) {       // semantic error
    process();
}
```

A `break` statement may appear only inside a `while` or `for` loop.

A function with return type `voloid` may use:

```vol
return;
```

A `voloid` function must not return a value.

A function with a non-`voloid` return type must return a value whose type exactly matches the declared return type.

Control must not be able to reach the end of a non-`voloid` function without returning a value.

For example:

```vol
fn i32 valid(condition: bool) {
    if (condition) {
        return 1;
    } else {
        return 0;
    }
}
```

The following function is invalid because execution may reach its end without returning:

```vol
fn i32 invalid(condition: bool) {
    if (condition) {
        return 1;
    }
}
```

### 4.10 Definite initialization

A scalar local variable must be definitely initialized before its value is read.

Every scalar and structure parameter is considered definitely initialized when execution enters the function body.

A structure object is considered definitely initialized after at least one of its fields has been assigned.

Array and string parameters refer to caller-provided storage. The compiler does not consider the referenced object or its individual elements definitely initialized.

Initialization analysis takes control-flow branches into account. After an `if` statement, a variable is considered definitely initialized only if every possible branch initializes it.

For example:

```vol
i32 value;

if (condition) {
    value = 10;
} else {
    value = 20;
}

print_i32(value);  // valid
```

The following use is invalid:

```vol
i32 value;

if (condition) {
    value = 10;
}

print_i32(value);  // value may be uninitialized
```

Assignments performed inside a loop do not guarantee that a variable is initialized after the loop, because the loop body may execute zero times.

Arrays and strings are exempt from whole-object definite-initialization checking. Their storage exists immediately after declaration, and individual elements may be assigned in any order:

```vol
string[16] buffer;
buffer[0] = (u32)65;
buffer[1] = (u32)0;
```

The compiler does not track initialization separately for every array element, string element or struct field. Reading an object that has not previously been assigned produces an unspecified value.

### 4.11 Built-in `print` and program entry

The implementation provides a built-in function named `print`.

Its conceptual signature is:

```vol
fn voloid print(string[]...);
```

The function accepts one or more string arguments. Every argument must have a string type.

Arguments are written to standard output in their original order. No spaces, separators, or line terminators are inserted automatically.

For each argument, `print` writes bytes beginning at the start of the string and stops at the first null byte.

For example:

```vol
print("hello", " ", "world", "\n");
```

The `print` function does not validate or decode UTF-8. Runtime strings are raw byte arrays, and any byte sequence may be written to standard output.

A string passed to `print` must contain a null byte within its valid storage. If no null byte is present, `print` continues reading beyond the string object and the behavior is undefined.

The name `print` is predefined in the global scope and cannot be redeclared or shadowed.

A complete executable program must provide exactly one definition of `main` with the following signature:

```vol
fn i32 main()
```

The `main` function must not have parameters.

Execution begins by calling `main`. The value returned by `main` is used as the process exit status.

A program that does not define `main`, defines it more than once, or gives it an incompatible type is not a valid complete executable program.

### 4.12 Semantic diagnostics and implementation limits

Semantic analysis applies the following limits:

| Limit                                     |        Maximum |
| ----------------------------------------- | -------------: |
| Collected semantic errors                 |             16 |
| Length of one semantic diagnostic message | 256 characters |
| Nested block depth                        |            256 |

If a seventeenth semantic error is encountered, semantic analysis is terminated and compilation is aborted.

All previously collected semantic diagnostics are emitted when semantic analysis completes or is terminated.

## 5. Code generation

Code generation is performed after semantic analysis has completed successfully.

The compiler targets 64-bit x86 Linux and emits NASM assembly in the `elf64` format. The generated assembly may then be assembled with NASM and linked into an executable.

The backend is responsible for:

* assigning stack storage to local objects;
* generating addresses for lvalues;
* emitting scalar and aggregate loads, stores, and copies;
* implementing arithmetic, comparisons, casts, and control flow;
* arranging function arguments and return values;
* maintaining the required stack alignment;
* emitting the program entry point.

### 5.1 Object size and alignment

The size of an object is determined from its semantic type.

| Type           |                                 Size |                        Alignment |
| -------------- | -----------------------------------: | -------------------------------: |
| `bool`         |                              4 bytes |                          4 bytes |
| `i32`          |                              4 bytes |                          4 bytes |
| `u32`          |                              4 bytes |                          4 bytes |
| `i64`          |                              8 bytes |                          8 bytes |
| `u64`          |                              8 bytes |                          8 bytes |
| `string[N]`    |                            `N` bytes |                           1 byte |
| `T[N]`         |                      `N × sizeof(T)` |                     `alignof(T)` |
| Structure type | Computed from its fields and padding | Maximum required field alignment |

A structure field is placed at the next offset satisfying the alignment requirement of that field.

The total size of a structure includes any padding inserted between fields and any trailing padding required to make the complete structure suitably aligned.

The size of an array is the size of its element type multiplied by its element count. The alignment of an array is the alignment of its element type.

The `voloid` type does not represent an object and therefore requires no storage.

### 5.2 Stack storage

The source language provides no heap allocation and no pointer type.

Local variables, temporary aggregate values, saved intermediate values, and function-call argument areas are stored on the process stack.

Although the language does not expose pointers, the backend internally computes and passes memory addresses when implementing:

* lvalue access;
* array and string parameters;
* structure and aggregate copies;
* aggregate return values.

Each local object is assigned a fixed offset relative to the function frame pointer.

Objects are placed at offsets satisfying their alignment requirements. The total frame size is rounded up so that the required stack alignment is preserved.

Because no heap is available, the maximum amount of storage available to a program is constrained primarily by the process stack limit. That limit is determined by the execution environment and is not guaranteed by the language.

Allocating an object or call frame larger than the available stack may cause stack exhaustion and abnormal program termination.

### 5.3 Functions and stack alignment

Every generated function uses a stack frame.

The stack pointer is maintained at a 16-byte alignment at function-call boundaries. The size of each function frame and outgoing argument area is rounded up as necessary to preserve this alignment.

Scalar values are returned in registers:

* `bool`, `i32`, and `u32` values are returned through `eax`;
* `i64` and `u64` values are returned through `rax`.

Ordinary function arguments are placed in an outgoing stack area prepared by the caller.

Integer and Boolean arguments are copied by value.

Structure arguments are also passed by value. The caller copies the complete structure into the argument area before transferring control to the called function.

Arrays and strings are represented at the calling-convention level by the address of their existing storage. Their elements are not copied when they are passed as arguments.

### 5.4 Aggregate return values

Structures, arrays, and strings are aggregate values and are returned through caller-provided storage.

When calling a function that returns an aggregate, the caller passes an additional hidden argument containing the address of a modifiable destination object.

The called function copies its returned aggregate value into that destination.

Conceptually, a function such as:

```vol
fn Pair make_pair()
```

is implemented using a hidden destination parameter similar to:

```text
make_pair(address of result storage)
```

The hidden parameter is part of the backend calling convention and is not visible in the source language.

The destination must have enough storage for the complete returned object. Since concrete array and string lengths are not encoded in function return types, the compiler does not verify that the source and destination lengths agree.

If the actual lengths do not match, behavior is undefined.

An aggregate-returning call must be given valid destination storage. Discarding an aggregate return value leaves the function without the destination required by the calling convention and results in undefined behavior.

An array or string literal cannot be returned directly. It must first be stored in a local object:

```vol
fn i32[] values() {
    i32[3] result = [1, 2, 3];
    return result;
}
```

The returned object is copied into caller-provided storage before the called function's stack frame is destroyed.

### 5.5 Integer arithmetic

Arithmetic instructions are selected according to operand width and signedness.

Addition, subtraction, and multiplication operate on either 32-bit or 64-bit registers.

Division and remainder use signed or unsigned x86-64 division instructions:

* signed `i32` division uses sign extension followed by `idiv`;
* signed `i64` division uses sign extension followed by `idiv`;
* unsigned division clears the high half of the dividend before using `div`.

Division or remainder by zero causes a runtime divide exception for every integer type.

For signed integers, dividing or taking the remainder of the minimum representable value by `-1` also causes a runtime divide exception. This applies to both `i32` and `i64`.

On the target Linux environment, a processor divide exception normally terminates the program with a division-related runtime signal. No language-level recovery mechanism is provided.

### 5.6 Integer overflow and casts

The compiler does not emit general runtime checks for integer overflow.

Overflow behavior follows the behavior of the emitted fixed-width x86-64 instructions. NASM only encodes those instructions; the actual arithmetic behavior is defined by the target processor.

For ordinary addition, subtraction, and multiplication, only the low 32 or 64 result bits are retained.

A cast from a 32-bit integer to a 64-bit integer performs either sign extension or zero extension according to the source type.

A cast from a 64-bit integer to a 32-bit integer discards the upper 32 bits.

A cast between signed and unsigned types of the same width preserves the underlying bit representation.

Unary `-` is generated only for signed integer types. The semantic analyzer rejects its application to `u32` or `u64`.

### 5.7 Control flow

Conditional and loop statements are implemented using generated labels and conditional or unconditional jumps.

A Boolean condition is compared with zero:

* zero represents `false`;
* one represents `true`.

The logical operators `&&` and `||` use control flow to implement short-circuit evaluation.

For `&&`, the right operand is skipped when the left operand is false.

For `||`, the right operand is skipped when the left operand is true.

A `break` statement jumps to the end label of the nearest enclosing loop.

Each function has a common return label. A generated `return` statement places the result in the required register or aggregate destination and transfers control to the function epilogue.

### 5.8 Runtime facilities

The language does not provide:

* heap allocation;
* source-level pointer types;
* garbage collection;
* exceptions;
* runtime array-bounds checks;
* runtime integer-overflow checks;
* automatic stack-growth management.

Invalid memory access, stack exhaustion, division exceptions, and other processor or operating-system faults terminate the program according to the behavior of the target environment.