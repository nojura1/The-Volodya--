# Volodya--

Volodya-- is a small statically typed imperative programming language with an ahead-of-time compiler targeting 64-bit Linux.

The compiler translates Volodya-- source code into NASM and can either emit the assembly directly or assemble and link it into an executable.

## Status

Version **1.0.0**.

The language, compiler pipeline, NASM backend, examples, tests, and language specification are implemented.

## Features

- Static, strong, nominal, and explicit typing
- Signed and unsigned 32-bit and 64-bit integers
- Boolean values
- Fixed-size strings and one-dimensional arrays
- User-defined structures
- Functions, declarations, recursion, and aggregate returns
- `if`, `else`, `while`, `for`, `break`, and `return`
- Explicit integer casts
- Definite-initialization analysis
- Short-circuit `&&` and `||`
- Built-in variadic `print`
- NASM x86-64 code generation
- Automatic assembling and linking on Linux

## Example

```vol
fn u32 char(text: string[]) {
    return text[0];
}

fn i32 main() {
    string[12] buffer;
    string[2] current = "1";

    for (i32 i = 0; i < 11; i = i + 1) {
        buffer[i] = char(current);
        current[0] = current[0] + (u32)1;
    }

    buffer[11] = (u32)0;
    print(buffer, "\n");

    return 0;
}
```

The program prints:

```text
123456789:;
```

## Requirements

The compiler currently targets 64-bit Linux and requires:

- a C compiler with C23 support
- GNU Make
- NASM
- GNU `ld`

On Debian or Ubuntu, the required tools can be installed with:

```sh
sudo apt install build-essential nasm binutils
```

## Building

Build the compiler from the repository root:

```sh
make
```

The resulting compiler executable is:

```text
./vmm
```

Create a debug build with AddressSanitizer and UndefinedBehaviorSanitizer:

```sh
make debug
```

Remove generated files:

```sh
make clean
```

Rebuild everything:

```sh
make rebuild
```

## Usage

Compile a Volodya-- source file into an executable:

```sh
./vmm input.vol
```

Choose the output filename:

```sh
./vmm input.vol -o program
```

Run the resulting executable:

```sh
./program
```

Emit NASM assembly without assembling or linking:

```sh
./vmm input.vol -S -o program.asm
```

`-nasm` is an alias for `-S`:

```sh
./vmm input.vol -nasm -o program.asm
```

Only one input file is supported.

## Compilation pipeline

```text
source file
    |
    v
lexical analysis
    |
    v
parsing
    |
    v
semantic analysis
    |
    v
NASM x86-64 code generation
    |
    v
nasm -f elf64
    |
    v
GNU ld
    |
    v
executable
```

When `-S` or `-nasm` is used, compilation stops after NASM assembly has been emitted.

## Language specification

The complete lexical, syntactic, semantic, and code-generation specification is available in:

```text
docs/spec.md
```

The specification defines the language grammar, type system, object layout, diagnostics, translation limits, calling convention, undefined behavior, and runtime restrictions.

## Current limitations

- Linux x86-64 is the only target
- NASM and GNU `ld` are required
- Only one source file may be compiled at a time
- No heap allocation
- No source-level pointer types
- No multidimensional arrays
- No arrays of strings
- No runtime bounds checking
- No runtime integer-overflow checking
- No exceptions or garbage collection
- Function calls cannot be operands of larger expressions

## Running tests

Run the configured test programs with:

```sh
make test
```

## Version

Volodya-- 1.0.0