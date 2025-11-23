# Laotzus

A self-made tiny compiler in __Pascal__ language.

This compiler translates a custom programming language (.dao) into executable code.
The project showcases compiler design concepts including lexical analysis, parsing, semantic analysis, and code generation.

## Table of Contents
- [Introduction](#introduction)
- [Features](#features)
- [Installation](#installation)
- [Usage](#usage)
- [Example](#example)

## Introduction

The name of the compiler __Laotzus__ came from the idea of an ancient Chinese philosopher,
[Laozi](https://en.wikipedia.org/wiki/Laozi), whose ideology profoundly influenced me.
Thus, I'm writing a compiler on his name.

The __Laotzus__ compiler is a project designed to demonstrate the basic components and functionalities of a compiler.
It is developed using Free Pascal (FPC) and includes the following stages:
- **Lexical Analysis**: Tokenizing the input source code.
- **Parsing**: Constructing the syntax tree from tokens.
- **Semantic Analysis**: Checking for semantic errors.
- **Code Generation**: Generating target code from the syntax tree.

## Features

- **Lexical Analyzer**: Converts source code into tokens.
- **Parser**: Constructs a syntax tree from tokens.
- **Semantic Analyzer**: Ensures code adheres to language rules.
- **Code Generator**: Produces executable code.

## Installation

To install and run the Pascal Compiler, you need to have Free Pascal (FPC) installed on your system. You can download and install FPC from [the official website](https://www.freepascal.org/).

1. Clone the repository:
    ```sh
    git clone <repo>
    ```
2. Navigate to the project directory:
    ```sh
    cd <project-directory>
    ```
3. Compile the compiler:
    ```sh
    make
    ```

## Usage

After compiling, you can run the compiler from the command line.
The compiler takes a source file as input and generates an assembly code file.
And be noticed, the source file must be of __.dao__ ending.

    ```sh
    ./Laotzus <sourcefile>
    ```

After the compilation, go to the same directory of the souce file,
and you'll find the compiled output file which is of __.asm__ ending and compatible with NASM syntax.


## Example

Here is an example of how to use the compiler:

1. Create a souce file like __probe.dao__ in the directory _example_:
```pascal
    (*-----------------------------------------*)
    program HelloWorld; // Program Entrance Name...
    (*-----------------------------------------*)
    type
        i8ptr = @i8,
        i8pptr = @i8ptr,
        u8ptr = @u8,
        letter = char,
        Book1 = structure begin
            bookId: i8,
            bookName: @char,
            bookAuthor: @ char,
        end,
        Book2 = structure begin
            bookId: i8,
            bookName: @char,
            bookAuthor: @ char,
            smallBook: Book1,
        end;
    (*-----------------------------------------*)
    var
        c1: char,
        c2: char,
        c3: char,
        book: Book1;
    (*-----------------------------------------*)
    val
        MAGIC1 = -20 * 11 + (1111),
        MAGIC2: i8 = 30;
    (*-----------------------------------------*)
    var
        i: i8,
        j: i8,
        k: i8,
        iptr: @i8,
        uptr: @u16,
        count: u16;
    (*-----------------------------------------*)
    procedure hello(t1: char, t2: char, t3: i16, t4: i16, t5:u8);
    var
        v1: @char,
        v2: u16;
    val
        X1: i8 = t1,
        X2: u16 = 100,
        X3: u8 = 1;
    begin
        i := 1;
        v1 := 2;

        iptr := @i;

        v2 := v1 + t1 + t2 + t3 * 100;
    end;
    (*-----------------------------------------*)
    function min(n1: i8, n2: i8): i8;
    var
        temp: i8;
    begin
        return n1;
    end;
    (*-----------------------------------------*)
    begin

        count := 0;

        i := 1 + 2 + 3;

        j := 2 * 6 + (1 - 5 / 7 + 11) * 4;

        count := count + 1;

        hello(c1, c2, count, 1, 1);

        uptr := @ count;

        i :=  min(18, 19);

        min(-1, -2);

    end.
    (*-----------------------------------------*)
```

2. Run the compiler:
```sh
    ./Laotzus example/probe.dao
```

3. Read the generated assembly code:
```asm
    	jmp HelloWorld
        c1:
            resb 1
        c2:
            resb 1
        c3:
            resb 1
        book:
            resb 5
        MAGIC1:
            db 0x0000037B
        MAGIC2:
            db 0x0000001E
        i:
            resb 1
        j:
            resb 1
        k:
            resb 1
        iptr:
            resb 2
        uptr:
            resb 2
        count:
            resb 2
    hello:
        push bp
        mov bp, sp
        sub sp, 8
        mov ax, byte [bp + 10]
        mov byte [bp - 5], ax
        mov ax, 100
        mov word [bp - 7], ax
        mov ax, 1
        mov byte [bp - 8], ax
        mov ax, 1
        mov byte [i], ax
        mov ax, 2
        mov word [bp - 2], ax
        lea ax, [i]
        mov word [iptr], ax
        mov ax, word [bp - 2]
        push ax
        mov ax, byte [bp + 10]
        pop cx
        add ax, cx
        push ax
        mov ax, byte [bp + 9]
        pop cx
        add ax, cx
        push ax
        mov ax, word [bp + 7]
        push ax
        mov ax, 100
        pop cx
        mul ax, cx
        pop cx
        add ax, cx
        mov word [bp - 4], ax
    .exit
        mov sp, bp
        pop bp
        ret
    min:
        push bp
        mov bp, sp
        sub sp, 1
        mov ax, byte [bp + 5]
        push ax
        lea ax, [bp + 6]
        pop cx
        mov word [ax], cx
        jmp .exit
    .exit
        lea ax, [bp + 6]
        mov sp, bp
        pop bp
        ret
    HelloWorld:
        push bp
        mov bp, sp
        sub sp, 2
        mov ax, 0
        mov word [count], ax
        mov ax, 1
        push ax
        mov ax, 2
        pop cx
        add ax, cx
        push ax
        mov ax, 3
        pop cx
        add ax, cx
        mov byte [i], ax
        mov ax, 2
        push ax
        mov ax, 6
        pop cx
        mul ax, cx
        push ax
        mov ax, 1
        push ax
        mov ax, 5
        push ax
        mov ax, 7
        pop cx
        div cx, ax
        mov ax, cx
        pop cx
        sub ax, cx
        neg ax
        push ax
        mov ax, 11
        pop cx
        add ax, cx
        push ax
        mov ax, 4
        pop cx
        mul ax, cx
        pop cx
        add ax, cx
        mov byte [j], ax
        mov ax, word [count]
        push ax
        mov ax, 1
        pop cx
        add ax, cx
        mov word [count], ax
        mov ax, byte [c1]
        push ax
        mov ax, byte [c2]
        push ax
        mov ax, word [count]
        push ax
        mov ax, 1
        push ax
        mov ax, 1
        push ax
        call hello
        add sp, 11
        lea ax, [count]
        mov word [uptr], ax
        lea ax, [bp - 1]
        push ax
        mov ax, 18
        push ax
        mov ax, 19
        push ax
        call min
        add sp, 8
        mov byte [i], ax
        lea ax, [bp - 1]
        push ax
        mov ax, 1
        neg ax
        push ax
        mov ax, 2
        neg ax
        push ax
        call min
        add sp, 8
    .exit:
        mov sp, bp
        pop bp
```

