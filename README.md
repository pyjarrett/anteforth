# anteforth

![image](https://img.shields.io/badge/2022-inside-green?logo=ada&logoColor=or&logoSize=auto)

An attempt to write a verified Forth virtual machine.

This project is a fork of [postfix_calc](https://github.com/pyjarrett/postfix_calc)
to hopefully expand it into a verified Forth virtual machine.

## Execution

Run a with Alire:

```
alr run
```

Run with a script to execute with Alire:

```
alr run --args="myfile.fth"
```

## Properties

The virtual machine will verify that only valid words are execute.  Allocations
for words and names for words will always be valid, as will all instructions
written during compilation.

Runtime properties of certain properties of various other words.

## Implemented and Verified Words

* `RESET` - reset any error conditions
* `WORDS` - prints all available words
* `.` - pop and print the top element
* `.S` - dump the stack without modifying it
* `+`, `-`, `*`, `/` - basic arithmetic operations
* `NEGATE` - negate the top element
* `OVER` - copy element one below the top to the top
* `SWAP` - swap the top two elements
* `ROT` - rotate top 3 elements
* `DUP` - duplicate the top element
* `(` - a comment terminated by `)`
* `:` - begin a word definition
* `;` - terminate a definition
* `EXIT` - return from a function definition
* `=`, `>`, `<` - comparison operations
* `0=`, `0>`, `0<` - zero comparison operations
* `IF` - two instructions written
* `THEN` - jump is verified to be valid
* `ELSE`

## Running proofs

Install `gnatprove` with Alire:

```
alr with gnatprove
```

Then run the provers.

```
alr gnatprove -j12 --level=2 --steps=20000 --counterexamples=on
```

Speed up evaluation using parallel analysis with the `-j` flag, like `-j12` if
you have 12 logical cores on your machine.
