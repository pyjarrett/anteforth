# anteforth

An attempt to write a verified Forth interpreter.

This project is a fork of [postfix_calc](https://github.com/pyjarrett/postfix_calc)
to hopefully expand it into a verified Forth intepreter.

## Execution

Run with Alire:

```
alr run
```

## Implemented and Verified Words

* `+`, `-`, `*`, `/` - basic arithmetic operations
* `.` - pop and print the top element
- `negate` - negate the top element
- `over` - copy element one below the top to the top
- `swap` - swap the top two elements
- `rot` - rotate top 3 elements
- `dup` - duplicate the top element
- `dump` - dump the stack
- `reset` - reset any error conditions

## Running proofs

Install `gnatprove` with Alire:

```
alr with gnatprove
```

Then run the provers.

```
alr gnatprove --level=2
```

Speed up evaluation using parallel analysis with the `-j` flag, like `-j12` if
you have 12 logical cores on your machine.
