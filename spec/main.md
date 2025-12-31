# AnteForth

AnteForth is a project to create a verified Forth virtual machine.

## Goal
The goal is to prove the viability of creating a Forth virtual machine in SPARK at the Gold level of functional correctness.  This is a high level
spec used to check project progress.

### Outside of Scope

The intent is not to emit native machine code at this time.

## Architecture

Dynamically allocated memory cannot be used in a SPARK program.  This requires
the usage of statically allocated blocks for data.  Each of these is
considered its own "address space" for proving access.

- *parameter stack* - Cells used to record temporary data for parameters.  Also
  called the "data stack". Used during compilation of control flow words to
  store stub data as needed.
- *return stack* - Stack storing return addresses of the callstack.
- *string block* - a block dedicated to the storage of character data which is
  accessed via `[start,end)` ranges.
- *instruction block* - a block dedicated to storing compiled words. This includes
  execution tokens and their operands (like branch offsets or literal values).
  This is also called "code space."
- *V&C block* - storage for variables and constants
- *word block* - storage of the base word data (word headers), with ranges for
  names pointing into the string table, and data sections indexing into variable,
  constant, instruction storage, or to built-in functions as appropriate.
  Built-in and user-defined words are all defined in word storage.
- *terminal input buffer (tib)* - the program's interface to receiving character
  data from the user outside of the program.

User and system variables are unique to each Forth "machine".

- *system variables* - variables used by the entire Forth implementation
- *user variable* - sort of like a thread-local variable for Forth

## Glossary

- Built-in words are also referred to as "standard words"

## Operation

The machine provides both a text and an address interpreter.

The **text interpreter** reads the next word off the terminal input buffer (TIB) and
then looks it up and executes it.  It repeats this process until the TIB has
been exhausted.

The **address interpreter** maintains a current "instruction pointer" (IP), which is
the index of the next instruction to execute within the instruction block.
This interpreter continues running each execution token and then incrementing
the instruction pointer until the return stack is emptied.

## Data Types and Transforms

* `cell` - a unit of data storage, sufficient to store one execution token or
  integer. Defined as a 64-bit signed integer.
* `execution token` - a unique identifier which can be used to execute a word
* `word` - the base unit of execution. Each word has a unique `execution token`.
  An immediate word is always executed even when encountered when compiling.
  A word has an associated type, that being a variable, constant, built-in, or
  starting execution token within the instruction block.

## Parameter stack

A stack of `Cell`s which can be manipulated.  Most word operations interact
with the parameter stack.

- `.` - pop and print the top element
- `.S` - dump the stack without modifying it
- `+`, `-`, `*`, `/` - basic arithmetic operations
- `NEGATE` - negate the top element
- `OVER` - copy element one below the top to the top
- `SWAP` - swap the top two elements
- `ROT` - rotate top 3 elements
- `DUP` - duplicate the top element
- `DROP` - discard the top element
- `SP@` - the index of the stack top before `SP@` was called
- `SP0` - the index of the bottom of the stack
- `AND` - bitwise AND
- `OR` - bitwise OR
- `0=` - return TRUE if the top of the stack is 0
- `0<` - return TRUE if the top of the stack is less than 0
- `0>` - return TRUE if the top of the stack is greater than 0
- `0<>` - return TRUE if the top of the stack is not 0
- `=` - return TRUE if the top two elements are equal
- `<` - return TRUE if the second element is less than the top
- `>` - return TRUE if the second element is greater than the top
- `<>` - return TRUE if the top two elements are not equal

## Return stack

A stack of instruction indices into the instruction block which can only be
pushed and popped.  Execution of the address interpreter is complete when `EXIT`
is called and the last index is popped off of the return stack.
Before an instruction is executed, its return address is pushed onto this stack.

- `>R` - pops the data stack and pushes this value onto the return stack
- `R>` - pops the return stack and pushes this value onto the data stack
- `EXIT` - returns control to the parent calling definition
- `?EXIT` - return control to the parent calling function if the top of the
  stack is false

## Input Functions

- `KEY` ( -- char ) - Waits for and returns the next available character from the input device. This is an unbuffered operation and does not interact with the TIB.
- `ACCEPT` ( c-addr +n1 -- +n2 ) - Reads up to +n1 characters from the input device into the buffer at c-addr. Returns the actual number of characters received (+n2). Used to fill the TIB.

### Terminal Input Buffer (TIB)

The terminal input buffer is the program's interface to receive buffered line input from the outside world.

- `TIB` ( -- addr ) - Returns the address of the start of the terminal input buffer.
- `#TIB` ( -- addr ) - A variable holding the number of characters in the TIB.
- `>IN` ( -- addr ) - A variable holding the offset of the next character to be parsed in the TIB.

## Output Functions

The machine writes to a separate output buffer which can then be read and
cleared.

- `EMIT` - prints the character representation of the top of the data stack
- `CR` - emits a carriage return
- `SPACE` - emits a space
- `TYPE` - prints a string given by address and length on the stack

## Memory Operations

Access to the V&C block (Variables and Constants).

- `@` - fetch value from address
- `!` - store value to address
- `+!` - add value to the value at address

## Defining Words

- `VARIABLE` - create a variable in the V&C block
- `CONSTANT` - create a constant in the V&C block

## Dictionary Construction

- `,` - reserve one cell of instruction space (at HERE) and store the top of the stack there. Used for compiling operands.
- `ALLOT` - reserve n cells of V&C space.

## Interpretation

### Compilation

Some words are used to control the program state:

- [`:`](https://forth-standard.org/standard/core/Colon) - enters compilation mode
- [`;`](https://forth-standard.org/standard/core/Semi) - ends a definition.
- [`\]`](https://forth-standard.org/standard/core/Bracket) - enters interpretation state
- `[`

Within compilation these words are valid:

- [`IF`](https://forth-standard.org/standard/core/IF)
- `THEN`
- `ELSE`
- `BEGIN` - marks the beginning of a loop
- `UNTIL`
- `DO`
- `LOOP`
- `BEGIN`
- `UNTIL`
- `WHILE`
- `REPEAT`
- `RECURSE` - support recursive calls during definition

## Dictionary Interaction

- `'` - find execution token of next word
- `EXECUTE` - execute the token on top of stack
- `[` - enter interpretation state
- `]` - enter compilation state
- `IMMEDIATE` - mark most recent word as immediate

## Input Parsing

- `WORD` - parse the next word from the input stream.
- `NUMBER?` - attempt to convert a string to a number.

## Address Interpretation

In address interpretation, these words are valid:

- `BRANCH` - unconditional jump by the relative amount given in the next cell.
- `0BRANCH` - jump if the top of the stack is zero (false).
- `LIT` - push the value in the next cell onto the stack.
- `(DO)` - runtime for `DO`: pop limit and index from data stack, push to return stack.
- `(LOOP)` - runtime for `LOOP`: increment index, check limit, branch or exit loop.

## Comments

- `(` - comment until `)`
- `\` - comment until end of line

## Error Reporting

The machine reports errors as an outwardly visible state.

- `Stack_Overflow` - the data stack ran out of space
- `Stack_Underflow` - the data stack did not have enough elements
- `Unknown_Word` - a word being interpreted was not recognized
- `Resources_Exhausted` - one of the blocks within the machine ran out of space

## System Variables

The state of the interpreter is maintained by several global variables.

- `STATE` - Is the interpreter compiling (non-zero) or interpreting (zero)?
- `HERE` - The next available index in the Instruction Block.
- `LATEST` - The index of the most recently defined word in the Word Block.
- `BASE` - The current number base for input and output conversion.
- `VHERE` - The next available index in the V&C Block.

## Dictionary Structure

The "Word Block" is an array of records representing the dictionary headers.

- *Link Field* - Index of the previous word in the dictionary (linked list).
- *Name Field* - Index and length into the String Block.
- *Immediate* - Whether the word should be immediately executed, even in compilation mode.
- *Behavior* - Determines if the word is a primitive (Ada procedure), a constant,
  a variable, or a compiled colon definition.
