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

This is a single cell Forth, which only operates on 32-bit integers.

The virtual machine will verify that only valid words are execute.  Allocations
for words and names for words will always be valid, as will all instructions
written during compilation.

Runtime properties of certain properties of various other words.  Additional
behaviors are proved, such as that the terminal input buffer subprograms always
terminates.

## Implemented and Verified Words

Some words have their compile-time behavior checked and prevent introducing
errors (like bad jumps), while others ensure that all errors are caught
at runtime.

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
* `DROP` - remove the top element
* `(` - a comment terminated by `)`
* `:` - begin a word definition
* `;` - terminate a definition
* `EXIT` - return from a function definition
* `?EXIT` - conditional exit (returns when data stack top is 0)
* `=`, `>`, `<` - comparison operations
* `0=`, `0>`, `0<` - zero comparison operations
* `IF` - two instructions written
* `THEN`, `ELSE` - jumps are verified to be valid
* `BEGIN`, `UNTIL`
* `>R`, `R>` - catchs stack overflow/underflow
* `RECURSE` - verifies instruction to current word is written

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

## Proof

```
=========================
Summary of SPARK analysis
=========================

------------------------------------------------------------------------------------------------------------
SPARK Analysis results        Total         Flow                              Provers   Justified   Unproved
------------------------------------------------------------------------------------------------------------
Data Dependencies                33           33                                    .           .          .
Flow Dependencies                 1            1                                    .           .          .
Initialization                   45           45                                    .           .          .
Non-Aliasing                      .            .                                    .           .          .
Run-time Checks                 360            .    360 (CVC5 92%, Trivial 6%, Z3 2%)           .          .
Assertions                       34            .                            34 (CVC5)           .          .
Functional Contracts            373            .    373 (CVC5 91%, Trivial 7%, Z3 2%)           .          .
LSP Verification                 34            .                            34 (CVC5)           .          .
Termination                      61           59                             2 (CVC5)           .          .
Concurrency                       .            .                                    .           .          .
------------------------------------------------------------------------------------------------------------
Total                           941    138 (15%)                            803 (85%)           .          .


max steps used for successful proof: 19358

============================
Most difficult proved checks
============================

No check found with max time greater than 1 second

========================
Detailed analysis report
========================

Analyzed 4 units
in unit anteforth, 0 subprograms and packages out of 0 analyzed
in unit terminal_input_buffers, 31 subprograms and packages out of 31 analyzed
  Terminal_Input_Buffers at terminal_input_buffers.ads:5 flow analyzed (0 errors, 0 checks, 0 warnings and 0 pragma Assume statements) and proved (2 checks)
  Terminal_Input_Buffers."=" at terminal_input_buffers.ads:164 flow analyzed (0 errors, 0 checks, 0 warnings and 0 pragma Assume statements) and proved (0 checks)
  Terminal_Input_Buffers.Char at terminal_input_buffers.adb:4 flow analyzed (0 errors, 0 checks, 0 warnings and 0 pragma Assume statements) and proved (1 checks)
  Terminal_Input_Buffers.Char at terminal_input_buffers.adb:9 flow analyzed (0 errors, 0 checks, 0 warnings and 0 pragma Assume statements) and proved (1 checks)
  Terminal_Input_Buffers.Contains at terminal_input_buffers.ads:99 flow analyzed (0 errors, 0 checks, 0 warnings and 0 pragma Assume statements) and proved (1 checks)
  Terminal_Input_Buffers.Contains at terminal_input_buffers.ads:100 flow analyzed (0 errors, 0 checks, 0 warnings and 0 pragma Assume statements) and proved (0 checks)
  Terminal_Input_Buffers.End_Cursor_Index at terminal_input_buffers.ads:211 flow analyzed (0 errors, 0 checks, 0 warnings and 0 pragma Assume statements) and proved (0 checks)
  Terminal_Input_Buffers.Has_Input at terminal_input_buffers.ads:43 flow analyzed (0 errors, 0 checks, 0 warnings and 0 pragma Assume statements) and proved (0 checks)
  Terminal_Input_Buffers.Has_Lexeme at terminal_input_buffers.ads:69 flow analyzed (0 errors, 0 checks, 0 warnings and 0 pragma Assume statements) and proved (0 checks)
  Terminal_Input_Buffers.Has_More_Characters at terminal_input_buffers.ads:55 flow analyzed (0 errors, 0 checks, 0 warnings and 0 pragma Assume statements) and proved (1 checks)
  Terminal_Input_Buffers.Has_Next at terminal_input_buffers.ads:121 flow analyzed (0 errors, 0 checks, 0 warnings and 0 pragma Assume statements) and proved (1 checks)
  Terminal_Input_Buffers.Has_Valid_Cursor at terminal_input_buffers.ads:214 flow analyzed (0 errors, 0 checks, 0 warnings and 0 pragma Assume statements) and proved (0 checks)
  Terminal_Input_Buffers.Ignore_Lexeme at terminal_input_buffers.ads:142 flow analyzed (0 errors, 0 checks, 0 warnings and 0 pragma Assume statements) and proved (2 checks)
  Terminal_Input_Buffers.Image at terminal_input_buffers.ads:92 flow analyzed (0 errors, 0 checks, 0 warnings and 0 pragma Assume statements) and proved (2 checks)
  Terminal_Input_Buffers.Input_Size at terminal_input_buffers.ads:45 flow analyzed (0 errors, 0 checks, 0 warnings and 0 pragma Assume statements) and proved (0 checks)
  Terminal_Input_Buffers.Input_Unchanged at terminal_input_buffers.ads:47 flow analyzed (0 errors, 0 checks, 0 warnings and 0 pragma Assume statements) and proved (0 checks)
  Terminal_Input_Buffers.Is_Number at terminal_input_buffers.ads:80 flow analyzed (0 errors, 0 checks, 0 warnings and 0 pragma Assume statements) and proved (8 checks)
  Terminal_Input_Buffers.Is_Word at terminal_input_buffers.ads:78 flow analyzed (0 errors, 0 checks, 0 warnings and 0 pragma Assume statements) and proved (1 checks)
  Terminal_Input_Buffers.Lexeme_Range at terminal_input_buffers.ads:175 flow analyzed (0 errors, 0 checks, 0 warnings and 0 pragma Assume statements) and proved (0 checks)
  Terminal_Input_Buffers.Lexeme_Size at terminal_input_buffers.ads:72 flow analyzed (0 errors, 0 checks, 0 warnings and 0 pragma Assume statements) and proved (2 checks)
  Terminal_Input_Buffers.Load_Input at terminal_input_buffers.ads:107 flow analyzed (0 errors, 0 checks, 0 warnings and 0 pragma Assume statements) and proved (17 checks)
  Terminal_Input_Buffers.Next at terminal_input_buffers.ads:153 flow analyzed (0 errors, 0 checks, 0 warnings and 0 pragma Assume statements) and proved (4 checks)
  Terminal_Input_Buffers.Next_Token at terminal_input_buffers.ads:128 flow analyzed (0 errors, 0 checks, 0 warnings and 0 pragma Assume statements) and proved (31 checks)
  Terminal_Input_Buffers.Peek at terminal_input_buffers.ads:151 flow analyzed (0 errors, 0 checks, 0 warnings and 0 pragma Assume statements) and proved (1 checks)
  Terminal_Input_Buffers.Remaining_Characters at terminal_input_buffers.ads:61 flow analyzed (0 errors, 0 checks, 0 warnings and 0 pragma Assume statements) and proved (1 checks)
  Terminal_Input_Buffers.Skip_Whitespace at terminal_input_buffers.ads:248 flow analyzed (0 errors, 0 checks, 0 warnings and 0 pragma Assume statements) and proved (7 checks)
  Terminal_Input_Buffers.Take_Lexeme at terminal_input_buffers.ads:283 flow analyzed (0 errors, 0 checks, 0 warnings and 0 pragma Assume statements) and proved (12 checks)
  Terminal_Input_Buffers.Terminal_Input_Buffer at terminal_input_buffers.ads:191 flow analyzed (0 errors, 0 checks, 0 warnings and 0 pragma Assume statements) and proved (1 checks)
  Terminal_Input_Buffers.Terminal_Input_Bufferpredicate at terminal_input_buffers.ads:191 flow analyzed (0 errors, 0 checks, 0 warnings and 0 pragma Assume statements) and proved (0 checks)
  Terminal_Input_Buffers.To_Number at terminal_input_buffers.ads:84 flow analyzed (0 errors, 0 checks, 0 warnings and 0 pragma Assume statements) and proved (12 checks)
  Terminal_Input_Buffers.Width at terminal_input_buffers.ads:19 flow analyzed (0 errors, 0 checks, 0 warnings and 0 pragma Assume statements) and proved (1 checks)
in unit vms, 53 subprograms and packages out of 53 analyzed
  VMS at vms.ads:4 flow analyzed (0 errors, 0 checks, 0 warnings and 0 pragma Assume statements) and proved (0 checks)
  VMS."=" at vms.ads:92 flow analyzed (0 errors, 0 checks, 2 warnings and 0 pragma Assume statements) and proved (0 checks)
  VMS.Allocate_Word at vms.ads:550 flow analyzed (0 errors, 0 checks, 0 warnings and 0 pragma Assume statements) and proved (22 checks)
  VMS.Append_Instruction at vms.ads:230 flow analyzed (0 errors, 0 checks, 0 warnings and 0 pragma Assume statements) and proved (6 checks)
  VMS.Can_Allocate_Name at vms.ads:546 flow analyzed (0 errors, 0 checks, 0 warnings and 0 pragma Assume statements) and proved (0 checks)
  VMS.Can_Allocate_Word at vms.ads:543 flow analyzed (0 errors, 0 checks, 0 warnings and 0 pragma Assume statements) and proved (0 checks)
  VMS.Can_Append_Instructions at vms.ads:225 flow analyzed (0 errors, 0 checks, 0 warnings and 0 pragma Assume statements) and proved (0 checks)
  VMS.Dump_Param_Stack at vms.ads:275 flow analyzed (0 errors, 0 checks, 0 warnings and 0 pragma Assume statements) and proved (1 checks)
  VMS.Dump_VM at vms.ads:276 flow analyzed (0 errors, 0 checks, 0 warnings and 0 pragma Assume statements) and proved (22 checks)
  VMS.Dump_VM.Safe_Col at vms.adb:52 flow analyzed (0 errors, 0 checks, 0 warnings and 0 pragma Assume statements) and proved (3 checks)
  VMS.Exec at vms.ads:510 flow analyzed (0 errors, 0 checks, 0 warnings and 0 pragma Assume statements) and proved (23 checks)
  VMS.Execute at vms.ads:577 flow analyzed (0 errors, 0 checks, 0 warnings and 0 pragma Assume statements) and proved (18 checks)
  VMS.Is_Compiling at vms.ads:209 flow analyzed (0 errors, 0 checks, 0 warnings and 0 pragma Assume statements) and proved (0 checks)
  VMS.Is_Interpreting at vms.ads:213 flow analyzed (0 errors, 0 checks, 0 warnings and 0 pragma Assume statements) and proved (0 checks)
  VMS.Is_Running at vms.ads:186 flow analyzed (0 errors, 0 checks, 0 warnings and 0 pragma Assume statements) and proved (0 checks)
  VMS.Is_Stopped at vms.ads:190 flow analyzed (0 errors, 0 checks, 0 warnings and 0 pragma Assume statements) and proved (0 checks)
  VMS.Is_Valid_IP at vms.ads:217 flow analyzed (0 errors, 0 checks, 0 warnings and 0 pragma Assume statements) and proved (0 checks)
  VMS.Is_Valid_Jump at vms.ads:221 flow analyzed (0 errors, 0 checks, 0 warnings and 0 pragma Assume statements) and proved (0 checks)
  VMS.Is_Word at vms.ads:574 flow analyzed (0 errors, 0 checks, 0 warnings and 0 pragma Assume statements) and proved (0 checks)
  VMS.Lookup at vms.ads:571 flow analyzed (0 errors, 0 checks, 0 warnings and 0 pragma Assume statements) and proved (1 checks)
  VMS.Only_Instructions_Changed at vms.ads:393 flow analyzed (0 errors, 0 checks, 0 warnings and 0 pragma Assume statements) and proved (0 checks)
  VMS.Only_Param_Stack_Changed at vms.ads:365 flow analyzed (0 errors, 0 checks, 0 warnings and 0 pragma Assume statements) and proved (0 checks)
  VMS.Only_Status_Changed at vms.ads:351 flow analyzed (0 errors, 0 checks, 0 warnings and 0 pragma Assume statements) and proved (0 checks)
  VMS.Only_TIB_Changed at vms.ads:407 flow analyzed (0 errors, 0 checks, 0 warnings and 0 pragma Assume statements) and proved (0 checks)
  VMS.Only_Words_Changed at vms.ads:379 flow analyzed (0 errors, 0 checks, 0 warnings and 0 pragma Assume statements) and proved (0 checks)
  VMS.Op_Procedure at vms.ads:71 flow analyzed (0 errors, 0 checks, 0 warnings and 0 pragma Assume statements) and proved (0 checks)
  VMS.Param_Multipop at vms.ads:471 flow analyzed (0 errors, 0 checks, 0 warnings and 0 pragma Assume statements) and proved (10 checks)
  VMS.Param_Peek at vms.ads:430 flow analyzed (0 errors, 0 checks, 0 warnings and 0 pragma Assume statements) and proved (1 checks)
  VMS.Param_Peek at vms.ads:434 flow analyzed (0 errors, 0 checks, 0 warnings and 0 pragma Assume statements) and proved (1 checks)
  VMS.Param_Pop at vms.ads:455 flow analyzed (0 errors, 0 checks, 0 warnings and 0 pragma Assume statements) and proved (11 checks)
  VMS.Param_Push at vms.ads:438 flow analyzed (0 errors, 0 checks, 0 warnings and 0 pragma Assume statements) and proved (9 checks)
  VMS.Param_Stack_Equal_From_Bottom_Until at vms.ads:282 flow analyzed (0 errors, 0 checks, 0 warnings and 0 pragma Assume statements) and proved (2 checks)
  VMS.Param_Stack_Size at vms.ads:427 flow analyzed (0 errors, 0 checks, 0 warnings and 0 pragma Assume statements) and proved (0 checks)
  VMS.Param_Top_Print at vms.ads:490 flow analyzed (0 errors, 0 checks, 0 warnings and 0 pragma Assume statements) and proved (10 checks)
  VMS.Print_Words at vms.ads:588 flow analyzed (0 errors, 0 checks, 0 warnings and 0 pragma Assume statements) and proved (2 checks)
  VMS.Register at vms.ads:521 flow analyzed (0 errors, 0 checks, 0 warnings and 0 pragma Assume statements) and proved (6 checks)
  VMS.Register at vms.ads:532 flow analyzed (0 errors, 0 checks, 0 warnings and 0 pragma Assume statements) and proved (6 checks)
  VMS.Return_Stack_Equal_From_Bottom_Until at vms.ads:287 flow analyzed (0 errors, 0 checks, 0 warnings and 0 pragma Assume statements) and proved (2 checks)
  VMS.Return_Stack_Size at vms.ads:502 flow analyzed (0 errors, 0 checks, 0 warnings and 0 pragma Assume statements) and proved (0 checks)
  VMS.Run_Address_Interpreter at vms.ads:593 flow analyzed (0 errors, 0 checks, 0 warnings and 0 pragma Assume statements) and proved (26 checks)
  VMS.Same at vms.ads:337 flow analyzed (0 errors, 0 checks, 0 warnings and 0 pragma Assume statements) and proved (0 checks)
  VMS.Same_Address_Interpreter at vms.ads:328 flow analyzed (0 errors, 0 checks, 0 warnings and 0 pragma Assume statements) and proved (0 checks)
  VMS.Same_Instructions at vms.ads:324 flow analyzed (0 errors, 0 checks, 0 warnings and 0 pragma Assume statements) and proved (0 checks)
  VMS.Same_Params at vms.ads:311 flow analyzed (0 errors, 0 checks, 0 warnings and 0 pragma Assume statements) and proved (0 checks)
  VMS.Same_Returns at vms.ads:316 flow analyzed (0 errors, 0 checks, 0 warnings and 0 pragma Assume statements) and proved (0 checks)
  VMS.Same_State at vms.ads:302 flow analyzed (0 errors, 0 checks, 0 warnings and 0 pragma Assume statements) and proved (0 checks)
  VMS.Same_Status at vms.ads:305 flow analyzed (0 errors, 0 checks, 0 warnings and 0 pragma Assume statements) and proved (0 checks)
  VMS.Same_TIB at vms.ads:308 flow analyzed (0 errors, 0 checks, 0 warnings and 0 pragma Assume statements) and proved (0 checks)
  VMS.Same_Words at vms.ads:321 flow analyzed (0 errors, 0 checks, 0 warnings and 0 pragma Assume statements) and proved (0 checks)
  VMS.Set_Instruction at vms.ads:243 flow analyzed (0 errors, 0 checks, 0 warnings and 0 pragma Assume statements) and proved (9 checks)
  VMS.Step_IP at vms.ads:258 flow analyzed (0 errors, 0 checks, 0 warnings and 0 pragma Assume statements) and proved (4 checks)
  VMS.Stop at vms.ads:194 flow analyzed (0 errors, 0 checks, 0 warnings and 0 pragma Assume statements) and proved (13 checks)
  VMS.Word_Code at vms.ads:78 flow analyzed (0 errors, 0 checks, 0 warnings and 0 pragma Assume statements) and proved (0 checks)
in unit vms-builtins, 36 subprograms and packages out of 36 analyzed
  VMS.Builtins at vms-builtins.ads:1 flow analyzed (0 errors, 0 checks, 0 warnings and 0 pragma Assume statements) and proved (0 checks)
  VMS.Builtins.Colon at vms-builtins.ads:226 flow analyzed (0 errors, 0 checks, 0 warnings and 0 pragma Assume statements) and proved (7 checks)
  VMS.Builtins.Comment at vms-builtins.ads:209 flow analyzed (0 errors, 0 checks, 0 warnings and 0 pragma Assume statements) and proved (2 checks)
  VMS.Builtins.Op_Add at vms-builtins.ads:17 flow analyzed (0 errors, 0 checks, 0 warnings and 0 pragma Assume statements) and proved (14 checks)
  VMS.Builtins.Op_Begin at vms-builtins.ads:351 flow analyzed (0 errors, 0 checks, 0 warnings and 0 pragma Assume statements) and proved (8 checks)
  VMS.Builtins.Op_Branch at vms-builtins.ads:466 flow analyzed (0 errors, 0 checks, 0 warnings and 0 pragma Assume statements) and proved (4 checks)
  VMS.Builtins.Op_Branch_If_False at vms-builtins.ads:473 flow analyzed (0 errors, 0 checks, 0 warnings and 0 pragma Assume statements) and proved (9 checks)
  VMS.Builtins.Op_Conditional_Exit at vms-builtins.ads:273 flow analyzed (0 errors, 0 checks, 0 warnings and 0 pragma Assume statements) and proved (17 checks)
  VMS.Builtins.Op_Divide at vms-builtins.ads:62 flow analyzed (0 errors, 0 checks, 0 warnings and 0 pragma Assume statements) and proved (15 checks)
  VMS.Builtins.Op_Drop at vms-builtins.ads:145 flow analyzed (0 errors, 0 checks, 0 warnings and 0 pragma Assume statements) and proved (8 checks)
  VMS.Builtins.Op_Dupe at vms-builtins.ads:129 flow analyzed (0 errors, 0 checks, 0 warnings and 0 pragma Assume statements) and proved (10 checks)
  VMS.Builtins.Op_Else at vms-builtins.ads:328 flow analyzed (0 errors, 0 checks, 0 warnings and 0 pragma Assume statements) and proved (39 checks)
  VMS.Builtins.Op_Equal at vms-builtins.ads:420 flow analyzed (0 errors, 0 checks, 0 warnings and 0 pragma Assume statements) and proved (10 checks)
  VMS.Builtins.Op_Exit at vms-builtins.ads:258 flow analyzed (0 errors, 0 checks, 0 warnings and 0 pragma Assume statements) and proved (15 checks)
  VMS.Builtins.Op_Greater_than at vms-builtins.ads:450 flow analyzed (0 errors, 0 checks, 0 warnings and 0 pragma Assume statements) and proved (11 checks)
  VMS.Builtins.Op_If at vms-builtins.ads:294 flow analyzed (0 errors, 0 checks, 0 warnings and 0 pragma Assume statements) and proved (8 checks)
  VMS.Builtins.Op_Left_Bracket at vms-builtins.ads:216 flow analyzed (0 errors, 0 checks, 0 warnings and 0 pragma Assume statements) and proved (1 checks)
  VMS.Builtins.Op_Less_than at vms-builtins.ads:435 flow analyzed (0 errors, 0 checks, 0 warnings and 0 pragma Assume statements) and proved (10 checks)
  VMS.Builtins.Op_Literal at vms-builtins.ads:192 flow analyzed (0 errors, 0 checks, 0 warnings and 0 pragma Assume statements) and proved (11 checks)
  VMS.Builtins.Op_Multiply at vms-builtins.ads:48 flow analyzed (0 errors, 0 checks, 0 warnings and 0 pragma Assume statements) and proved (12 checks)
  VMS.Builtins.Op_Negate at vms-builtins.ads:76 flow analyzed (0 errors, 0 checks, 0 warnings and 0 pragma Assume statements) and proved (9 checks)
  VMS.Builtins.Op_Over at vms-builtins.ads:98 flow analyzed (0 errors, 0 checks, 0 warnings and 0 pragma Assume statements) and proved (16 checks)
  VMS.Builtins.Op_Push_From_Return_Stack at vms-builtins.ads:174 flow analyzed (0 errors, 0 checks, 0 warnings and 0 pragma Assume statements) and proved (11 checks)
  VMS.Builtins.Op_Push_To_Return_Stack at vms-builtins.ads:156 flow analyzed (0 errors, 0 checks, 0 warnings and 0 pragma Assume statements) and proved (12 checks)
  VMS.Builtins.Op_Right_Bracket at vms-builtins.ads:219 flow analyzed (0 errors, 0 checks, 0 warnings and 0 pragma Assume statements) and proved (1 checks)
  VMS.Builtins.Op_Rotate at vms-builtins.ads:115 flow analyzed (0 errors, 0 checks, 0 warnings and 0 pragma Assume statements) and proved (18 checks)
  VMS.Builtins.Op_Subtract at vms-builtins.ads:34 flow analyzed (0 errors, 0 checks, 0 warnings and 0 pragma Assume statements) and proved (13 checks)
  VMS.Builtins.Op_Swap at vms-builtins.ads:88 flow analyzed (0 errors, 0 checks, 0 warnings and 0 pragma Assume statements) and proved (14 checks)
  VMS.Builtins.Op_Then at vms-builtins.ads:306 flow analyzed (0 errors, 0 checks, 0 warnings and 0 pragma Assume statements) and proved (23 checks)
  VMS.Builtins.Op_Until at vms-builtins.ads:364 flow analyzed (0 errors, 0 checks, 0 warnings and 0 pragma Assume statements) and proved (14 checks)
  VMS.Builtins.Op_Zero_Equal at vms-builtins.ads:375 flow analyzed (0 errors, 0 checks, 0 warnings and 0 pragma Assume statements) and proved (9 checks)
  VMS.Builtins.Op_Zero_Greater_than at vms-builtins.ads:405 flow analyzed (0 errors, 0 checks, 0 warnings and 0 pragma Assume statements) and proved (9 checks)
  VMS.Builtins.Op_Zero_Less_than at vms-builtins.ads:390 flow analyzed (0 errors, 0 checks, 0 warnings and 0 pragma Assume statements) and proved (9 checks)
  VMS.Builtins.Recurse at vms-builtins.ads:244 flow analyzed (0 errors, 0 checks, 0 warnings and 0 pragma Assume statements) and proved (5 checks)
  VMS.Builtins.Register_Builtins at vms-builtins.ads:4 flow analyzed (0 errors, 0 checks, 0 warnings and 0 pragma Assume statements) and proved (109 checks)
  VMS.Builtins.Semicolon at vms-builtins.ads:236 flow analyzed (0 errors, 0 checks, 0 warnings and 0 pragma Assume statements) and proved (3 checks)
```
