# anteforth

A verified Forth interpreter.

## Execution

Run with Alire:

```
alr run
```

## Implemented Words


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
