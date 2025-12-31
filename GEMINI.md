# Project Context & Persona

# Core Guidelines

## 1. Coding Standards (Ada/SPARK)
* **Language Level:** Use Ada 2012/2022 standards.
* **SPARK Mode:** All core interpreter logic (stacks, dictionary, execution loop) must be `SPARK_Mode => On`.
* **Typer Safety:**
    * Use strong typing for the Forth stack cells (e.g., specific modular or signed integer types, not generic `Integer`).
    * Avoid `Unchecked_Conversion` unless absolutely necessary and rigorously justified.
* **Restrictions:**
    * No pointers/access types in the verified core (use cursor-based data structures for the dictionary).
    * No side effects in functions; enforce `Global => null` or specific `Global => (Input => ...)` where strictly required.

## 2. Formal Verification & Contracts
* **Design by Contract:** Every subprogram must have clear `Pre` and `Post` conditions.
* **Forth Specific Invariants:**
    * **Stack Safety:** Operations must prove they do not underflow or overflow the parameter/return stacks.
        * *Example:* A `Pop` operation precondition must verify `Stack_Depth > 0`.
    * **Memory Safety:** Dictionary access must be proven to be within bounds at compile/proof time.
* **Loop Proofs:** Always provide `Loop_Invariant` and `Loop_Variant` for the interpreter's inner loop to prove termination and state consistency.
* **Provers:** Code must be written to be friendly to `gnatprove` (Alt-Ergo/CVC5). Avoid constructs known to cause prover timeouts.

## 3. Communication Style
* **Professional Tone:** Responses must be direct, rigorous, and concise.
* **No Emojis:** Strictly no emojis in explanations, code comments, or commit messages.
* **Proof Explanations:** When a proof fails or is complex, explain the counter-example or the missing assertion required to satisfy the solver.

## 4. Documentation
* **Ghost Code:** Differentiate clearly between executable code and "Ghost" code used only for verification.
* **Rationale:** Comment on *why* a specific loop invariant was chosen, specifically how it helps the solver advancing the proof.

## 5. Specification
* Ensure that the specification at spec/main.md is kept consistent with the
  code in the folder src/
