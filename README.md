# ML-GS: Gradual Security Typing with References

Toy implementation of Fennell and Thiemann's *Gradual Security Typing with References* (CSF 2013)
for an MSc seminar at TU Delft.

ML-GS lets programmers mix static and dynamic information-flow control (IFC) in the same program.
Security casts mediate between statically checked regions (where non-interference is guaranteed by
the type system) and dynamically checked regions (where security levels are tracked at runtime).
Security levels form a two-element lattice: `L` (public) and `H` (secret).

## Building and running

Requires sbt and a JVM.

```sh
# Run a source file
sbt "run examples/0.mlgs"

# Start the REPL
sbt run

# Run the test suite (reproduces the evaluation results from the report)
sbt "runMain runProgramTests"
```

## Syntax

Security levels are written in square brackets. `[L]` is public, `[H]` is secret, and `[?]` means the level is checked at runtime.

```
// Types
int[L]                          // public integer
int[?]                          // integer with runtime-checked level
ref[L]<int[H]>                  // public pointer to a secret integer
(int[L] -[L]-> int[H])[L]      // public fn, low pc, takes public int, returns secret int

// Values
42[H]                           // secret integer literal (defaults to [L] if omitted)

// Functions
fn f(x : int[L]) -[L]-> { x }               // named function, low pc
fn[H] f(x : int[H]) -[H]-> { x }           // secret function value, high pc
fn f(x : int[L], y : int[L]) -[L]-> { x }  // multi-arg, desugars to nested lambdas

// References
let r = new[L]<int[L]>(0);     // allocate a public ref with pointer level L
r := 42;                        // assign
!r                              // dereference

// Casts
e as int[?]                     // source type is inferred
cast<int[H], int[L]>(e)         // explicit source and target types

// Sequencing
e1; e2                          // evaluate e1, discard result, return e2
```

## Implementation

- **Parser**: parses ML-GS source programs into an AST
- **Type checker**: implements the gradual security type system (Fig. 2), including
  subtyping, compatibility, and security level constraints on effects and program counter
- **Interpreter**: applies reduction rules in the paper, and executes the given program

## Examples

### `examples/0.mlgs` -- downcast blame

Allocates a public `ref[L]<int[L]>`, then writes a secret integer through a cast to
`ref[L]<int[H]>`. A subsequent dereference into a function expecting `int[L]` detects the
level mismatch. Result: **runtime blame exception** pointing to the allocation site.

### `examples/1.mlgs` -- implicit level promotion

Passes a public integer `0[L]` to a function expecting `int[H]`. The dynamic annotation `[?]`
on the function's pc allows the call. The result is promoted to level `H` because the function
annotation subsumes it. Result: `0[H]`.

### `examples/2.mlgs` -- dynamic argument level

A function accepting a fully dynamic `int[?]` is called with a secret `0[H]`. The dynamic
annotation propagates the runtime level through to the result. Result: `0[H]`.

### `examples/3.mlgs` -- NSU success (low pc write)

A public reference is cast to `ref[?]<int[?]>` and passed to a function that assigns a secret
integer under a **low** pc (`-[L]->`). The NSU check passes because the write happens outside a
secret context. The cast reconciles the access type at dereference time. Result: `0[H]`.

### `examples/4.mlgs` -- NSU failure (high pc write to public cell)

Same setup as `3.mlgs`, but the function runs under a **high** pc (`-[H]->`). Writing a secret
into what is ultimately a public cell (`new[L]<int[L]>`) violates the NSU policy. Result:
**runtime blame exception** pointing to the allocation site.

### `examples/buggy.mlgs` -- NSU violation via dynamic cast

A public reference is cast to `ref<int[?]>` and assigned to inside a high-security function
body. The assignment upgrades a low cell under a high pc, which the dynamic IFC monitor
catches. Result: **runtime blame exception**.

### `examples/facebook_fail.mlgs` -- static false positive

The `addPrivileged` example from Section II of the paper, fully statically typed. The type
checker rejects the program because `addPrivileged`'s body writes `infoH : int[H]` into
`report : ref[L]<int[L]>`, even though the write only occurs on the `true` branch. The static
system cannot track the correspondence between the `isPrivileged` flag and the security level
of `worker`. Result: **type error**.

### `examples/facebook_dyn.mlgs` -- gradual fix

The same scenario with `addPrivileged` given a fully dynamic signature and `sendToFacebook`
wrapped in a cast at the call site. The `false` path executes safely at runtime with no
secret flowing to Facebook. Result: `0[L]`.

### `examples/secret_func.mlgs` -- high-security function value

Defines a function annotated at level `H` that takes and returns a secret integer. The program
evaluates to the function value itself at level `H`. Demonstrates that function values carry
their own security level and can be treated as secrets. Result: `Lambda[H]`.

