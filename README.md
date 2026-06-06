# ML-GS: Gradual Security Typing with References

Toy implementation of Fennell and Thiemann's *Gradual Security Typing with References* (CSF 2013)
for an MSc seminar at TU Delft.

ML-GS lets programmers mix static and dynamic information-flow control (IFC) in the same program.
Security casts mediate between statically checked regions (where non-interference is guaranteed by
the type system) and dynamically checked regions (where security levels are tracked at runtime).
Security levels form a two-element lattice: `L` (public) and `H` (secret).

```
-- public pointer to a secret integer
let x = 5^H in new^(int^H, L) x

-- cast a statically typed function into a dynamic context
let w = {ref Report^? ^? -> unit} sendToFacebook in
{ref Report^L ^L -> unit}(addPrivileged false w)
```

## Implementation

- **Parser**: parses ML-GS source programs into an AST
- **Type checker**: implements the gradual security type system (Fig. 2), including
  subtyping, compatibility, and security level constraints on effects and program counter

## Not implemented
- Operational semantics and reduction rules
- Runtime guard/pointer/function casts (runtime-only constructs per the paper)
- Blame tracking
