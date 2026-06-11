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

## TODO
- Implicit security type : If nothing is specified, assume L. e.g. int => int[L], or ref<i>
- Security level denoted by square brackets: e.g. int[H], int[L], int[?]
- Reference with ref<type>. e.g. ref<int> => ref[L]<int[L]> or ref[H]<int>
- Function accepts both effect level and function value level
- casts are done using `as type` syntax instead of `cast()`
- Casts don't need source type. Just provide target type. I don't know if its possible.
- Values should reside in the AST. They should be generated in interp time. So, with that, you must have a LambdaExp and Lambda Raw Value. One is annotated, and the other is not annotated, used as a runtime value
- Do I allow subtyping?  Check below:
>The system also supports
  standard security subtyping [14], [21] which allows low-
  security information to be implicitly promoted to a high
  security level and functions with high-security pc to be
