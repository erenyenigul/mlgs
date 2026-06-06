package static
import lang.*

case class TypeErrorAt(error: TypeError, line: Int, col: Int, sourceLine: String) extends Exception:
  override def getMessage: String =
    val caret = " " * (col - 1) + "^"
    s"${error.getMessage}\n${line} |${sourceLine}\n   ${caret}"

enum TypeError extends Exception:
  case StructuralMismatch(expected: RawType, found: RawType)
  case TypeMismatch(expected: Type, found: Type)
  case ExpectedRefType(found: Type)
  case SecurityFlowViolation(expected: TypeAnnotation, found: TypeAnnotation)
  case ProgramCounterViolation(pc: TypeAnnotation, allowedEffect: TypeAnnotation)
  case ReferenceInvarianceViolation(t1: Type, t2: Type)
  case IncompatibleCast(from: Type, to: Type)
  case UnboundVariable(name: String)
  case InvalidMemoryLocation(location: String)
  case CannotApplyNonFunction(found: Type)

  override def getMessage: String = this match
    case StructuralMismatch(exp, fnd) =>
      s"Static Type Error: Structural mismatch. Expected ${exp}, but found ${fnd}."
    case TypeMismatch(exp, fnd) =>
      s"Static Type Error: Type mismatch. Expected ${exp}, but found ${fnd}."
    case ExpectedRefType(fnd) =>
      s"Static Type Error: Expected a reference, but found ${fnd}"
    case SecurityFlowViolation(exp, fnd) =>
      s"Static Security Error: Illegal flow. Cannot unify tier ${fnd} into ${exp}."
    case ProgramCounterViolation(pc, eff) =>
      s"Static Security Error: Side effect at level ${eff} is forbidden under PC context (${pc})."
    case ReferenceInvarianceViolation(t1, t2) =>
      s"Static Type Error: References must be invariant. ${t1} and ${t2} must match exactly."
    case IncompatibleCast(from, to) =>
      s"Static Type Error: Invalid cast from ${from} to ${to}. Underlying structures must be compatible."
    case UnboundVariable(name) =>
      s"Static Scope Error: Variable '$name' is not defined in the current environment."
    case InvalidMemoryLocation(loc) =>
      s"Runtime Memory Error: Attempted to access non-existing or unallocated heap location '$loc'."
    case CannotApplyNonFunction(found) =>
      s"Static Type Error: Tried to call a non-function. It is a ${found}"
