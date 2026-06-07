package static
import lang.*
import util.Diagnostic.formatLine

case class TypeErrorAt(error: TypeError, line: Int, col: Int, sourceLine: String, notes: List[String] = Nil) extends Exception:
  override def getMessage: String =
    val notesStr = if notes.isEmpty then "" else "\n" + notes.map(n => s"  note: $n").mkString("\n")
    s"Type Error: ${error.getMessage}\n${formatLine(line, col, sourceLine)}$notesStr"

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
      s"Structural mismatch. Expected ${exp}, but found ${fnd}."
    case TypeMismatch(exp, fnd) =>
      s"Type mismatch. Expected ${exp}, but found ${fnd}."
    case ExpectedRefType(fnd) =>
      s"Expected a reference, but found ${fnd}"
    case SecurityFlowViolation(exp, fnd) =>
      s"Illegal flow. Cannot unify tier ${fnd} into ${exp}."
    case ProgramCounterViolation(pc, eff) =>
      s"Side effect at ${eff} level is forbidden under ${pc} PC context."
    case ReferenceInvarianceViolation(t1, t2) =>
      s"References must be invariant. ${t1} and ${t2} must match exactly."
    case IncompatibleCast(from, to) =>
      s"Invalid cast from ${from} to ${to}. Underlying structures must be compatible."
    case UnboundVariable(name) =>
      s"Variable '$name' is not defined in the current environment."
    case InvalidMemoryLocation(loc) =>
      s"Attempted to access non-existing or unallocated heap location '$loc'."
    case CannotApplyNonFunction(found) =>
      s"Tried to call a non-function. It is a ${found}"
