package lang

enum RawType:
  case IntType
  case UnitType
  case RefType(to: Type)
  case FuncType(from: Type, pc: TypeAnnotation, to: Type)

  override def toString: String = this match
    case IntType => "int"
    case UnitType => "unit"
    case RefType(to) => s"ref<$to>"
    case FuncType(f, pc, t) => s"($f ->$pc $t)"
