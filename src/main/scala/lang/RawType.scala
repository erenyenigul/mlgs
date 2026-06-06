package lang

enum RawType:
  case IntType
  case UnitType
  case RefType(to: Type)
  case FuncType(from: Type, pc: TypeAnnotation, to: Type)
