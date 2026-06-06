package lang

enum RawValue:
  case Unit
  case Const(k: Int)
  case Loc(l: String, t: Type, p: BlameLabel)
  case Lambda(x: Variable, paramType: Type, pc: TypeAnnotation, e: Expression)
