package lang

enum Expression extends scala.util.parsing.input.Positional:
  case Val(v: Value)
  case Var(x: Variable)
  case Apply(target: Expression, arg: Expression)

  case New(t: Type, B: SecurityLevel, e: Expression)
  case Bang(e: Expression)
  case Assign(lhs: Expression, rhs: Expression)
  case Cast(to: Type, from: Type, p: BlameLabel, e: Expression)
  case Prot(B: SecurityLevel, e: Expression)
  case GuardCast(to: TypeAnnotation, from: TypeAnnotation, p: BlameLabel, e: Expression)
  case PointerCast(to: TypeAnnotation, from: TypeAnnotation, p: BlameLabel, e: Expression)
  case FunctionGuardCast(to: TypeAnnotation, from: TypeAnnotation, p: BlameLabel, e: Expression)


  // sugar
  case Let(x: Variable, e1: Expression, e2: Expression)
  case Seq(e1: Expression, e2: Expression)
