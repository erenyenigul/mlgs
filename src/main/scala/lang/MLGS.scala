package lang

enum SecurityLevel:
  case L
  case H

enum TypeAnnotation:
  case Known(level: SecurityLevel)
  case StaticUnknown

enum RawType:
  case IntType
  case UnitType
  case RefType(to: Type)
  case FuncType(from: Type, pc: TypeAnnotation, to: Type)

case class Type(s: RawType, annotation: TypeAnnotation)
case class BlameId(id : String)
type BlameLabel = List[BlameId]

case class Variable(id: String)

enum RawValue:
  case Unit
  case Const(k: Int)
  case Loc(l: String, t: Type, p: BlameLabel)
  case Lambda(x: Variable, e: Expression)

case class Value(w: RawValue, B: SecurityLevel)

enum Expression:
  case Val(v: Value)
  case Var(x: Variable)
  case Apply(target: Expression, arg: Expression)

  case New(t: Type, B: SecurityLevel, e: Expression)
  case Bang(e: Expression)
  case Assign(lhs: Expression, rhs: Expression)
  case Cast(to: Type, from: Type, p: BlameLabel, e: Expression)
  case Prot(B: SecurityLevel, e: Expression)
  case GuardCast(to: TypeAnnotation, from: TypeAnnotation, p:BlameLabel, e: Expression)
  case PointerCast(to: TypeAnnotation, from: TypeAnnotation, p:BlameLabel, e: Expression)
  case FunctionGuardCast(to: TypeAnnotation, from: TypeAnnotation, p: BlameLabel, e:Expression )
