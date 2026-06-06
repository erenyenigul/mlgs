package lang

enum SecurityLevel:
  case L
  case H

object SecurityLevel:
  extension (a: SecurityLevel)
    def ⊔(b: SecurityLevel): SecurityLevel = (a, b) match
      case (L, L) => L
      case _      => H

    def ⊑(b: SecurityLevel): Boolean = (a, b) match
      case (L, _) => true
      case (H, H) => true
      case (H, L) => false

enum TypeAnnotation:
  case Static(level: SecurityLevel)
  case Dyn

object TypeAnnotation:
  extension (a: TypeAnnotation)
    def ⊔(b: TypeAnnotation): TypeAnnotation = (a, b) match
      case (Static(x), Static(y)) => Static(x ⊔ y)
      case _                      => Dyn

    def ⊑(b: TypeAnnotation): Boolean = (a, b) match
      case (Static(x), Static(y)) => x ⊑ y
      case (_, Dyn)               => true
      case (Dyn, Static(_))       => false

enum RawType:
  case IntType
  case UnitType
  case RefType(to: Type)
  case FuncType(from: Type, pc: TypeAnnotation, to: Type)

case class Type(s: RawType, annotation: TypeAnnotation)

object Type:
  def compatible(a: Type, b: Type): Boolean = (a.s, b.s) match
    case (RawType.IntType, RawType.IntType)   => true
    case (RawType.UnitType, RawType.UnitType) => true
    case (RawType.RefType(t1), RawType.RefType(t2)) =>
      compatible(t1, t2)
    case (RawType.FuncType(f1, _, t1), RawType.FuncType(f2, _, t2)) =>
      compatible(f1, f2) && compatible(t1, t2)
    case _ => false

  extension (a: Type)
    def ≺(b: Type): Boolean = (a, b) match
      case (Type(RawType.IntType, b1), Type(RawType.IntType, b2)) =>
        b1 ⊑ b2
      case (Type(RawType.UnitType, b1), Type(RawType.UnitType, b2)) =>
        b1 ⊑ b2
      case (Type(RawType.RefType(t1), b1), Type(RawType.RefType(t2), b2)) =>
        b1 ⊑ b2 && t1 == t2 // invariant
      case (Type(RawType.FuncType(f1, pc1, t1), b1), Type(RawType.FuncType(f2, pc2, t2), b2)) =>
        b1 ⊑ b2 && pc2 ⊑ pc1 && f2 ≺ f1 && t1 ≺ t2
      case _ => false

case class BlameId(id : String)
type BlameLabel = List[BlameId]

case class Variable(id: String)

enum RawValue:
  case Unit
  case Const(k: Int)
  case Loc(l: String, t: Type, p: BlameLabel)
  case Lambda(x: Variable, paramType: Type, pc: TypeAnnotation, e: Expression)

case class Value(w: RawValue, B: SecurityLevel)

enum Expression extends scala.util.parsing.input.Positional:
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


  // sugar
  case Let(x: Variable, e1: Expression, e2: Expression)
  case Seq(e1: Expression, e2: Expression)