package lang

import lang.TypeAnnotation.Dyn

case class Type(s: RawType, annotation: TypeAnnotation):
  override def toString: String = s"$s@$annotation"

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