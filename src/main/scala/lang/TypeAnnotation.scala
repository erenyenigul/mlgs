package lang

enum TypeAnnotation:
  case Static(level: SecurityLevel)
  case Dyn

  override def toString: String = this match
    case Static(level) => "[" ++ level.toString ++ "]"
    case Dyn => "[?]"

object TypeAnnotation:
  extension (a: TypeAnnotation)
    def ⊔(b: TypeAnnotation): TypeAnnotation = (a, b) match
      case (Static(x), Static(y)) => Static(x ⊔ y)
      case _                      => Dyn

    def ⊑(b: TypeAnnotation): Boolean = (a, b) match
      case (Static(x), Static(y)) => x ⊑ y
      case (_, Dyn)               => true
      case (Dyn, Static(_))       => false