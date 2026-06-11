package lang

enum SecurityLevel:
  case L
  case H

  override def toString: String = this match
    case L => "L"
    case H => "H"

object SecurityLevel:
  extension (a: SecurityLevel)
    def ⊔(b: SecurityLevel): SecurityLevel = (a, b) match
      case (L, L) => L
      case _      => H

    def ⊑(b: SecurityLevel): Boolean = (a, b) match
      case (L, _) => true
      case (H, H) => true
      case (H, L) => false