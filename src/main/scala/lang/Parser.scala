package lang
import lang.*
import java.util.UUID
import scala.util.parsing.combinator.*

def freshVar(prefix: String = ""): Variable =
  // Take a portion of the UUID string to keep the identifier reasonably short
  val uniqueId = UUID.randomUUID().toString.replace("-", "").take(8)
  Variable(s".${prefix}_$uniqueId")

class Parser extends JavaTokenParsers {

  override def skipWhitespace = true

  def run(input: String): Unit =
    parseAll(program, input) match {
      case Success(matched, _) => println(matched)
      case Failure(msg, _) => println(s"FAILURE: $msg")
      case Error(msg, _) => println(s"ERROR: $msg")
    }

  private def program: Parser[Expression] =
    sugar | expression

  private def sugar: Parser[Expression] = {
    "let" ~ variable ~ "=" ~ expression ~ "in" ~ expression ^^ {
      case _ ~ variable ~ _ ~ e1 ~ _ ~ e2 => Expression.Apply(
        Expression.Val(
          Value(
            RawValue.Lambda(
              variable, e2
            ),
            SecurityLevel.L
          )
        ),
        e1
      )
    } |
    expression ~ ";" ~ expression ^^ {
      case e1 ~ _ ~ e2 => Expression.Apply(
        Expression.Val(
          Value(
            RawValue.Lambda(
              freshVar(), e2
            ),
            SecurityLevel.L
          )
        ),
        e1
      )
    }
  }

  private def expression: Parser[Expression] =
    value ^^ Expression.Val.apply |
      variable ^^ Expression.Var.apply |
      expression ~ expression ^^ {
        case e1 ~ e2 => Expression.Apply(e1, e2)
      } |
      "new" ~ "^" ~ "(" ~ _type ~ "," ~ securityLevel ~ ")" ~ expression ^^ {
        case _ ~ _ ~ _ ~ t ~ _ ~ b ~ _ ~ e => Expression.New(t, b, e)
      } |
      "!" ~ expression ^^ {
        case _ ~ e => Expression.Bang(e)
      } |
      expression ~ ":=" ~ expression ^^ {
        case lhs ~ _ ~ rhs => Expression.Assign(lhs, rhs)
      } |
      "{" ~ _type ~ "<=" ~ _type ~ "}" ~ "^" ~ blameLabel ~ expression ^^ {
        case _ ~ to ~ _ ~ from ~ _ ~ _ ~ p ~ e => Expression.Cast(to, from, p, e)
      } |
      "prot" ~ "^" ~ securityLevel ~ expression ^^ {
        case _ ~ _ ~ b ~ e => Expression.Prot(b, e)
      }

  private def value: Parser[Value] =
    rawValue ~ opt("^" ~> securityLevel) ^^ {
      case w ~ Some(explicitLevel) =>
       Value(w, explicitLevel)

      case w ~ None =>
        Value(w, SecurityLevel.L)
    }

  def rawValue: Parser[RawValue] =
    constant | unit | lambda

  private def unit: Parser[RawValue] =
    "(" ~ ")" ^^^ RawValue.Unit

  private def constant: Parser[RawValue.Const] =
    """\d+""".r ^^ { numericStr => RawValue.Const(numericStr.toInt) }

  private def lambda: Parser[RawValue.Lambda] =
    ("λ" | "fn") ~> variable ~ ("." | "=>") ~ expression ^^ {
      case xVar ~ _ ~ body => RawValue.Lambda(xVar, body)
    }

  private def variable: Parser[Variable] =
    ident ^^ Variable.apply

  private def blameLabel: Parser[BlameLabel] =
    repsep(blameId, ",")

  private def blameId: Parser[BlameId] =
    ident ^^ BlameId.apply

  private def _type: Parser[Type] =
    (rawType <~ "^") ~ typeAnnotation ^^ {
      case s ~ b => Type(s, b)
    }

  private def rawType: Parser[RawType] =
      "int" ^^^ RawType.IntType |
      "unit" ^^^ RawType.UnitType |
      (_type <~ "->") ~ typeAnnotation ~ _type ^^ {
        case from ~ pc ~ to => RawType.FuncType(from, pc, to)
      } |
      "ref" ~> _type ^^ RawType.RefType.apply

  private def typeAnnotation: Parser[TypeAnnotation] =
    securityLevel ^^ TypeAnnotation.Known.apply | "?" ^^^ TypeAnnotation.StaticUnknown

  private def securityLevel: Parser[SecurityLevel] =
    "L" ^^^ SecurityLevel.L | "H" ^^^ SecurityLevel.H
}
