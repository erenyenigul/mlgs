package lang
import lang.*
import java.util.UUID
import scala.util.parsing.combinator.*

def freshVar(prefix: String = ""): Variable =
  // Take a portion of the UUID string to keep the identifier reasonably short
  val uniqueId = UUID.randomUUID().toString.replace("-", "").take(8)
  Variable(s".${prefix}_$uniqueId")

object Parser extends JavaTokenParsers {

  override def skipWhitespace = true

  def run(input: String): Expression =
    parseAll(program, input) match {
      case Success(matched, _) => matched
      case Failure(msg, _) => throw Exception("Parsing failed: " + msg)
      case Error(msg, _) => throw Exception("Parsing failed: " + msg)
    }

  private def program: Parser[Expression] =
    sugar | expression

  private def sugar: Parser[Expression] = {
    "let" ~ variable ~ "=" ~ expression ~ "in" ~ expression ^^ {
      case _ ~ v ~ _ ~ e1 ~ _ ~ e2 => Expression.Let(v, e1, e2)
    } |
      expression ~ ";" ~ expression ^^ {
        case e1 ~ _ ~ e2 => Expression.Seq(e1, e2)
      }
  }

  private def expression: Parser[Expression] =
    assign | simpleExpression

  private def assign: Parser[Expression] =
    simpleExpression ~ ":=" ~ simpleExpression ^^ {
      case lhs ~ _ ~ rhs => Expression.Assign(lhs, rhs)
    }

  private def simpleExpression: Parser[Expression] =
    "new" ~> ("^" ~> "(" ~> _type) ~ ("," ~> securityLevel <~ ")") ~ simpleExpression ^^ {
      case t ~ b ~ e => Expression.New(t, b, e)
    } |
      "!" ~> simpleExpression ^^ Expression.Bang.apply |
      "{" ~> _type ~ ("<=" ~> _type) ~ ("}" ~> "^" ~> blameLabel) ~ simpleExpression ^^ {
        case to ~ from ~ p ~ e => Expression.Cast(to, from, p, e)
      } |
      "prot" ~> ("^" ~> securityLevel) ~ simpleExpression ^^ {
        case b ~ e => Expression.Prot(b, e)
      } |
      atomExpression ~ atomExpression ^^ {
        case e1 ~ e2 => Expression.Apply(e1, e2)
      } |
      atomExpression

  private def atomExpression: Parser[Expression] =
    value ^^ Expression.Val.apply |
      variable ^^ Expression.Var.apply |
      ("(" ~> rawValue <~ ")") ~ ("^" ~> securityLevel) ^^ {
        case w ~ b => Expression.Val(Value(w, b))
      } |
    "(" ~> expression <~ ")"


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
    ("λ" | "fn") ~> variable ~ (":" ~> _type) ~ ("@" ~> typeAnnotation) ~ (("." | "=>") ~> atomExpression) ^^ {
      case xVar ~ paramType ~ pc ~ body => RawValue.Lambda(xVar, paramType, pc, body)
    }

  private def variable: Parser[Variable] =
    not("in" | "let" | "fn" | "new" | "prot" | "ref" | "int" | "unit") ~> ident ^^ Variable.apply

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
    securityLevel ^^ TypeAnnotation.Static.apply | "?" ^^^ TypeAnnotation.Dyn

  private def securityLevel: Parser[SecurityLevel] =
    "L" ^^^ SecurityLevel.L | "H" ^^^ SecurityLevel.H
}
