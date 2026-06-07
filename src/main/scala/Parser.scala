import lang.*

import scala.util.parsing.combinator.*
import util.FreshBlame

object Parser extends JavaTokenParsers {

  override def skipWhitespace = true

  def run(input: String): Either[Exception, (Expression, String)] =
    try
      parseAll(program, input) match {
        case Success(matched, _) => Right((matched, input))
        case Failure(msg, next) =>
          val pos = next.pos
          val srcLine = pos.longString.split('\n').headOption.getOrElse("")
          val caret = " ".repeat(pos.column - 1) + "^"
          Left(Exception(s"[${pos.line}:${pos.column}] Parsing failed: $msg\n  $srcLine\n  $caret"))
        case Error(msg, next) =>
          val pos = next.pos
          val srcLine = pos.longString.split('\n').headOption.getOrElse("")
          val caret = " ".repeat(pos.column - 1) + "^"
          Left(Exception(s"[${pos.line}:${pos.column}] Parsing failed: $msg\n  $srcLine\n  $caret"))
      }
    catch
      case e: Exception => Left(e)

  private def currentPosition: Parser[(Int, Int, String)] = Parser { input =>
    val line = input.pos.line
    val col = input.pos.column
    val srcLine = input.source.toString.linesIterator.drop(line - 1).nextOption().getOrElse("")
    Success((line, col, srcLine), input)
  }

  private def program: Parser[Expression] = anyExpr
  private def anyExpr: Parser[Expression] = sugar | expression

  private def sugar: Parser[Expression] =
    positioned("let" ~> variable ~ ("=" ~> anyExpr) ~ ("in" ~> anyExpr) ^^ {
      case v ~ e1 ~ e2 => Expression.Let(v, e1, e2)
    }) |
      positioned(expression ~ (";" ~> anyExpr) ^^ {
        case e1 ~ e2 => Expression.Seq(e1, e2)
      })

  private def expression: Parser[Expression] =
      assign |
      simpleExpression

  private def assign: Parser[Expression] =
    positioned(simpleExpression ~ ":=" ~ simpleExpression ^^ {
      case lhs ~ _ ~ rhs => Expression.Assign(lhs, rhs)
    })

  private def simpleExpression: Parser[Expression] =
    positioned("new" ~> ("(" ~> _type) ~ ("," ~> securityLevel <~ ")") ~ simpleExpression ^^ {
      case t ~ b ~ e => Expression.New(t, b, e)
    }) |
      positioned("!" ~> simpleExpression ^^ Expression.Bang.apply) |
      positioned(currentPosition ~ ("{" ~> _type) ~ ("<=" ~> _type <~ "}") ~ simpleExpression ^^ {
        case (line, col, srcLine) ~ to ~ from ~ e =>
          Expression.Cast(to, from, FreshBlame("cast", line, col, srcLine), e)
      }) |
      positioned("prot" ~> securityLevel ~ simpleExpression ^^ {
        case b ~ e => Expression.Prot(b, e)
      }) |
      positioned(atomExpression ~ atomExpression ^^ {
        case e1 ~ e2 => Expression.Apply(e1, e2)
      }) |
      atomExpression

  private def atomExpression: Parser[Expression] =
    positioned(value ^^ Expression.Val.apply) |
      positioned(variable ^^ Expression.Var.apply) |
      positioned(("(" ~> rawValue <~ ")") ~ ("@" ~> securityLevel) ^^ {
        case w ~ b => Expression.Val(Value(w, b))
      }) |
    "(" ~> expression <~ ")"


  private def value: Parser[Value] =
    rawValue ~ opt("@" ~> securityLevel) ^^ {
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
    ("λ" | "fn") ~> variable ~ (":" ~> _type) ~ ("@" ~> typeAnnotation) ~ ("->" ~> atomExpression) ^^ {
      case xVar ~ paramType ~ pc ~ body => RawValue.Lambda(xVar, paramType, pc, body)
    }

  private def variable: Parser[Variable] =
    not("in" | "let" | "fn" | "new" | "prot" | "ref" | "int" | "unit" | "low" | "high") ~> ident ^^ Variable.apply

  private def _type: Parser[Type] =
    (rawType <~ "@") ~ typeAnnotation ^^ {
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
    "low" ^^^ SecurityLevel.L | "high" ^^^ SecurityLevel.H
}
