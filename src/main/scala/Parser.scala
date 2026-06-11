import lang.*

import scala.util.parsing.combinator.*
import lang.SecurityLevel.*
import lang.TypeAnnotation.*
import util.FreshBlame

object Parser extends JavaTokenParsers {

  override def skipWhitespace = true
  private val keywords = Set("in", "let", "fn", "lambda", "new", "prot", "ref", "int", "unit", "L", "H", "as")

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
  private def anyExpr: Parser[Expression] = fnDef | letBinding | exprThenOpt

  private def fnDef: Parser[Expression] =
    positioned("fn" ~> opt("[" ~> securityLevel <~ "]") ~ variable ~ paramList ~ ("-[" ~> annotationContent <~ "]->") ~ ("{" ~> anyExpr <~ "}") ~ (";" ~> anyExpr) ^^ {
      case vl ~ name ~ params ~ pc ~ body ~ rest =>
        Expression.Let(name, desugarParams(params, pc, vl.getOrElse(L), body), rest)
    })

  private def letBinding: Parser[Expression] =
    positioned("let" ~> variable ~ ("=" ~> expression) ~ (";" ~> anyExpr) ^^ {
      case v ~ e1 ~ e2 => Expression.Let(v, e1, e2)
    })

  // expression optionally followed by "; rest" (sequencing), or standalone
  private def exprThenOpt: Parser[Expression] =
    positioned(expression ~ opt(";" ~> anyExpr) ^^ {
      case e ~ None       => e
      case e ~ Some(rest) => Expression.Seq(e, rest)
    })

  private def paramList: Parser[List[Variable ~ Type]] =
    "(" ~> rep1sep(variable ~ (":" ~> _type), ",") <~ ")"

  private def desugarParams(params: List[Variable ~ Type], pc: TypeAnnotation, outerLevel: SecurityLevel, body: Expression): Expression =
    params match
      case (x ~ t) :: Nil  => Expression.LambdaExp(x, t, pc, outerLevel, body)
      case (x ~ t) :: rest => Expression.LambdaExp(x, t, pc, outerLevel, desugarParams(rest, pc, outerLevel, body))
      case Nil             => body


  private def expression: Parser[Expression] =
      assign |
      simpleExpression

  private def assign: Parser[Expression] =
    positioned(simpleExpression ~ ":=" ~ simpleExpression ^^ {
      case lhs ~ _ ~ rhs => Expression.Assign(lhs, rhs)
    })

  private def simpleExpression: Parser[Expression] =
    positioned("new" ~> ("[" ~> securityLevel <~ "]") ~ ("<" ~> _type <~ ">") ~ ("(" ~> expression <~ ")") ^^ {
      case b ~ t ~ e => Expression.New(t, b, e)
    }) |
      positioned("!" ~> simpleExpression ^^ Expression.Bang.apply) |
      positioned("prot" ~> securityLevel ~ simpleExpression ^^ {
        case b ~ e => Expression.Prot(b, e)
      }) |
      positioned(currentPosition ~ ("cast" ~> "<" ~> _type ~ opt("," ~> _type) <~ ">") ~ ("(" ~> expression <~ ")") ^^ {
        case (line, col, srcLine) ~ (to ~ Some(from)) ~ e =>
          Expression.Cast(to, from, FreshBlame("cast", line, col, srcLine), e)
        case (line, col, srcLine) ~ (to ~ None) ~ e =>
          Expression.UntypedCast(to, FreshBlame("cast", line, col, srcLine), e)
      }) |
      appOrCastChain

  private def appOrCastChain: Parser[Expression] = {
    positioned(currentPosition ~ atomExpression ~ rep(appOrCastSuffix) ^^ {
      case pos ~ head ~ suffixes =>
        suffixes.foldLeft(head) {
          case (acc, Left(Nil)) =>
            Expression.Apply(acc, Expression.Val(Value(RawValue.Unit, SecurityLevel.L))).setPos(acc.pos)
          case (acc, Left(args)) =>
            args.foldLeft(acc)((f, arg) => Expression.Apply(f, arg).setPos(acc.pos))
          case (acc, Right((t, line, col, srcLine))) =>
            Expression.UntypedCast(t, FreshBlame("cast", line, col, srcLine), acc).setPos(acc.pos)
        }
    })
  }

  private def appOrCastSuffix: Parser[Either[List[Expression], (Type, Int, Int, String)]] =
    (currentPosition ~ ("as" ~> _type) ^^ {
      case (line, col, srcLine) ~ t => Right((t, line, col, srcLine))
    }) |
    ("(" ~> repsep(expression, ",") <~ ")" ^^ { args => Left(args) }) |
    (atomExpression ^^ { e => Left(List(e)) })

  private def atomExpression: Parser[Expression] =
    positioned(lambda) |
      positioned(rawValue ~ opt(typeAnnotation) ^^ {
        case w ~ ann =>
          val level = ann match
            case Some(Static(l)) => l
            case _               => SecurityLevel.L
          Expression.Val(Value(w, level))
      }) |
      positioned(variable ^^ Expression.Var.apply) |
      "(" ~> expression <~ ")"

  private def annotationContent: Parser[TypeAnnotation] =
    securityLevel ^^ TypeAnnotation.Static.apply | "?" ^^^ TypeAnnotation.Dyn

  private def lambda: Parser[Expression] =
    ("λ" | "lambda") ~> opt("[" ~> securityLevel <~ "]") ~ paramList ~ ("-[" ~> annotationContent <~ "]->") ~ ("{" ~> anyExpr <~ "}") ^^ {
      case vl ~ params ~ pc ~ body =>
        desugarParams(params, pc, vl.getOrElse(L), body)
    }

  def rawValue: Parser[RawValue] =
    constant | unit

  private def unit: Parser[RawValue] =
    "(" ~ ")" ^^^ RawValue.Unit

  private def constant: Parser[RawValue.Const] =
    """\d+""".r ^^ { numericStr => RawValue.Const(numericStr.toInt) }

  private def variable: Parser[Variable] =
    ident.filter(s => !keywords.contains(s)) ^^ Variable.apply

  // _type returns a full Type (RawType + annotation, defaulting to Static(L))
  // ref is handled here specially to capture optional annotation before <...>
  private def _type: Parser[Type] =
    refType |
    (simpleRawType ~ opt(typeAnnotation) ^^ {
      case s ~ ann => Type(s, ann.getOrElse(Static(L)))
    })

  // ref with optional annotation then angle-bracket inner type
  // Syntax: ref[ann]<inner> or ref<inner>
  private def refType: Parser[Type] =
    "ref" ~> opt(typeAnnotation) ~ ("<" ~> _type <~ ">") ^^ {
      case ann ~ inner => Type(RawType.RefType(inner), ann.getOrElse(Static(L)))
    }

  private def simpleRawType: Parser[RawType] =
    "int" ^^^ RawType.IntType |
      "unit" ^^^ RawType.UnitType |
      "(" ~> _type ~ ("-[" ~> annotationContent <~ "]->") ~ _type <~ ")" ^^ {
        case from ~ pc ~ to => RawType.FuncType(from, pc, to)
      }

  private def typeAnnotation: Parser[TypeAnnotation] =
    "[" ~> (securityLevel ^^ TypeAnnotation.Static.apply | "?" ^^^ TypeAnnotation.Dyn) <~ "]"

  private def securityLevel: Parser[SecurityLevel] =
    "H" ^^^ SecurityLevel.H | "L" ^^^ SecurityLevel.L
}
