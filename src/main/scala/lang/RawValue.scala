package lang

import lang.RawType.*
import lang.Expression.*
import runtime.InterpreterError.BlameError
import util.FreshVar

enum RawValue:
  case Unit
  case Const(k: Int)
  case Loc(l: String, t: Type, p: BlameLabel)
  case Lambda(x: Variable, paramType: Type, pc: TypeAnnotation, e: Expression)

object RawValue:
  extension (w: RawValue)
    def propagate(to: RawType, from: RawType, p: BlameLabel): RawValue =
      (w, to, from) match
        case (Const(k), IntType, IntType) => Const(k)

        case (Unit, UnitType, UnitType) => Unit

        case (Loc(l, t1, q), RefType(t1_), RefType(_)) =>
          Loc(l, t1_, p.concat(q))

        case (Lambda(x, paramType, pc, body),
        FuncType(t1_, pc_, t2_),
        FuncType(t1, pc2, t2)) =>
          val freshX = FreshVar()
          Lambda(
            Variable(freshX),
            t1_,
            pc_,
            Cast(
              t2_, t2, p,
              Apply(
                Val(Value(
                  Lambda(x, paramType, pc,
                    GuardCast(pc_, pc2, p, body)
                  ), SecurityLevel.L
                )),
                Cast(t1, t1_, p, Var(Variable(freshX)))
              )
            )
          )

        case _ => throw BlameError(p)