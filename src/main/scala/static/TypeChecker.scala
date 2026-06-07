package static

import lang.RawType.{FuncType, IntType, RefType, UnitType}
import lang.RawValue.{Const, Lambda, Loc, Unit}
import lang.SecurityLevel.L
import lang.TypeAnnotation.Static
import lang.{Expression, SecurityLevel, Type, TypeAnnotation, Value, Variable}
import lang.Expression.*
import static.*
import static.TypeError.{CannotApplyNonFunction, ExpectedRefType, InvalidMemoryLocation, ProgramCounterViolation, TypeMismatch, UnboundVariable, IncompatibleCast}

import scala.util.parsing.input.NoPosition

class TypeChecker(program: Expression, source: String = "") {

  def run(context: Context = Context()): Type = infer(program, context)

  private def getSourceLine(line: Int): String =
    source.linesIterator.drop(line - 1).nextOption().getOrElse("")

  private def errorAt(e: Expression, err: TypeError): Nothing =
    val pos = e.pos
    if pos == NoPosition then throw err
    else throw TypeErrorAt(err, pos.line, pos.column, getSourceLine(pos.line))

  private def infer(e: Expression, context: Context): Type = {
    e match {
      case Var(Variable(id)) =>
        context.variableEnv.get(id) match {
          case Some(t) => t
          case None => errorAt(e, UnboundVariable(id))
        }
      case Val(Value(w, b)) =>
        w match {
          case Const(k) => Type(IntType, Static(b))
          case Unit => Type(UnitType, Static(b))
          case Loc(l, t_, p) =>
            context.addressEnv.get(l) match {
              case Some(t) => Type(RefType(t_), Static(b))
              case None => errorAt(e, InvalidMemoryLocation(l))
            }
          case Lambda(Variable(id), t, pc_, body) =>
            val t_ = infer(body, context.withVariable(id, t))
            Type(FuncType(t, pc_, t_), Static(b))
        }
      case Apply(e1, e2) =>
        val t1 = infer(e1, context) // func type
        val t2 = infer(e2, context) // val type

        t1 match {
          case Type(FuncType(from, pc, to), b) =>
            if (!(t2 ≺ from)) {
              errorAt(e, TypeMismatch(from, t2))
            }
            if !((context.pc ⊔ b) ⊑ pc) then
              errorAt(e, ProgramCounterViolation(context.pc, pc))

            Type(to.s, to.annotation ⊔ b)

          case other => errorAt(e, CannotApplyNonFunction(other))
        }

      case Let(x, e1, e2) =>
        val t1 = infer(e1, context)
        infer(e2, context.withVariable(x.id, t1))

      case Seq(e1, e2) =>
        infer(e1, context)
        infer(e2, context)

      case New(t, b, e2) =>
        val te = infer(e2, context)
        if !(te ≺ t) then
          errorAt(e, TypeMismatch(t, te))
        if !(context.pc ⊑ t.annotation) then
          errorAt(e, ProgramCounterViolation(context.pc, t.annotation))
        Type(RefType(t), Static(b))

      case Prot(b, e2) =>
        val te = infer(e2, context)
        Type(te.s, te.annotation ⊔ Static(b))

      case Bang(e2) =>
        val te = infer(e2, context)

        te match {
          case Type(RefType(Type(s, b_)), b) =>
            Type(s, b_ ⊔ b)
          case _ => errorAt(e, ExpectedRefType(te))
        }

      case Assign(e1, e2) =>
        val te1 = infer(e1, context)
        val te2 = infer(e2, context)

        te1 match {
          case Type(RefType(sb_), b) =>
            val b_ = sb_.annotation

            if (!((context.pc ⊔ b) ⊑ b_))
              errorAt(e, ProgramCounterViolation(context.pc ⊔ b, b_))

            if (!(te2 ≺ sb_)) {
              errorAt(e, TypeMismatch(sb_, te2))
            }

            Type(UnitType, b_)
          case _ => errorAt(e, ExpectedRefType(te1))
        }

      case Cast(t_, t, p, e2) =>
        val te = infer(e2, context)

        if (!(te ≺ t)) {
          errorAt(e, TypeMismatch(t, te))
        }

        if (!Type.compatible(t, t_)) {
          errorAt(e, IncompatibleCast(t, t_))
        }

        t_

      /*
      case lang.Expression.GuardCast(_, _, _, _) => ???
      case lang.Expression.PointerCast(_, _, _, _) => ???
      case lang.Expression.FunctionGuardCast(_, _, _, _) => ??? */
    }
  }
}