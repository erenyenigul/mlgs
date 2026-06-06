package static

import lang.RawType.{FuncType, IntType, RefType, UnitType}
import lang.RawValue.{Const, Lambda, Loc, Unit}
import lang.SecurityLevel.L
import lang.TypeAnnotation.Static
import lang.{Expression, SecurityLevel, Type, TypeAnnotation, Value, Variable}
import lang.Expression.*
import static.*
import static.TypeError.{CannotApplyNonFunction, ExpectedRefType, InvalidMemoryLocation, ProgramCounterViolation, TypeMismatch, UnboundVariable, IncompatibleCast}

import scala.collection.mutable

type Env [A, B] = mutable.Map[A, B]

case class Context(
    pc: TypeAnnotation = Static(L),
    variableEnv: Env[String, Type] = mutable.Map(),
    addressEnv: Env[String, Type] = mutable.Map()
) {
  def withVariable(name: String, t: Type): Context =
    this.copy(variableEnv = this.variableEnv + (name -> t))

  def withAllocation(loc: String, t: Type): Context =
    this.copy(addressEnv = this.addressEnv + (loc -> t))

  def withPC(newPC: TypeAnnotation): Context =
    this.copy(pc = newPC)
}

object TypeChecker {

  def infer(e: Expression, context: Context): Type = {
    e match {
      case Var(Variable(id)) =>
        context.variableEnv.get(id) match {
          case Some(t) => t
          case None => throw UnboundVariable(id)
        }
      case Val(Value(w, b)) =>
        w match {
          case Const(k) => Type(IntType, Static(b))
          case Unit => Type(UnitType, Static(b))
          case Loc(l, t_, p) =>
            context.addressEnv.get(l) match {
              case Some(t) => Type(RefType(t_), Static(b))
              case None => throw InvalidMemoryLocation(l)
            }
          case Lambda(Variable(id), t, pc_, e) =>
            val t_ = infer(e, context.withVariable(id, t))

            Type(FuncType(t, pc_, t_), Static(b))
        }
      case Apply(e1, e2) =>
        val t1 = infer(e1, context) // func type
        val t2 = infer(e2, context) // val type

        t1 match {
          case Type(FuncType(from, pc, to), b) =>
            if (!(t2 ≺ from)) {
              throw TypeMismatch(from, t2)
            }
            if !((context.pc ⊔ b) ⊑ pc) then
              throw ProgramCounterViolation(context.pc, pc)

            println(b)
            println(to)
            Type(to.s, to.annotation ⊔ b)

          case other => throw CannotApplyNonFunction(other)
        }

      case Let(x, e1, e2) =>
        val t1 = infer(e1, context)
        infer(e2, context.withVariable(x.id, t1))

      case Seq(e1, e2) =>
        infer(e1, context)
        infer(e2, context)

      case New(t, b, e) =>
        val te = infer(e, context)
        if !(te ≺ t) then
          throw TypeMismatch(t, te)
        if !(context.pc ⊑ t.annotation) then
          throw ProgramCounterViolation(context.pc, t.annotation)
        Type(RefType(t), Static(b))

      case Prot(b, e) =>
        val te = infer(e, context)

        Type(
          te.s, te.annotation ⊔ Static(b)
        )

      case Bang(e) =>
        val te = infer(e, context)

        te match {
          case Type(RefType(Type(s, b_)), b) =>
            Type(s, b_ ⊔ b)
          case _ => throw ExpectedRefType(te)
        }

      case Assign(e1, e2) =>
        val te1 = infer(e1, context)
        val te2 = infer(e2, context)

        te1 match {
          case Type(RefType(sb_), b) =>
            val b_ = sb_.annotation

            if (!((context.pc ⊔ b) ⊑ b_))
             throw ProgramCounterViolation(context.pc ⊔ b, b_)

            if (!(te2 ≺ sb_)) {
              throw TypeMismatch(sb_, te2)
            }

            Type(UnitType, b_)
          case _ => throw ExpectedRefType(te1)
        }

      case Cast(t_, t, p, e) =>
        val te = infer(e, context)

        if (!(te ≺ t)) {
          throw TypeMismatch(t, te)
        }

        if (!Type.compatible(t, t_)) {
          throw IncompatibleCast(t, t_)
        }

        t_

      /*
      case lang.Expression.GuardCast(_, _, _, _) => ???
      case lang.Expression.PointerCast(_, _, _, _) => ???
      case lang.Expression.FunctionGuardCast(_, _, _, _) => ??? */
    }
  }
}
