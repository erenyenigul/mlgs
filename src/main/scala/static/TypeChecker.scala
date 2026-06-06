package static

import lang.RawType.{FuncType, IntType, RefType, UnitType}
import lang.RawValue.{Const, Lambda, Loc, Unit}
import lang.SecurityLevel.L
import lang.TypeAnnotation.Static
import lang.{Expression, SecurityLevel, Type, TypeAnnotation, Value, Variable}
import lang.Expression.*
import static.*
import static.TypeError.{CannotApplyNonFunction, InvalidMemoryLocation, ProgramCounterViolation, TypeMismatch, UnboundVariable}

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
            if (t2 != from) {
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
    }
  }
}
