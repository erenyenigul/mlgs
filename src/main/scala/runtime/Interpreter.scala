package runtime

import lang.*

import scala.util.parsing.input.NoPosition
import lang.Expression.*
import lang.RawValue.*
import lang.TypeAnnotation.*
import runtime.InterpreterError.BlameError
import util.{FreshBlame, FreshLoc}

class Interpreter (program: Expression, source: String = "") {

  def run(state: State = State()): Either[Exception, (Value, State)] =
    try Right(interp(program, state))
    catch case e: Exception => Left(e)

  private def getSourceLine(line: Int): String =
    source.linesIterator.drop(line - 1).nextOption().getOrElse("")

  private def errorAt(e: Expression, err: InterpreterError): Nothing =
    val pos = e.pos
    if pos == NoPosition then throw err
    else throw InterpreterErrorAt(err, pos.line, pos.column, getSourceLine(pos.line))

  private def subst(in: Expression, id: Variable, v: Value) : Expression =
    def rebuild(node: Expression, result: Expression): Expression = result.setPos(node.pos)
    in match {
      case node @ Val(Value(Lambda(x, paramType, pc, e), b)) => if x == id then
        node
      else
        rebuild(node, Val(Value(Lambda(x, paramType, pc, rebuild(e, subst(e, id, v))), b)))
      case node @ Val(_) => node
      case node @ Var(x) => if x == id then rebuild(node, Val(v)) else node
      case node @ Apply(target, arg) => rebuild(node, Apply(subst(target, id, v), subst(arg, id, v)))
      case node @ New(t, b, e) => rebuild(node, New(t, b, subst(e, id, v)))
      case node @ Bang(e) => rebuild(node, Bang(subst(e, id, v)))
      case node @ Assign(lhs, rhs) => rebuild(node, Assign(subst(lhs, id, v), subst(rhs, id, v)))
      case node @ Cast(to, from, p, e) => rebuild(node, Cast(to, from, p, subst(e, id, v)))
      case node @ Prot(b, e) => rebuild(node, Prot(b, subst(e, id, v)))
      case node @ GuardCast(to, from, p, e) => rebuild(node, GuardCast(to, from, p, subst(e, id, v)))
      case node @ PointerCast(to, from, p, e) => rebuild(node, PointerCast(to, from, p, subst(e, id, v)))
      case node @ FunctionGuardCast(to, from, p, e) => rebuild(node, FunctionGuardCast(to, from, p, subst(e, id, v)))
      case node @ Let(x, e1, e2) =>
        if x == id then rebuild(node, Let(x, subst(e1, id, v), e2))
        else rebuild(node, Let(x, subst(e1, id, v), subst(e2, id, v)))
      case node @ Seq(e1, e2) => rebuild(node, Seq(subst(e1, id, v), subst(e2, id, v)))
      case node @ LambdaExp(x, paramType, pc, vl, e) =>
        if x == id then node
        else rebuild(node, LambdaExp(x, paramType, pc, vl, rebuild(e, subst(e, id, v))))
      case node @ UntypedCast(to, p, e) => rebuild(node, UntypedCast(to, p, subst(e, id, v)))
    }

  private def resolveType(t: Type, v: Value): Type =
    t.annotation match
      case Static(_) => t
      case Dyn       => Type(t.s, Static(v.B))

  private def interp(e: Expression, state: State) : (Value, State) = {
    e match {
      case Val(v) => (v, state)
      case Apply(target, arg) =>
        val (v, state1) = interp(arg, state)
        val (Value(f, b), state2) = interp(target, state1)

        f match {
          case Lambda(x, paramType, pc, body) =>
            interp(
              Prot(b, subst(body, x, v)), state2
            )
            // other cases must be handled by type checker
        }

      case Prot(b, Val(Value(w, b_))) =>
        (Value(w, b ⊔ b_), state)

      case Prot(b, e) =>
        val (v, state1) = interp(e, state.withPC(state.pc ⊔ b))
        val finalState = state1.withPC(state.pc)
        interp(Prot(b, Val(v)), finalState)

      case New(t, b, innerExpr) =>
        val (v, state1) = interp(innerExpr, state)

        val resolvedT = resolveType(t, v)
        val loc = FreshLoc()
        val blameLabel = FreshBlame("alloc", e.pos.line, e.pos.column, getSourceLine(e.pos.line))

        val declaredLevel = resolvedT.annotation match
          case Static(l) => l
          case Dyn => v.B // can't happen after resolveType

        (
          Value(Loc(loc, resolvedT, blameLabel), b),
          state1.withAllocation(
            loc,
            HeapCell(
              resolvedT,
              blameLabel,
              Value(v.w, v.B ⊔ state1.pc ⊔ declaredLevel)
            ))
        )

      case Bang(innerE) =>
        val (Value(w, b), state1) = interp(innerE, state)

        w match {
          case Loc(l, t2_, q) =>
            val cell = state1.heap.get(l)

            cell match {
              case Some(HeapCell(t2, p, v)) => interp(
                Prot(b, Cast(t2_, t2, p.concat(q), Val(v)).setPos(e.pos)).setPos(e.pos),
                state1
              )
            }
        }

      case Assign(lhs, rhs) =>
        val (Value(w, b2), state1) = interp(rhs, state)
        val (Value(Loc(l, Type(s, b2_), q), b), state2) = interp(lhs, state1)

        state2.heap.get(l) match {
          case Some(HeapCell(Type(s1, _), p, Value(w1, b1))) =>
            // NSU check: use actual runtime level b1 of stored value
            if !((state2.pc ⊔ b) ⊑ b1) then
              errorAt(lhs, BlameError(p.concat(q)))

            val newLevel = b2 ⊔ state2.pc ⊔ b
            (
              Value(Unit, state2.pc),
              state2.withAllocation(
                l, HeapCell(
                  Type(s, Static(newLevel)),  // concrete level at runtime
                  p.concat(q),
                  Value(w, newLevel)
                )
              )
            )
          case None => errorAt(lhs, BlameError(q))
        }

      case GuardCast(to, from, p, innerE) =>
        innerE match
          case Val(v) => (v, state)

          case GuardCast(to2, from2, q, inner) =>
            interp(GuardCast(to, from2, p.concat(q), inner), state)

          case New(t, b, inner) =>
            interp(New(Type(t.s, t.annotation ⊔ to), b, GuardCast(to, from, p, inner)), state)

          case Assign(lhs, rhs) =>
            interp(
              Assign(
                PointerCast(to, from, p, GuardCast(to, from, p, lhs)),
                GuardCast(to, from, p, rhs)
              ),
              state
            )

          case Apply(e1, e2) =>
            interp(
              Apply(
                FunctionGuardCast(to, from, p, GuardCast(to, from, p, e1)),
                GuardCast(to, from, p, e2)
              ),
              state
            )

          case _ =>
            val (v, state1) = interp(innerE, state)
            interp(GuardCast(to, from, p, Val(v)), state1)

      case FunctionGuardCast(to, from, p, innerE) =>
        val (Value(w, b), state1) = interp(innerE, state)
        w match
          case Lambda(x, paramType, _, body) =>
            (Value(Lambda(x, paramType, to, GuardCast(to, from, p, body)), b), state1)
          case _ => throw new RuntimeException(s"FunctionGuardCast applied to non-lambda: $w")

      case PointerCast(to, from, p, innerE) =>
        val (Value(w, b), state1) = interp(innerE, state)
        w match
          case Loc(l, Type(s, annot), q) =>
            (Value(Loc(l, Type(s, annot ⊔ to), p.concat(q)), b), state1)
          case _ => throw new RuntimeException(s"PointerCast applied to non-pointer: $w")

      case Cast(Type(s1,b1), Type(s2,b2), p, innerE) =>
        val (Value(w2, b), state1) = interp(innerE, state)

        val w1 = w2.propagate(s1, s2, p)

        b1 match
          case Dyn =>
            (Value(w1, b), state1)
          case Static(b1_) =>
            if !(b ⊑ b1_) then errorAt(e, BlameError(p))

            (Value(w1, b ⊔ b1_), state1)

      case LambdaExp(x, paramType, pc, valueLevel, body) =>
        (Value(Lambda(x, paramType, pc, body), valueLevel), state)

      case UntypedCast(Type(s1, b1), p, innerE) =>
        val (Value(w2, b), state1) = interp(innerE, state)
        val s2 = inferRawType(w2, s1)
        val w1 = w2.propagate(s1, s2, p)
        b1 match
          case Dyn =>
            (Value(w1, b), state1)
          case Static(b1_) =>
            if !(b ⊑ b1_) then errorAt(e, BlameError(p))
            (Value(w1, b ⊔ b1_), state1)

      case Let(x, e1, e2) =>
        val (v, state1) = interp(e1, state)
        interp(subst(e2, x, v), state1)

      case Seq(e1, e2) =>
        val (_, state1) = interp(e1, state)
        interp(e2, state1)

    }
  }

  private def inferRawType(w: RawValue, toRaw: RawType): RawType = w match
    case Const(_)           => RawType.IntType
    case Unit               => RawType.UnitType
    case Loc(_, t, _)       => RawType.RefType(t)
    case Lambda(_, _, _, _) => toRaw
}
