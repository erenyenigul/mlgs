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
    }

  private def resolveType(t: Type, v: Value): Type =
    t.annotation match
      case Static(_) => t
      case Dyn       => Type(t.s, Static(v.B))

  private def interp(e: Expression, state: State) : (Value, State) =
    e match {
      case Val(v) => (v, state)
      case Apply(target, arg) =>
        val (v, state1) = interp(arg, state)
        val (Value(f, b), state2) = interp(target, state1)

        f match {
          case Lambda(x, paramType, pc, body) =>
            interp(
              Prot(b, subst(body, x, v)), state
            )
            // other cases must be handled by type checker
        }

      case Prot(b, Val(Value(w, b_))) => (Value(w, b ⊔ b_), state)

      case Prot(b, e) =>
        interp(e, state.withPC(state.pc ⊔ b))

      case New(t, b, innerExpr) =>
        val (v, state1) = interp(innerExpr, state)

        val resolvedT = resolveType(t, v)
        val loc = FreshLoc()
        val blameLabel = FreshBlame("alloc", e.pos.line, e.pos.column, getSourceLine(e.pos.line))

        (
          Value(Loc(loc, resolvedT, blameLabel), b),
          state1.withAllocation(
            loc,
            HeapCell(
              resolvedT,
              blameLabel,
              Value(v.w, v.B ⊔ state1.pc)
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

      case Cast(Type(s1,b1), Type(s2,b2), p, innerE) =>
        val (Value(w2, b), state1) = interp(innerE, state)

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
