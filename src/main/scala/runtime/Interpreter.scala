package runtime

import lang.*

import scala.util.parsing.input.NoPosition
import lang.Expression.*
import lang.RawValue.*
import lang.TypeAnnotation.*
import runtime.InterpreterError.BlameError
import util.{FreshBlame, FreshLoc}

class Interpreter (program: Expression, source: String = "") {

  def run(state: State = State()): Value = interp(program, state)._1

  private def getSourceLine(line: Int): String =
    source.linesIterator.drop(line - 1).nextOption().getOrElse("")

  private def errorAt(e: Expression, err: InterpreterError): Nothing =
    val pos = e.pos
    if pos == NoPosition then throw err
    else throw InterpreterErrorAt(err, pos.line, pos.column, getSourceLine(pos.line))

  private def subst(in: Expression, id: Variable, v: Value) : Expression = in match {
    case Val(w) => Val(w)
    case Var(x) => if x == id then Val(v) else Var(x)
    case Apply(target: Expression, arg: Expression) => Apply(
      subst(target, id, v), subst(arg, id, v)
    )

    case New(t: Type, B: SecurityLevel, e: Expression) => New(
      t, B, subst(e, id, v)
    )
    case Bang(e: Expression) => Bang(
      subst(e, id, v)
    )
    case Assign(lhs: Expression, rhs: Expression) => Assign(
      subst(lhs, id, v),
      subst(rhs, id, v),
    )
    case Cast(to: Type, from: Type, p: BlameLabel, e: Expression) => Cast(
      to, from, p, subst(e, id, v)
    )
    case Prot(B: SecurityLevel, e: Expression) => Prot(B, subst(e, id, v))
    case GuardCast(to: TypeAnnotation, from: TypeAnnotation, p: BlameLabel, e: Expression) => GuardCast(
      to, from, p, subst(e, id, v)
    )
    case PointerCast(to: TypeAnnotation, from: TypeAnnotation, p: BlameLabel, e: Expression) => PointerCast(
      to, from, p, subst(e, id, v)
      )
    case FunctionGuardCast(to: TypeAnnotation, from: TypeAnnotation, p: BlameLabel, e: Expression) => FunctionGuardCast(
      to, from, p, subst(e, id, v)
      )

    // sugar
    case Let(x, e1, e2) =>
      if x == id then Let(x, subst(e1, id, v), e2)
      else Let(x, subst(e1, id, v), subst(e2, id, v))

    case Seq(e1: Expression, e2: Expression) => Seq(
      subst(e1, id, v),
      subst(e2, id, v),
    )
  }

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

      case New(t, b, e) =>
        val (v, state1) = interp(e, state)

        val loc = FreshLoc()
        val blameLabel = FreshBlame()

        (
          Value(Loc(loc, t, blameLabel), b),
          state1.withAllocation(
            loc,
            HeapCell(
              t,
              blameLabel,
              Value(v.w, v.B ⊔ state1.pc)
            ))
        )

      case Bang(e) =>
        val (Value(w, b), state1) = interp(e, state)

        w match {
          case Loc(l, t2_, q) =>
            val cell = state1.heap.get(l)

            cell match {
              case Some(HeapCell(t2, p, v)) => interp(
                Prot(b, Cast(t2_, t2, p.concat(q), Val(v))),
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

      case Cast(Type(s1,b1), Type(s2,b2), p, e) =>
        val (Value(w2, b), state1) = interp(e, state)

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
