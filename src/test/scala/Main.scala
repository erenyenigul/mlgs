package test

import lang.*
import lang.SecurityLevel.*
import lang.TypeAnnotation.*
import lang.RawType.*
import lang.RawValue.*
import lang.Expression.*
import static.TypeChecker

object Tests:

  // helper to build types cleanly
  def intT(b: SecurityLevel)  = Type(IntType, Static(b))
  def unitT(b: SecurityLevel) = Type(UnitType, Static(b))
  def refT(b: SecurityLevel, t: Type) = Type(RefType(t), Static(b))
  def funT(from: Type, pc: TypeAnnotation, to: Type, b: SecurityLevel) =
    Type(FuncType(from, pc, to), Static(b))

  def assertThrows[T <: Throwable : reflect.ClassTag](block: => Any): Unit =
    try
      block
      throw AssertionError(s"Expected ${reflect.classTag[T].runtimeClass.getSimpleName} but nothing was thrown")
    catch
      case e: AssertionError => throw e
      case e if reflect.classTag[T].runtimeClass.isInstance(e) => ()
      case e => throw AssertionError(s"Expected ${reflect.classTag[T].runtimeClass.getSimpleName} but got ${e.getClass.getSimpleName}")

  def v(w: RawValue, b: SecurityLevel = L) = Val(Value(w, b))
  def int(k: Int, b: SecurityLevel = L)    = v(Const(k), b)
  def ctx = static.Context()

  val tests: List[(String, () => Unit)] = List(

    // T-Int: low security integer
    "int literal L" -> (() =>
      val e = int(5)
      assert(TypeChecker(e).run(ctx) == intT(L))
      ),

    // T-Int: high security integer
    "int literal H" -> (() =>
      val e = int(5, H)
      assert(TypeChecker(e).run(ctx) == intT(H))
      ),

    // T-Var: bound variable
    "variable lookup" -> (() =>
      val e = Var(Variable("x"))
      val c = ctx.withVariable("x", intT(L))
      assert(TypeChecker(e).run(c) == intT(L))
      ),

    // T-Var: unbound variable throws
    "unbound variable" -> (() =>
      val e = Var(Variable("x"))
      assertThrows[static.TypeError](TypeChecker(e).run(ctx))
      ),

    // T-Abs + T-App: identity function
    "identity function" -> (() =>
      val lam = v(Lambda(Variable("x"), intT(L), Static(L), Var(Variable("x"))))
      val app = Apply(lam, int(5))
      assert(TypeChecker(app).run(ctx) == intT(L))
      ),

    // T-App: security level joins
    "apply H function to L arg" -> (() =>
      val lam = v(Lambda(Variable("x"), intT(L), Static(H), Var(Variable("x"))), H)
      val app = Apply(lam, int(5))
      // result annotation should join with function's B=H
      assert(TypeChecker(app).run(ctx) == intT(H))
      ),

    // T-New: allocate low int in low ref
    "new low ref" -> (() =>
      val e = New(intT(L), L, int(5))
      assert(TypeChecker(e).run(ctx) == refT(L, intT(L)))
      ),

    // T-New: allocate high int in low ref (public pointer to secret)
    "new low ref high content" -> (() =>
      val e = New(intT(H), L, int(5, H))
      assert(TypeChecker(e).run(ctx) == refT(L, intT(H)))
      ),

    // T-New: pc violation - H pc allocating L content
    "new pc violation" -> (() =>
      val e = New(intT(L), L, int(5))
      val c = ctx.withPC(Static(H))
      assertThrows[static.TypeError](TypeChecker(e).run(c))
      ),

    // T-Deref: dereference low ref
    "deref low ref" -> (() =>
      val ref = New(intT(L), L, int(5))
      val e   = Bang(ref)
      assert(TypeChecker(e).run(ctx) == intT(L))
      ),

    // T-Deref: dereference high ref raises level
    "deref high ref" -> (() =>
      val ref = New(intT(L), H, int(5))
      val e   = Bang(ref)
      // result joins content type L with pointer level H
      assert(TypeChecker(e).run(ctx) == intT(H))
      ),

    // T-Asgn: valid assignment
    "assign low to low ref" -> (() =>
      val e = Let(
        Variable("r"),
        New(intT(L), L, int(0)),
        Assign(Var(Variable("r")), int(5))
      )
      assert(TypeChecker(e).run(ctx) == unitT(L))
      ),

    // T-Asgn: pc violation - assigning under H pc to L ref
    "assign under H pc violation" -> (() =>
      val e = Let(
        Variable("r"),
        New(intT(L), L, int(0)),
        Assign(Var(Variable("r")), int(5))
      )
      val c = ctx.withPC(Static(H))
      assertThrows[static.TypeError](TypeChecker(e).run(c))
      ),

    // T-Cast: compatible cast L to H
    "cast int L to H" -> (() =>
      val e = Cast(intT(H), intT(L), Set(BlameId("p")), int(5))
      assert(TypeChecker(e).run(ctx) == intT(H))
      ),

    // T-Cast: incompatible cast int to ref
    "incompatible cast" -> (() =>
      val e = Cast(refT(L, intT(L)), intT(L), Set(BlameId("p")), int(5))
      assertThrows[static.TypeError](TypeChecker(e).run(ctx))
      ),

    // T-Cast: cast to dynamic
    "cast to dynamic" -> (() =>
      val e = Cast(Type(IntType, Dyn), intT(L), Set(BlameId("p")), int(5))
      assert(TypeChecker(e).run(ctx) == Type(IntType, Dyn))
      ),

    // Let binding
    "let binding" -> (() =>
      val e = Let(Variable("x"), int(5), Var(Variable("x")))
      assert(TypeChecker(e).run(ctx) == intT(L))
      ),

    // Sequencing
    "sequence" -> (() =>
      val e = Seq(int(5), int(42, H))
      assert(TypeChecker(e).run(ctx) == intT(H))
      ),

    // Prot raises security level
    "prot raises level" -> (() =>
      val e = Prot(H, int(5))
      assert(TypeChecker(e).run(ctx) == intT(H))
      ),

    // Non-interference example from paper Section II
    // addPrivileged false sendToFacebook should be safe
    "paper example: public report to facebook" -> (() =>
      val reportT = intT(L)
      val sendToFacebook = v(
        Lambda(Variable("r"), refT(L, reportT), Static(L),
          Var(Variable("r"))  // simplified body
        ), L
      )
      val e = Let(
        Variable("w"),
        Cast(
          funT(refT(L, Type(IntType, Dyn)), Dyn, refT(L, Type(IntType, Dyn)), L),
          funT(refT(L, reportT), Static(L), refT(L, reportT), L),
          Set(BlameId("p")),
          sendToFacebook
        ),
        Var(Variable("w"))
      )
      TypeChecker(e).run(ctx) // should not throw
      ),
  )

  def run(): Unit =
    var passed = 0
    var failed = 0
    for (name, test) <- tests do
      try
        test()
        println(s"✓ $name")
        passed += 1
      catch
        case e: AssertionError =>
          println(s"✗ $name: assertion failed - ${e.getMessage}")
          failed += 1
        case e: Exception =>
          println(s"✗ $name: threw ${e.getClass.getSimpleName} - ${e.getMessage}")
          failed += 1
    println(s"\n$passed passed, $failed failed")

@main def runTests(): Unit = Tests.run()