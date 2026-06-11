import static.TypeChecker
import runtime.Interpreter
import lang.{Value, Type, RawValue, RawType, TypeAnnotation, SecurityLevel}

object ProgramTests:

  def run(source: String): Either[Exception, (Value, runtime.State)] =
    for
      parsed <- Parser.run(source)
      (program, _) = parsed
      _ <- TypeChecker(program, source).run()
      res <- Interpreter(program, source).run()
    yield res

  def typecheck(source: String): Either[Exception, Type] =
    for
      parsed <- Parser.run(source)
      (program, _) = parsed
      t <- TypeChecker(program, source).run()
    yield t

  def assertThrows[T <: Throwable : reflect.ClassTag](block: => Any): Unit =
    try
      block
      throw AssertionError(s"Expected ${reflect.classTag[T].runtimeClass.getSimpleName} but nothing was thrown")
    catch
      case e: AssertionError => throw e
      case e if reflect.classTag[T].runtimeClass.isInstance(e) => ()
      case e => throw AssertionError(s"Expected ${reflect.classTag[T].runtimeClass.getSimpleName} but got ${e.getClass.getSimpleName}: ${e.getMessage}")

  val tests: List[(String, () => Unit)] = List(

    "parse: int literal H level" -> (() =>
      val result = run("42[H]")
      assert(result.isRight, result)
      val Right((Value(RawValue.Const(42), SecurityLevel.H), _)) = result: @unchecked
      ),

    "parse: let binding" -> (() =>
      val result = run("let x = 5; x")
      assert(result.isRight, result)
      val Right((Value(RawValue.Const(5), SecurityLevel.L), _)) = result: @unchecked
      ),

    "parse: named fn single arg" -> (() =>
      val result = run("""
        fn double(x : int[L]) -[L]-> { x };
        double 7
      """)
      assert(result.isRight, result)
      val Right((Value(RawValue.Const(7), _), _)) = result: @unchecked
      ),

    "parse: multi-arg fn curried application" -> (() =>
      val result = run("""
        fn first(x : int[L], y : int[L]) -[L]-> { x };
        (first 3) 4
      """)
      assert(result.isRight, result)
      val Right((Value(RawValue.Const(3), _), _)) = result: @unchecked
      ),

    "parse: ref alloc and deref" -> (() =>
      val result = run("""
        let r = new(int[L], L) 99;
        !r
      """)
      assert(result.isRight, result)
      val Right((Value(RawValue.Const(99), _), _)) = result: @unchecked
      ),

    "parse: ref assignment" -> (() =>
      val result = run("""
        let r = new(int[L], L) 0;
        r := 42;
        !r
      """)
      assert(result.isRight, result)
      val Right((Value(RawValue.Const(42), _), _)) = result: @unchecked
      ),

    "parse: as cast to dynamic" -> (() =>
      val result = typecheck("5 as int[?]")
      assert(result.isRight, result)
      val Right(Type(RawType.IntType, TypeAnnotation.Dyn)) = result: @unchecked
      ),

    "parse: lambda explicit H value level" -> (() =>
      val result = typecheck("lambda[H](x : int[L]) -[L]-> { x }")
      assert(result.isRight, result)
      val Right(Type(_, TypeAnnotation.Static(SecurityLevel.H))) = result: @unchecked
      ),

    "parse: prot raises level" -> (() =>
      val result = run("prot H 0")
      assert(result.isRight, result)
      val Right((Value(RawValue.Const(0), SecurityLevel.H), _)) = result: @unchecked
      ),

    "parse: NSU violation raises blame at runtime" -> (() =>
      val source = """
        let pub = new(int[L], L) 0;
        let dyn = pub as ref[?]<int[?]>;
        prot H (dyn := 42)
      """
      assert(typecheck(source).isRight, "should type-check")
      assertThrows[Exception](run(source).fold(throw _, _ => ()))
      ),

    "parse: sequence returns second expr" -> (() =>
      val result = run("1[H]; 99")
      assert(result.isRight, result)
      val Right((Value(RawValue.Const(99), SecurityLevel.L), _)) = result: @unchecked
      ),

    "parse: nested let bindings" -> (() =>
      val result = run("let x = 3; let y = 7; x")
      assert(result.isRight, result)
      val Right((Value(RawValue.Const(3), _), _)) = result: @unchecked
      ),

    // deref H pointer: type is H (content L joined with pointer level H)
    "parse: deref H pointer type is H" -> (() =>
      val result = typecheck("""
        let r = new(int[L], H) 5;
        !r
      """)
      assert(result.isRight, result)
      val Right(Type(RawType.IntType, TypeAnnotation.Static(SecurityLevel.H))) = result: @unchecked
      ),

    "parse: cast dynamic value to H succeeds when level matches" -> (() =>
      val result = run("""
        let x = 42[H] as int[?];
        x as int[H]
      """)
      assert(result.isRight, result)
      ),

    "parse: cast H value to L fires blame" -> (() =>
      val source = "42[H] as int[?] as int[L]"
      assert(typecheck(source).isRight, "should type-check")
      assertThrows[Exception](run(source).fold(throw _, _ => ()))
      ),

    // H-level fn: result type joins with H; need matching H effect to call under H PC
    "parse: H-level fn type has H annotation" -> (() =>
      val result = typecheck("lambda[H] (x : int[L]) -[H]-> { x }")
      assert(result.isRight, result)
      val Right(Type(RawType.FuncType(_, _, _), TypeAnnotation.Static(SecurityLevel.H))) = result: @unchecked
      ),

    // H-pc lambda body: allocating L ref under H pc is a type error
    "parse: H pc cannot allocate L ref" -> (() =>
      val source = "lambda(x : int[H]) -[H]-> { new(int[L], L) 0 }"
      assertThrows[Exception](typecheck(source).fold(throw _, _ => ()))
      ),

    "parse: H pc can allocate H ref" -> (() =>
      val result = run("prot H (new(int[H], L) 0[H])")
      assert(result.isRight, result)
      ),

    "parse: higher-order function" -> (() =>
      val result = run("""
        fn apply(f : (int[L] -[L]-> int[L])[L], x : int[L]) -[L]-> { f x };
        fn inc(n : int[L]) -[L]-> { n };
        (apply inc) 10
      """)
      assert(result.isRight, result)
      val Right((Value(RawValue.Const(10), _), _)) = result: @unchecked
      ),

    "parse: omitted annotation defaults to L" -> (() =>
      val result = typecheck("7")
      assert(result.isRight, result)
      val Right(Type(RawType.IntType, TypeAnnotation.Static(SecurityLevel.L))) = result: @unchecked
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

@main def runProgramTests(): Unit = ProgramTests.run()
