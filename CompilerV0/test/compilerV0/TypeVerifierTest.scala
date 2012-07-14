package compilerV0

object TypeVerifierTest {
  def run() {
    println("Type Verifier Tests")
    
    // TODO turn these into actual tests...
    /*
        println(TypeVerifier(Parser("""println({val t0, t1 : Double = 5.5; {val t1 : Int = 6; val t1 : Int = 7; println(t1)}; t0 + t1;})""")));
	    println(TypeVerifier(Parser("""{var x, y, z : Int = if (true) 5.0 else 6.0;}""")));
	    println(TypeVerifier(Parser("""{var f : Int = 5; def bar (x : Any, y : Int, s : String): AnyVal = f; bar("", 5, "");}""")));
	    
	    TypeVerifier.initScalaTypes();
	    println("Should be true: "+TypeVerifier.checkType(new FuncType(new BaseType("Int"), List(new BaseType("Int"))), new FuncType(new BaseType("Int"), List(new BaseType("Int")))));
	    println("Should be false: "+TypeVerifier.checkType(new FuncType(new BaseType("Int"), List(new BaseType("Boolean"))), new FuncType(new BaseType("Int"), List(new BaseType("Int")))));
	    println("Should be false: "+TypeVerifier.checkType(new FuncType(new BaseType("Any"), List(new BaseType("Any"))), new FuncType(new BaseType("AnyVal"), List(new BaseType("AnyVal")))));
	    println("Should be true: "+TypeVerifier.checkType(new FuncType(new BaseType("Int"), List(new BaseType("Any"))), new FuncType(new BaseType("AnyVal"), List(new BaseType("AnyVal")))));
	    println(TypeVerifier.firstCommonSuperType(new FuncType(new BaseType("Int"), List(new BaseType("Any"))), new FuncType(new BaseType("AnyVal"), List(new BaseType("Any")))));
	    println(TypeVerifier.firstCommonSuperType(new FuncType(new BaseType("Int"), List(new BaseType("Any"))), new BaseType("String")));
	*/
  }
}