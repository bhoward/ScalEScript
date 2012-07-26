package compilerV0

object TypeVerifierTest extends Test {
  def checkSrcType(src: String, expect: TypedStmt) {
    try {
      val actual = TypeVerifier(ASTConverter(Parser(src)))
      if (actual != expect) {
        System.err.println("Checking type of " + src + "\nExpected: " + expect + "\n  Actual: " + actual);
      }
    } catch {
      case e: Exception => System.err.println("Error while type verifying " + src + ".\n" + e + "\n")
    }
  }
  
  def checkSrcTypeThrows(src: String, expect: String) {
    shouldThrow(expect) {
      TypeVerifier(ASTConverter(Parser(src)))
    }
  }
  
  def checkSubType(first: Type, second: Type, expect: Boolean) {
    try {
      val actual = TypeVerifier.checkType(first, second, ScalaBase.getScope()::Nil)
      if (actual != expect) {
        System.err.println("Checking subtype " + first + " of " + second + "\nExpected: " + expect + "\n  Actual: " + actual);
      }
    } catch {
      case e: Exception => System.err.println("Error while type comparing " + first + " with " + second + ".\n" + e + "\n")
    }
  }
  
  def checkJoinType(first: Type, second: Type, expect: Type) {
    try {
      val actual = TypeVerifier.firstCommonSuperType(first, second, ScalaBase.getScope()::Nil)
      if (actual != expect) {
        System.err.println("Checking join type of " + first + " and " + second + "\nExpected: " + expect + "\n  Actual: " + actual);
      }
    } catch {
      case e: Exception => System.err.println("Error while type joining " + first + " with " + second + ".\n" + e + "\n")
    }
  }

  def run() {
    println("Type Verifier Tests")

    checkSrcTypeThrows("""println({val t0, t1 : Double = 5.5; {val t1 : Int = 6; val t1 : Int = 7; println(t1)}; t0 + t1;})""",
        "The variable t1 is already defined in the current scope.");
	checkSrcTypeThrows("""{var x, y, z : Int = if (true) 5.0 else 6.0;}""",
	    "Type BaseType(Double) does not match the required type BaseType(Int) for vals x, y, z.");
	checkSrcType("""{var f : Int = 5; def bar (x : Any, y : Int, s : String): AnyVal = f; bar("", 5, "");}""",
	    TypedBlockExpr(List(TypedValDefStmt(List("f"),BaseType("Int"),TypedNumExpr(NInt(5),BaseType("Int")),"var"),
	                        TypedFunDefStmt("bar",
	                                        List(TypedParamDclStmt("x",BaseType("Any")),
	                                             TypedParamDclStmt("y",BaseType("Int")),
	                                             TypedParamDclStmt("s",BaseType("String"))),BaseType("AnyVal"),
	                                        TypedVarExpr("f",BaseType("Int")),
	                                        { var scope: Scope = Scope(); 
	                                        	scope.types = scala.collection.mutable.Map[String,Scope]();
	                                        	scope.objects = scala.collection.mutable.Map[String,Type]("x"->BaseType("Any"), "y"->BaseType("Int"), "s"->BaseType("String"));
                                        	    scope;
                                        	}),
	                        TypedFunExpr(TypedVarExpr("bar",FuncType(BaseType("AnyVal"),List(BaseType("Any"), BaseType("Int"), BaseType("String")))),
	                                     List(TypedStringExpr("",BaseType("String")),
	                                          TypedNumExpr(NInt(5),BaseType("Int")),
	                                          TypedStringExpr("",BaseType("String"))),
	                                     BaseType("AnyVal"))),
                       {	var scope: Scope = Scope(); 
                    		scope.types = scala.collection.mutable.Map[String,Scope]();
                        	scope.objects = scala.collection.mutable.Map[String,Type]("bar"->FuncType(BaseType("AnyVal"),List(BaseType("Any"), BaseType("Int"), BaseType("String"))), "f"->BaseType("Int"));
                    	    scope;
                	   },
	                   BaseType("AnyVal")));
    
    checkSubType(FuncType(BaseType("Int"), List(BaseType("Int"))), FuncType(BaseType("Int"), List(BaseType("Int"))), true);
	checkSubType(FuncType(BaseType("Int"), List(BaseType("Boolean"))), FuncType(BaseType("Int"), List(BaseType("Int"))), false);
	checkSubType(FuncType(BaseType("Any"), List(BaseType("Any"))), FuncType(BaseType("AnyVal"), List(BaseType("AnyVal"))), false);
	checkSubType(FuncType(BaseType("Int"), List(BaseType("Any"))), FuncType(BaseType("AnyVal"), List(BaseType("AnyVal"))), true);
	
    checkJoinType(FuncType(BaseType("Int"), List(BaseType("Any"))), FuncType(BaseType("AnyVal"), List(BaseType("Any"))),
        FuncType(BaseType("AnyVal"),List(BaseType("Any"))));
    checkJoinType(FuncType(BaseType("Int"), List(BaseType("Any"))), BaseType("String"), BaseType("AnyRef"));
  }
}