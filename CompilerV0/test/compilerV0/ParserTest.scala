package compilerV0

object ParserTest {
  def checkParse(src: String, expect: Expr) {
    val actual = Parser(src)
    if (actual != expect) {
      System.err.println("Expected: " + expect + "\n  Actual: " + actual);
    }
  }
  
  def run() {
    println("Parser Tests")
    
    //Arithmetic expression
    checkParse("1", NumExpr(NInt(1)))
    checkParse("1   ", NumExpr(NInt(1)))
    checkParse("1 + 5", BinOpExpr("+",NumExpr(NInt(1)),NumExpr(NInt(5))))
    checkParse("2 * 6", BinOpExpr("*",NumExpr(NInt(2)),NumExpr(NInt(6))))
	checkParse("1 +: 4", BinOpExpr("+:",NumExpr(NInt(4)),NumExpr(NInt(1))))
	checkParse("1 + 4", BinOpExpr("+",NumExpr(NInt(1)),NumExpr(NInt(4))))
    checkParse("1 +: 4 +: 3", BinOpExpr("+:",BinOpExpr("+:",NumExpr(NInt(3)),NumExpr(NInt(4))),NumExpr(NInt(1))))
	checkParse("1 + 4 + 3", BinOpExpr("+",BinOpExpr("+",NumExpr(NInt(1)),NumExpr(NInt(4))),NumExpr(NInt(3))))
	checkParse("1 +: 4 +: 3 +: 5", BinOpExpr("+:",BinOpExpr("+:",BinOpExpr("+:",NumExpr(NInt(5)),NumExpr(NInt(3))),NumExpr(NInt(4))),NumExpr(NInt(1))))
	checkParse("1 + 4 + 3 + 5", BinOpExpr("+",BinOpExpr("+",BinOpExpr("+",NumExpr(NInt(1)),NumExpr(NInt(4))),NumExpr(NInt(3))),NumExpr(NInt(5))))
	checkParse("1 +: 4 +: 3 +: 5 +: 6", BinOpExpr("+:",BinOpExpr("+:",BinOpExpr("+:",BinOpExpr("+:",NumExpr(NInt(6)),NumExpr(NInt(5))),NumExpr(NInt(3))),NumExpr(NInt(4))),NumExpr(NInt(1))))
	checkParse("1 + 4 + 3 + 5 + 6", BinOpExpr("+",BinOpExpr("+",BinOpExpr("+",BinOpExpr("+",NumExpr(NInt(1)),NumExpr(NInt(4))),NumExpr(NInt(3))),NumExpr(NInt(5))),NumExpr(NInt(6))))
    checkParse("1 + 3 * 5", BinOpExpr("+",NumExpr(NInt(1)),BinOpExpr("*",NumExpr(NInt(3)),NumExpr(NInt(5)))))
    checkParse("1 * 3 + 5", BinOpExpr("+",BinOpExpr("*",NumExpr(NInt(1)),NumExpr(NInt(3))),NumExpr(NInt(5))))

    //If statement
    checkParse("if (true) 6", IfThenExpr(BoolExpr(true),NumExpr(NInt(6))))
    checkParse("if (5) 5 else 7", IfThenElseExpr(NumExpr(NInt(5)),NumExpr(NInt(5)),NumExpr(NInt(7))))
    checkParse("if (false && true) 5 else 6", IfThenElseExpr(BinOpExpr("&&",BoolExpr(false),BoolExpr(true)),NumExpr(NInt(5)),NumExpr(NInt(6))))
    checkParse("if (true == true) 5 else 6", IfThenElseExpr(BinOpExpr("==",BoolExpr(true),BoolExpr(true)),NumExpr(NInt(5)),NumExpr(NInt(6))))
    checkParse("if (true != false) 5 else 6", IfThenElseExpr(BinOpExpr("!=",BoolExpr(true),BoolExpr(false)),NumExpr(NInt(5)),NumExpr(NInt(6))))
    checkParse("if (5 >= 6) 5 else 6", IfThenElseExpr(BinOpExpr(">=",NumExpr(NInt(5)),NumExpr(NInt(6))),NumExpr(NInt(5)),NumExpr(NInt(6))))
    checkParse("if (5 <= 5) 5 else 6", IfThenElseExpr(BinOpExpr("<=",NumExpr(NInt(5)),NumExpr(NInt(5))),NumExpr(NInt(5)),NumExpr(NInt(6))))
    checkParse("if (5 < 5) 5 else 6", IfThenElseExpr(BinOpExpr("<",NumExpr(NInt(5)),NumExpr(NInt(5))),NumExpr(NInt(5)),NumExpr(NInt(6))))

    //While statement
    checkParse("while (true) 5", WhileExpr(BoolExpr(true),NumExpr(NInt(5))))

    //Block statement
    checkParse("{ 1; 2; 3; 4; }", BlockExpr(List(NumExpr(NInt(1)), NumExpr(NInt(2)), NumExpr(NInt(3)), NumExpr(NInt(4)))))
    checkParse("{ 1; 2; 3; 4 }", BlockExpr(List(NumExpr(NInt(1)), NumExpr(NInt(2)), NumExpr(NInt(3)), NumExpr(NInt(4)))))
    checkParse("{ 1; }", BlockExpr(List(NumExpr(NInt(1)))))
    checkParse("{ 1 }", BlockExpr(List(NumExpr(NInt(1)))))
    checkParse("{ }", BlockExpr(List()))
    checkParse("{if (5 > 3) 5 else 6; if (5 >= 6) 5 else 6; if (5 <= 5) 5 else 6; if (5 < 5) 5 else 6}",
      BlockExpr(List(IfThenElseExpr(BinOpExpr(">",NumExpr(NInt(5)),NumExpr(NInt(3))),NumExpr(NInt(5)),NumExpr(NInt(6))),
                     IfThenElseExpr(BinOpExpr(">=",NumExpr(NInt(5)),NumExpr(NInt(6))),NumExpr(NInt(5)),NumExpr(NInt(6))),
                     IfThenElseExpr(BinOpExpr("<=",NumExpr(NInt(5)),NumExpr(NInt(5))),NumExpr(NInt(5)),NumExpr(NInt(6))),
                     IfThenElseExpr(BinOpExpr("<",NumExpr(NInt(5)),NumExpr(NInt(5))),NumExpr(NInt(5)),NumExpr(NInt(6))))))

    //Print and println statement
    checkParse("print(5)", FunExpr(VarExpr("print"),List(NumExpr(NInt(5)))));
    checkParse("""println("hello world")""", FunExpr(VarExpr("println"),List(StringExpr("hello world"))))

    //Vals and Vars
    checkParse("{var t : Int = 5; }", BlockExpr(List(VarDefStmt(List("t"),BaseType("Int"),NumExpr(NInt(5))))))
    checkParse("{var t2, t1 : Double = 5.0; t2 + t1;}", BlockExpr(List(VarDefStmt(List("t2", "t1"),BaseType("Double"),NumExpr(NDouble(5.0))), BinOpExpr("+",VarExpr("t2"),VarExpr("t1")))))
    checkParse("{var t : Int = 5; t}", BlockExpr(List(VarDefStmt(List("t"),BaseType("Int"),NumExpr(NInt(5))), VarExpr("t"))))

    //Functions
    checkParse("foo(5, 6)", FunExpr(VarExpr("foo"),List(NumExpr(NInt(5)), NumExpr(NInt(6)))))
    checkParse("""{def boots (x : Int, y : Int, z : String): Int = x + y; boots(5, 6, "shoes")}""",
      BlockExpr(List(FunDefStmt("boots",List(ParamDclStmt("x",BaseType("Int")), ParamDclStmt("y",BaseType("Int")), ParamDclStmt("z",BaseType("String"))),BaseType("Int"),BinOpExpr("+",VarExpr("x"),VarExpr("y"))),
                     FunExpr(VarExpr("boots"),List(NumExpr(NInt(5)), NumExpr(NInt(6)), StringExpr("shoes"))))))
    checkParse("""{var f : (Int)=>Int = (x:Int) => x+1; f(5)}""", BlockExpr(List(VarDefStmt(List("f"),FuncType(BaseType("Int"),List(BaseType("Int"))),AnonFuncExpr(List(ParamDclStmt("x",BaseType("Int"))),BinOpExpr("+",VarExpr("x"),NumExpr(NInt(1))))), 
    																			 FunExpr(VarExpr("f"),List(NumExpr(NInt(5)))))))
    																			 
    // Assignment
    checkParse("""{var x: Int = 0; x = 1}""",
      BlockExpr(List(VarDefStmt(List("x"),BaseType("Int"),NumExpr(NInt(0))), AssignExpr(VarExpr("x"),NumExpr(NInt(1))))))
  }
}