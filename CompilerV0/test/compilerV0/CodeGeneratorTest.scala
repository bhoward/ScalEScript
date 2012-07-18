package compilerV0

object CodeGeneratorTest {
  def checkCode(src: String, expect: String) {
    try {
    	val ast = TypeVerifier(Parser(src))
	    val code = CodeGenerator(ast)
	    if (expect.replaceAll("\r", "") != code.replaceAll("\r", "")) {
	    	System.err.println("Expected: " + expect + "\n  Actual: " + code + "\n  AST: " + ast);
	    }
    } catch {
		case e: Exception => {System.err.println("Error while code generating "+src+".\n"+e+"\n");}
	}
  }
  
  def run() {
    println("Code Generator Tests")
    
    checkCode("1   ", "1")
    checkCode("1 + 5", "(1 + 5)")
    checkCode("1 * 5", "(1 * 5)")
    checkCode("1.0 + 3 * 5", "(1.0 + (3 * 5))")

    checkCode("if (true || false) 5",
      """ifThen( (function() {
        | return (true || false)}), (function() {
        | return 5}))""".stripMargin
    )
    checkCode("if (5 > 3) 5 else 6",
      """ifThenElse( (function() {
        | return (5 > 3)}), (function() {
        | return 5}), (function() {
        | return 6}) )""".stripMargin
    )

    checkCode("while (true) 5",
      """whileLoop( (function() {
        | return true}), (function() {
        | return 5}) )""".stripMargin
    )

    checkCode("{ ; ; ; ; }",
      """(function() { 
        |return ; })()""".stripMargin
    )
    checkCode("{ 1; 2; 3; 4; ; ;}",
      """(function() { 
        |1; 
        |2; 
        |3; 
        |return 4 ; })()""".stripMargin
    )

    checkCode(""""hey"""", """"hey"""")
    checkCode("print(5)", """print(5)""")
    checkCode("""println("hello world")""", """println("hello world")""")
    
    checkCode("println(\"\"\"This\nis\na\nmultiline\nstring\n\"\"\")", "println(\"This\\nis\\na\\nmultiline\\nstring\\n\")")

    // TODO: these throw an exception
    // checkCode("{var t : Int = 5;}", "")

    // checkCode("{val t0, t1 : Int = 5; }", "")
    // checkCode("{val t0, t1 : Int = 5;}", "")
    checkCode("{val t0, t1 : Int = 5; t0 + t1;}",
      """(function() { 
        |var t0 , t1 ; 
        | t0 = t1 = 5; 
        |return (t0 + t1) ; })()""".stripMargin
    )

    checkCode("{def foo (x : Int, y : Int, z : String): Int = x + y; foo(5)}",
      """(function() { 
        |var foo = function ( x, y, z )
        | {return (x + y); 
        | }; 
        |return foo(5) ; })()""".stripMargin
    )

    // checkCode("{def bar (x : Int, y : Int, z : String): Unit = x + y;}", "")

    checkCode("""{def bar (x : Int, y : Int, z : String): Int = x + y; println(bar(5, 6, ""));}""",
      """(function() { 
        |var bar = function ( x, y, z )
        | {return (x + y); 
        | }; 
        |return println(bar(5, 6, "")) ; })()""".stripMargin
    )

    checkCode("""((x:Int) => (x+1))(5)""", "(function (x ) { return (x + 1) }) (5)")
    checkCode("""{var f : function = (x:Int) => (x+1); f(5)}""", // TODO replace "function" with an actual function type "Int => Int"
      """(function() { 
        |var f ; 
        | f = (function (x ) { return (x + 1) }) ; 
        |return f(5) ; })()""".stripMargin
    )
    
    checkCode("""{var x: Int = 0; x = 1}""",
      """(function() { 
        |var x ; 
        | x = 0; 
        |return (x = 1) ; })()""".stripMargin)
  }
}