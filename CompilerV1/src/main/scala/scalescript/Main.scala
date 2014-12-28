package scalescript

import java.io.FileWriter

/**
 * The Main object for running the compiler
 *
 * Since specific tests were stripped out and moved in to the test objects,
 * the main purpose of Main is to generate HTML pages for testing JavaScript
 *
 * @author Trevor Griswold
 * @author Mike Stees
 * @author Brian Howard
 */
object Main {
  val destDir = "HTML"
  val baseCSS = "BASE.css"
  val baseJS = "BASE.js"
  
  def main(args: Array[String]) {
    val dir = new java.io.File(destDir)
    if (!dir.exists) {
      dir.mkdir()
    }
    createBase()
    testCompiler("Factorial",
      """{ def factorial(n: Int):Int = if (n == 0) 1 else n * factorial(n-1); println(factorial(5)); }""");
    testCompiler("Arithmetic",
      """4.0+15/4""");
    testCompiler("Blocks",
      """println({{5; 4; ; ; ; 6;}; {}})""");
    testCompilerThrows("""println({{}; {var x : Int = 5; ; ; ;}})""", "The last line in the block is a Stmt, expected an Expr");
    testCompiler("simpleExpr",
      """println( 1 + 3 * 5 )""");
    testCompiler("ifThen",
      """{println(if (true) 6); println(if (false) 6)}""");
    testCompiler("ifThenElseComp",
      """println(if (5 >= 6) 5 else 6)""");
    testCompiler("ifThenElse",
      """{println(if (true) 6 else 5); println(if (false) 6 else 5)}""");
    testCompiler("vars",
      """println({var t0, t1 : Double = 5.0; t0 + t1;})""");
    testCompiler("functions",
      """{def bar (x : Int, y : Int, z : String): Int = bar(5, 6, ""); println(5);}""");
    testCompiler("functions2",
      """{def bar (): Int = 5; println(bar());}""")
    testCompiler("recurfun1",
      """println({def fact(x: Int):Int = if (x == 0) 1 else x * fact(x-1);fact(5);})""");
    testCompiler("mutualRecur1",
      """{def even(n: Int):Boolean = if (n == 0) true else odd(n-1); def odd(n: Int):Boolean = if (n == 0) false else even(n-1); println(even(8)); println(even(51));}""");
    testCompiler("anonFunc",
      """println({var f : (Int)=>Int = (x:Int) => x+1; f(5)})""");
    testCompiler("fibo1",
      """{ // Very slow...
	      |  def fibo(n: Int): Int = if (n < 2) n else fibo(n-1) + fibo(n-2);
	      |  println(fibo(35));
        |}""".stripMargin);
    testCompiler("fibo2",
      """{ // This is much faster...
	      |  def fibo(n: Int): Int = {
	      |    def aux(n: Int, a: Int, b: Int): Int =
	      |      if (n == 0) a else aux(n-1, b, a+b);
	      |    aux(n, 0, 1)
	      |  };
	      |  println(fibo(35));
	      |}""".stripMargin);
    testCompiler("while",
      """{var n: Int = 0; while (n < 10) {n = n + 1; print(n);}; println("Done");}""");
    testCompiler("while2",
      """{var n: Int = 0; while (n < 10) {n = n + 1; print(n + " ");}; println("Done");}""");
    testCompiler("do",
      """{var x: Int = 0; do {x = x + 1; println(x);} while (x < 10)}""")
    testCompiler("strings",
      "{println(\"\"\"Hello\n" +
        "This is a test:\\tone\\ttwo\\tthree\"\"\"); /* should not be tabbed */\n" +
        "println(\"This is a test:\\tone\\ttwo\\tthree\") /* should be tabbed */}");
    testCompiler("Curry",
      """println({def doSomeMath(x:Int)(y:Int)(z:Int) : Int = x*y+z; doSomeMath(2)(3)(4)})""");
  }

  /** Generates an HTML page with the specified testName consisting of each stage of the compilation process of the specified scalaSource */
  def testCompiler(testName: String, scalaSource: String) {
    try {
      val ast = Parser(scalaSource);
      val annotatedAst = ASTConverter(ast)
      val typedAst = TypeVerifier(annotatedAst);
      val jsSource = CodeGenerator(typedAst, "top");
      writeToFile(destDir + "/" + testName + ".html", makeHTML(scalaSource, ast.toString(), annotatedAst.toString(), typedAst.toString(), jsSource));
      println(testName + ".html was successfully created.");
    } catch {
      case e: Exception => { System.err.println("Error while compiling " + testName + ". " + e); }
    }
  }

  def testCompilerThrows(scalaSource: String, expect: String) {
    try {
      CodeGenerator(TypeVerifier(ASTConverter(Parser(scalaSource))), "top")
      System.err.println("Expected exception \"" + expect + "\" not thrown")
    } catch {
      case e: Exception =>
        if (e.getMessage != expect) {
          System.err.println("Exception:\n" + e + "\ndoes not match \"" + expect + "\"")
        }
    }
  }

  /** Writes the specified contents to the specified fileName */
  def writeToFile(fileName: String, contents: String) {
    val fw = new FileWriter(fileName);
    fw.write(contents);
    fw.close();
  }
  
  def createBase() {
    writeToFile(destDir + "/" + baseCSS,
      """body {
        |  white-space-collapse: discard;
        |  float: left;
        |  background: silver;
        |}
        |div.code{
        |  background: white;
        |  color: black;
        |  font-family: Courier New;
        |  white-space: pre-wrap;
        |}
        |""".stripMargin)
    writeToFile(destDir + "/" + baseJS,
      """function ifThen(test, trueClause) {
        |  if (test()) {
        |    return trueClause();
        |  }
        |}
        |function ifThenElse(test, trueClause, falseClause) {
        |  if (test()) {
        |    return trueClause();
        |  } else {
        |    return falseClause();
        |  }
        |}
        |function whileLoop(test, body) {
        |  while (test()) {
        |    body();
        |  }
        |}
        |function doLoop(test, body) {
        |  do {
        |    body();
        |  } while (test())
        |}
        |
        |function div(left, right) {
        |  return ~~(left / right);
        |}
        |
        |function println(stmt) {
        |    document.writeln(stmt);
        |}
        |function print(stmt) {
        |    document.write(stmt);
        |}
        |""".stripMargin)
  }

  /** Builds a String containing the contents of an HTML page to be generated for a specific test */
  def makeHTML(scalaSource: String, ast: String, annotAst: String, typedAst: String, jsSource: String): String = {
    s"""<html lang="en">
       |  <head>
       |    <meta http-equiv="content-type" content="text/html; charset=utf-8">
       |    <title>CompilerV0</title>
       |    <link rel="stylesheet" type="text/css" href="$baseCSS" />
       |    <script type="text/javascript" src="$baseJS" /></script>
       |  </head>
       |  <body>
       |    <h1>Scala to Javascript compiler test page </h1>
       |    <h2>Scala Source:</h2>
       |      <div id="ScalaCode" class="code">$scalaSource</div>
       |    <h2>Abstract Syntax Tree:</h2>
       |      <div id="AST" class="code">$ast</div>
       |    <h2>Annotated Abstract Syntax Tree:</h2>
       |      <div id="AnnotAST" class="code">$annotAst</div>
       |    <h2>Typed Abstract Syntax Tree:</h2>
       |      <div id="TypedAST" class="code">$typedAst</div>
       |    <h2>Javascript Source:</h2>
       |      <div id="JSCode" class="code">$jsSource</div>
       |    <h2>Javascript Execution:</h2>
       |      <div id="JSExec" class="code"><script language="Javascript">$jsSource</script></div>
       |  </body>""".stripMargin
  }
}
