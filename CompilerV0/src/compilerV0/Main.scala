package compilerV0

import java.io.File;
import java.io.FileWriter;

object Main {
	def main(args: Array[String]) {
	    /* Entire Compiler testing */
	    testCompiler("Blocks", """println({{5; 4; ; ; ; 6;}; {}})""");
	    testCompiler("Blocks2", """println({{}; {var x : Int = 5; ; ; ;}})""");
	    testCompiler("simpleExpr", """println( 1 + 3 * 5 )""");
	    testCompiler("ifThen", """{println(if (true) 6); println(if (false) 6)}""");
	    testCompiler("ifThenElseComp", """println(if (5 >= 6) 5 else 6)""");
	    testCompiler("ifThenElse", """{println(if (true) 6 else 5); println(if (false) 6 else 5)}""");
	    testCompiler("vars", """println({var t0, t1 : Double = 5.0; t0 + t1;})""");
	    testCompiler("functions", """{def bar (x : Int, y : Int, z : String): Int = bar(5, 6, ""); println(5);}""");
	    testCompiler("functions2", """{def bar (): Int = 5; println(bar());}""")
	    testCompiler("recurfun1", """println({def fact(x: Int):Int = if (x == 0) 1 else x * fact(x-1);fact(5);})""");
	    testCompiler("mutualRecur1", """{def even(n: Int):Boolean = if (n == 0) true else odd(n-1); def odd(n: Int):Boolean = if (n == 0) false else even(n-1); println(even(8)); println(even(51));}""");
	    testCompiler("anonFunc", """println({var f : (Int)=>Int = (x:Int) => x+1; f(5)})""");
	}
  
	def testCompiler(testName : String, scalaSource : String) {
		try {
			val ast = Parser(scalaSource);
			val typedAst = TypeVerifier(ast);
			println(ast);
			println(typedAst)
			val jsSource = CodeGenerator(ast);
			writeToFile("""src/HTML/"""+testName+".html", makeHTML(scalaSource, ast.toString(), jsSource));
			println(testName+".html was successfully created.");
		} catch {
			case e: Exception => {println("Error while compiling "+testName+". "+e);}
		}
		println();
    }
  
	def writeToFile(fileName : String, contents : String){
		val fw = new FileWriter(fileName); 
	    fw.write(contents); 
	    fw.close();
    }
  
	def makeHTML(scalaSource : String, ast : String, jsSource : String) : String = {
		val p1 : String = """<html lang="en"><head><meta http-equiv="content-type" content="text/html; charset=utf-8"><title>CompilerV0</title><link rel="stylesheet" type="text/css" href="CompilerV0.css" /><script type="text/javascript" src="CompilerV0.js" /></script></head><body><h1>Scala to Javascript compiler test page </h1><h2>Scala Source:</h2><div id="ScalaCode" class="code">""";
	    val p2 : String = """</div><h2>Abstract Syntax Tree:</h2><div id="AST" class="code">""";
	    val p3 : String = """</div><h2>Javascript Source:</h2><div id="JSCode" class="code">""";
	    val p4 : String = """</div><h2>Javascript Execution:</h2><div id="JSExec" class="code"><script language="Javascript">""";
	    val p5 : String = """</script></div></body>""";
	    
	    return p1 + scalaSource + p2 + ast + p3 + jsSource + p4 + jsSource + p5;
  	}
}
