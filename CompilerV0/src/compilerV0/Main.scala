package compilerV0

import java.io.File;
import java.io.FileWriter;

object Main {
	def main(args: Array[String]) {
		/* Parser testing */
		/*
	    //Arithmetic expression
	    println(Parser("1"))
	    println(Parser("1   "))
	    println(Parser("1 + 5"))
	    println(Parser("2 * 6"))
	    println(Parser("1 +: 4 +: 3"))
	    println(Parser("1 + 3 * 5"))
	    println(Parser("1 * 3 + 5"))
	    
	    //If statement
	    println(Parser("if (true) 6"))
	    println(Parser("if (false) 6 else 7"))    
	    println(Parser("if (false && true) 5 else 6"))
	    println(Parser("if (true == true) 5 else 6"))
	    println(Parser("if (true != false) 5 else 6"))
	    println(Parser("if (5 >= 6) 5 else 6"))
	    println(Parser("if (5 <= 5) 5 else 6"))
	    println(Parser("if (5 < 5) 5 else 6"))
	    
	    //While statement
	    println(Parser("while (true) 5"))
	    
	    //Block statement
	    println(Parser("{ 1; 2; 3; 4; }"))
	    println(Parser("{ 1; 2; 3; 4 }"))
	    println(Parser("{ 1; }"))
	    println(Parser("{ 1 }"))
	    println(Parser("{ }"))
	    println(Parser("{if (5 > 3) 5 else 6; if (5 >= 6) 5 else 6; if (5 <= 5) 5 else 6; if (5 < 5) 5 else 6}"))
	    println(Parser("{ }"))
	    
	    //Print and println statement
	    println(Parser("print(5)"));
	    println(Parser("""println("hello world")"""))
	    
	    //Vals and Vars
	    println(Parser("{var t : Int = 5; }"))
	    println(Parser("{val t0, t1 : Int = 5; }"))
	    println(Parser("{var t : Int = 5; t}"))  
	    
		//Functions
		println(Parser("foo(5, 6)"))
	    println(Parser("""{def boots (x : Int, y : Int, z : String): Int = x + y; boots(5, 6, "shoes")}"""))
		*/
		println();
	    
	    /* CodeGenerator testing */
	    /*
	    println(CodeGenerator (Parser("1   ")))
	    println(CodeGenerator (Parser("1 + 5")))
	    println(CodeGenerator (Parser("1 * 5")))
	    println(CodeGenerator (Parser("1.0 + 3 * 5")))
	    
	    println("Parsed Expression: " + Parser("if (true || false) 5"))
	    println("Code Generated: " + CodeGenerator(Parser("if (true || false) 5")))
	    println("Parsed Expression: " + Parser("if (5 > 3) 5 else 6"))
	    println("Code Generated: " + CodeGenerator(Parser("if (5 > 3) 5 else 6")))
	    
	    println("Parsed Expression: " + Parser("while (true) 5"))
	    println("Code Generated: " + CodeGenerator(Parser("while (true) 5")))
	    */
	    println("Parsed Expression: " + Parser("{ }"))
	    println("Code Generated: " + CodeGenerator(Parser("{ }")))
	    println("Parsed Expression: " + Parser("{ 1; 2; 3; 4; }"))
	    println("Code Generated: " + CodeGenerator(Parser("{ 1; 2; 3; 4; }")))
	    /*
	    println("Parsed Expression: " + Parser(""""hey""""))
	    println("Code Generated: " + CodeGenerator(Parser(""""hey"""")))
	    println("Parsed Expression: " + Parser("print(5)"));
	    println("Code Generated: " + CodeGenerator(Parser("print(5)")))
	    println("Parsed Expression: " + Parser("""println("hello world")"""))
	    println("Code Generated: " + CodeGenerator(Parser("""println("hello world")""")))
	    */
	    println("Parsed Expression: " + Parser("{var t : Int = 5; }"))
	    println("Code Generated: " + CodeGenerator(Parser("{var t : Int = 5; }")))
	    /*
	    println("Parsed Expression: " + Parser("{val t0, t1 : Int = 5; }"))
	    println("Code Generated: " + CodeGenerator(Parser("{val t0, t1 : Int = 5; }")))
	    println("Parsed Expression: " + Parser("{val t0, t1 : Int = 5;}"))
	    println("Code Generated: " + CodeGenerator(Parser("{val t0, t1 : Int = 5;}")))
	    println("Parsed Expression: " + Parser("{val t0, t1 : Int = 5; t0 + t1;}"))
	    println("Code Generated: " + CodeGenerator(Parser("{val t0, t1 : Int = 5; t0 + t1;}")))
	   
	    println("Parsed Expression: " + Parser("{def foo (x : Int, y : Int, z : String): Int = x + y;}"))
	    println("Code Generated: " + CodeGenerator(Parser("{def foo (x : Int, y : Int, z : String): Int = x + y;}")))
	    
	    println("Parsed Expression: " + Parser("{def bar (x : Int, y : Int, z : String): Unit = x + y;}"))
	    println("Code Generated: " + CodeGenerator(Parser("{def bar (x : Int, y : Int, z : String): Unit = x + y;}")))
	    
		println("Parsed Expression: " + Parser("""{def bar (x : Int, y : Int, z : String): Int = x + y; println(bar(5, 6, ""));}"""))
	    println("Code Generated: " + CodeGenerator(Parser("""{def bar (x : Int, y : Int, z : String): Int = x + y; println(bar(5, 6, ""));}""")))
	   	*/
	    println();
	    
	    /* TypeVerifier testing */
	    /*
		println(TypeVerifier(Parser("""println({val t0, t1 : Double = 5.5; {val t1 : Int = 6; val t1 : Int = 7; println(t1)}; t0 + t1;})""")));
	    println(TypeVerifier(Parser("""{var x, y, z : Int = if (true) 5.0 else 6.0;}""")));
	    */
	    //println(TypeVerifier(Parser("""{var f : Int = 5; def bar (x : Any, y : Int, s : String): AnyVal = f; bar("", 5, "");}""")));
	    //println();
	    
	    /* Entire Compiler testing */
	    testCompiler("simpleExpr", """println( 1 + 3 * 5 )""");
	    testCompiler("ifThen", """{println(if (true) 6); println(if (false) 6)}""");
	    testCompiler("ifThenElseComp", """println(if (5 >= 6) 5 else 6)""");
	    testCompiler("ifThenElse", """{println(if (true) 6 else 5); println(if (false) 6 else 5)}""");
	    testCompiler("vars", """println({var t0, t1 : Double = 5.0; t0 + t1;})""");
	    testCompiler("functions", """{def bar (x : Int, y : Int, z : String): Int = x + y; println(bar(5, 6, ""));}""");
	    testCompiler("functions2", """{def bar (): Int = 5; println(bar());}""")
	}
  
	def testCompiler(testName : String, scalaSource : String) {
		val ast = Parser(scalaSource);
	    if (!TypeVerifier(ast)){
	    	println("Scala source did not pass the typeVerifier");
	    	return;
	    }
	    val jsSource = CodeGenerator(ast);
	    writeToFile("""src/HTML/"""+testName+".html", makeHTML(scalaSource, ast.toString(), jsSource));
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
