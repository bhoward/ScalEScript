package compilerV0

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
    */
    //Print and println statement
    println(Parser("print(5)"));
    println(Parser("""println("hello world")"""))
    
    println();
    /* CodeGeneration testing */
    
    /*
    println(CodeGeneration (Parser("1   ")))
    println(CodeGeneration (Parser("1 + 5")))
    println(CodeGeneration (Parser("1 * 5")))
    println(CodeGeneration (Parser("1.0 + 3 * 5")))
    */
    
    println("Parsed Expression: " + Parser("if (true || false) 5"))
    println("Code Generated: " + CodeGeneration(Parser("if (true || false) 5")))
    println("Parsed Expression: " + Parser("if (5 > 3) 5 else 6"))
    println("Code Generated: " + CodeGeneration(Parser("if (5 > 3) 5 else 6")))
    println("Parsed Expression: " + Parser("while (true) 5"))
    println("Code Generated: " + CodeGeneration(Parser("while (true) 5")))
    println("Parsed Expression: " + Parser("{ }"))
    println("Code Generated: " + CodeGeneration(Parser("{ }")))
    println("Parsed Expression: " + Parser("{ 1; 2; 3; 4; }"))
    println("Code Generated: " + CodeGeneration(Parser("{ 1; 2; 3; 4; }")))
    
    val scalaSource = """{println("Here we go!"); println( if (5 > 3) 5 else 6 ) }""";
    val jsSource = CodeGeneration(Parser(scalaSource));
    
    println(makeHTML(scalaSource, jsSource));
  }
  
  def makeHTML(scalaSource : String, jsSource : String) : String = {
    val p1 : String = 
      """<html lang="en">
<head>
	<meta http-equiv="content-type" content="text/html; charset=utf-8">
	<title>CompilerV0</title>
    <style type="text/css" media="screen">
    body {
        white-space-collapse: discard;
        float: left;
        background: silver;
    }
    div.ScalaCode {
        background: white;
        color: black;
        font-family: Courier New;
        white-space: pre;
    }
    div.JSCode {
        background: white;
        color: black;
        font-family: Courier New;
        white-space: pre;
    }
    div.JSExec {
        background: white;
        color: black;
        font-family: Courier New;
        white-space: pre;
    }
    </style>
</head>

<body>
<h1>Scala to Javascript compiler test page </h1>

<h2>Scala Source:</h2>
<div class="ScalaCode">""";
    val p2 : String = """
</div>

<h2>Javascript Source:</h2>
<div class="JSCode">""";
    val p3 : String = """
</div>

<h2>Javascript Execution:</h2>
<div class="JSExec"><script language="JavaScript">""";
    val p4 : String = """</script></div>

</body>""";
    
    return p1 + scalaSource + p2 + jsSource + p3 + jsSource + p4;
  }
}
