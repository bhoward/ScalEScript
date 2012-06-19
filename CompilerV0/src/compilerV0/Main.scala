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
    */
    
    //Block statement
    println(Parser("{ 1; 2; 3; 4; }"))
    println(Parser("{ 1; 2; 3; 4 }"))
    println(Parser("{ 1; }"))
    println(Parser("{ 1 }"))
    println(Parser("{ }"))
    println(Parser("{if (5 > 3) 5 else 6; if (5 >= 6) 5 else 6; if (5 <= 5) 5 else 6; if (5 < 5) 5 else 6}"))
    
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
  }
}
