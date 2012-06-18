package compilerV0

object Main {
  def main(args: Array[String]) {
    println(Parser("1"))
    println(Parser("1   "))
    println(Parser("1 + 5"))
    println(Parser("2 * 6"))
    println(Parser("if (true) 6"))
    println(Parser("if (false) 6 else 7"))
    println(Parser("while (true) 6"))
    
    println(Parser("if (true || false) 5"))
    println(Parser("if (false && true) 5 else 6"))
    println(Parser("if (true == true) 5 else 6"))
    println(Parser("if (true != false) 5 else 6"))
    println(Parser("if (5 > 3) 5 else 6"))
    println(Parser("if (5 >= 6) 5 else 6"))
    println(Parser("if (5 <= 5) 5 else 6"))
    println(Parser("if (5 < 5) 5 else 6"))
    println(Parser("while (true) 5"))
    
    println(Parser("1 +: 4 +: 3"))
    println(Parser("1 + 3 * 5"))
    println(Parser("1 * 3 + 5"))
    
    println(Parser("{ 1; 2; 3; 4; }"))
    println(Parser("{ 1; 2; 3; 4 }"))
    println(Parser("{ 1; }"))
    println(Parser("{ 1 }"))
    println(Parser("{ }"))
    println(Parser("{if (5 > 3) 5 else 6; if (5 >= 6) 5 else 6; if (5 <= 5) 5 else 6; if (5 < 5) 5 else 6}"))
  }
}