object Main {
  def main(Args: Array[String]) {
    println("Enter expressions to be evaluated")
    var line = readLine()
    while (line != null) {
      import ExprParser._
      
      expr(line) match {
        case Success(e, rem) =>
          if (rem == "") {
            println("Success: matched " + e)
          } else {
            println("Partial success: matched " + e + " with remainder " + rem)
          }
          println("Result is " + Expression.eval(e))
          
        case Failure(msg) =>
          println("Failure: " + msg)
      }
      
      println()
      line = readLine()
    }
    println("Done")
  }
}