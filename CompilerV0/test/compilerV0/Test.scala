package compilerV0

trait Test {
  def run(): Unit
  
  def shouldThrow(expect: String)(body: => Unit) {
    try {
      body
      System.err.println("Expected exception \"" + expect + "\" not thrown")
    } catch {
      case e: Exception =>
        if (e.getMessage != expect) {
          System.err.println("Exception:\n" + e + "\ndoes not match \"" + expect + "\"")
        }
    }
  }
}