package scalescript

object RunTests extends App {
  LexerTest.run()
  ParserTest.run()
//  TypeVerifierTest.run()
//  CodeGeneratorTest.run()
  println("Done")
}