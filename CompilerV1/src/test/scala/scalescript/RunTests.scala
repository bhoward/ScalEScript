package scalescript

object RunTests extends App {
  ParserTest.run()
  TypeVerifierTest.run()
  CodeGeneratorTest.run()
}