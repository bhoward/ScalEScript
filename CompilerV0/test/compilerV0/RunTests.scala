package compilerV0

object RunTests extends App {
  ParserTest.run()
  TypeVerifierTest.run()
  CodeGeneratorTest.run()
}