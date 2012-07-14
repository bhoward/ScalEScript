package compilerV0

object RunTests extends App {
  ParserTest.run()
  CodeGeneratorTest.run()
  TypeVerifierTest.run()
}