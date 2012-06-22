package compilerV0

object CodeGeneration {
  
  def generate (ast : Stmt):String = ast match {
    case NumExpr(value) => value.toString
    case BoolExpr(value) => value.toString
    case BinOpExpr(op, l, r) =>  "(" + generate(l) + " " + op + " " + generate(r) + ")"
    case ValDefStmt(listofvaldecs, valtype, expr) => "var " + varProcess(listofvaldecs, expr) + 
                                                     " " + varProcessAux(listofvaldecs, expr)
    case VarDefStmt(listofvardecs, valtype, expr) => "var " + varProcess(listofvardecs, expr) + 
                                                     " " + varProcessAux(listofvardecs, expr)
    case IfThenExpr(predicate, expr) => "ifThen( " + 
    "(function() { \n" + "return " + generate(predicate)  + " })()" + ", " + 
    "(function() { \n" + "return " + generate(predicate)  + " })()" + " )"
    case IfThenElseExpr(predicate, truevalue, falsevalue) => 
      "ifThenElse( " + "(function() { \n" + generate(predicate)  + " })()" + ", " + "(function() { \n" + 
      "return " + generate(truevalue)  + " })()" + ", " +"(function() { \n" + 
      "return " + generate(falsevalue)  + " })()" + " )"
    case WhileExpr(predicate, body) => "whileLoop( " + "(function() { \n" + "return " + generate(predicate)  + " })()" + ", " + 
                                       "(function() { \n" + "return " + generate(body)  + " })()" + " )"
    case BlockExpr(listofstatements) => "(function() { \n" + blockProcess(listofstatements) + " })()"
    case StringExpr(value) => value
    case PrintExpr(msg) => "document.write(" + generate(msg) + ")"
    case PrintlnExpr(msg) => "document.writeln(" + generate(msg) + ")"
    case _ => "failure"
  }
  def blockProcess(loe : List[Stmt]):String = loe match {
    case List() => ""
    case List(x) => if (x.isExpr()) "return " + generate(x) + " ;" else "return ;"
    case x::xs => generate(x) + "; \n" + blockProcess(xs)
  }
  def varProcess(los : List[String], expr : Expr):String = los match{
    case List(x)=> x + " ; \n"
    case x::xs => x + " , " + varProcess(xs, expr)
  } 
  def varProcessAux(los : List[String], expr : Expr):String = los match{
    case List() => generate(expr)
    case x::xs => x + " = " + varProcessAux(xs, expr)
  }
  def apply(source: Expr): String = generate (source)
  
  
 
}