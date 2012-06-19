package compilerV0

object CodeGeneration {
  def generate (ast : Expr):String = ast match {
    case NumExpr(value) => value.toString
    case BoolExpr(value) => value.toString
    case BinOpExpr(op, l, r) =>  "(" + generate(l) + " " + op + " " + generate(r) + ")"
    case IfThenStmt(predicate, expr) => "ifThen( " + generate(predicate) + ", " + generate(expr) + " )"
    case IfThenElseStmt(predicate, truevalue, falsevalue) => 
      "ifThenElse( " + generate(predicate) + ", " + generate(truevalue) + ", " + generate(falsevalue) + " )"
    case WhileStmt(predicate, body) => "whileLoop( " + generate(predicate) + ", " + generate(body) + " )"
    case BlockExpr(listofstatements) => "(function() = { \n" + blockProcess(listofstatements) + " })()"
    case _ => "failure"
  }
  def blockProcess(loe : List[Expr]):String = loe match {
    case List() => ""
    case x::xs => generate(x) + "; \n" + blockProcess(xs)
  }
  def apply(source: Expr): String = generate (source)
  
  
 
}