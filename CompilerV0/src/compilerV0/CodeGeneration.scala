package compilerV0

object CodeGeneration {
  def generate (ast : Expr):String = ast match {
    case NumExpr(value) => value.toString
    case BoolExpr(value) => value.toString
    case BinOpExpr(op, l, r) =>  "(" + generate(l) + " " + op + " " + generate(r) + ")"
    case IfThenStmt(predicate, expr) => "ifThen( " + generate(predicate) + ", " + generate(expr) + " )"
    case IfThenElseStmt(predicate, truevalue, falsevalue) => 
      "ifThenElse( " + generate(predicate) + ", " + generate(truevalue) + ", " + generate(falsevalue) + " )"
    case _ => "failure"
  }
  def apply(source: Expr): String = generate (source)
  
  
 
}