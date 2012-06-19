package compilerV0

sealed trait Numeric
case class Nint(num : Int) extends Numeric
case class Ndouble(Num : Double) extends Numeric

sealed trait Expr
case class StringExpr(str : String) extends Expr
case class BoolExpr(bool : Boolean) extends Expr
case class BlockExpr(body: List[Expr]) extends Expr
case class BinOpExpr(op: String, left: Expr, right: Expr) extends Expr
case class UnOpExpr(op: String, expr: Expr) extends Expr
case class NumExpr(num: Numeric) extends Expr
case class IfThenElseStmt(test: Expr, trueClause: Expr, falseClause: Expr) extends Expr
case class IfThenStmt(test: Expr, trueClause: Expr) extends Expr
case class WhileStmt(test: Expr, body: Expr) extends Expr
case class PrintExpr(expr: Expr) extends Expr
case class PrintlnExpr(expr: Expr) extends Expr

sealed trait OpPair {
  def isLeft() : Boolean;
  def getOp() : String;
  def getExpr() : Expr;
}
case class LeftOpPair(op: String, expr: Expr) extends OpPair {
  def isLeft() : Boolean = true;
  def getOp() : String = op;
  def getExpr() : Expr = expr;
}
case class RightOpPair(op: String, expr: Expr) extends OpPair {
  def isLeft(): Boolean = false;
  def getOp() : String = op;
  def getExpr() : Expr = expr;
}