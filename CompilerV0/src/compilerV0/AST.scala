package compilerV0

sealed trait Numeric
case class Nint(num : Int) extends Numeric
case class Ndouble(Num : Double) extends Numeric

sealed abstract class Stmt {
  def isExpr() : Boolean = {false}
}
case class VarDefStmt(ids : List[String], varType: String, value : Expr) extends Stmt
case class ValDefStmt(ids : List[String], varType: String, value : Expr) extends Stmt
case class FunDefStmt(name : String, args : List[(String, String)], body : Expr) extends Stmt

sealed abstract class Expr extends Stmt {
  override def isExpr() : Boolean = {true}
}
case class BoolExpr(bool : Boolean) extends Expr
case class NumExpr(num: Numeric) extends Expr
case class StringExpr(str : String) extends Expr
case class VarExpr(id: String) extends Expr
case class BlockExpr(body: List[Stmt]) extends Expr
case class BinOpExpr(op: String, left: Expr, right: Expr) extends Expr
case class UnOpExpr(op: String, expr: Expr) extends Expr
case class IfThenExpr(test: Expr, trueClause: Expr) extends Expr
case class IfThenElseExpr(test: Expr, trueClause: Expr, falseClause: Expr) extends Expr
case class WhileExpr(test: Expr, body: Expr) extends Expr
case class PrintExpr(expr: Expr) extends Expr
case class PrintlnExpr(expr: Expr) extends Expr



case class DefWrapper(ids : List[String], varType: String, value : Expr)


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