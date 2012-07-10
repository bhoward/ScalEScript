package compilerV0

//Non-Typed AST
sealed trait Stmt {
	def isExpr() : Boolean = {false}
}
case class VarDefStmt(ids : List[String], varType: String, value : Expr) extends Stmt
case class ValDefStmt(ids : List[String], varType: String, value : Expr) extends Stmt
case class FunDefStmt(name : String, params : List[VarDclStmt], retType : String, body : Expr) extends Stmt
case class VarDclStmt(ids : List[String], varType: String) extends Stmt

sealed trait Expr extends Stmt {
	override def isExpr() : Boolean = {true}
}
case class BoolExpr(bool : Boolean) extends Expr
case class NumExpr(num: Numeric) extends Expr
case class StringExpr(str : String) extends Expr
case class VarExpr(id: String) extends Expr
case class FunExpr(id: String, args : List[Expr]) extends Expr
case class BlockExpr(body: List[Stmt]) extends Expr
case class BinOpExpr(op: String, left: Expr, right: Expr) extends Expr
case class UnOpExpr(op: String, expr: Expr) extends Expr
case class IfThenExpr(test: Expr, trueClause: Expr) extends Expr
case class IfThenElseExpr(test: Expr, trueClause: Expr, falseClause: Expr) extends Expr
case class WhileExpr(test: Expr, body: Expr) extends Expr

//Typed AST
sealed trait TypedStmt {
	def isExpr() : Boolean = {false}
	def evalType() : Type;
}
case class TypedVarDefStmt(ids : List[String], varType: String, value : Expr, evalType : Type) extends TypedStmt
case class TypedValDefStmt(ids : List[String], varType: String, value : Expr, evalType : Type) extends TypedStmt
case class TypedFunDefStmt(name : String, params : List[VarDclStmt], retType : String, body : Expr, evalType : Type) extends TypedStmt
case class TypedVarDclStmt(ids : List[String], varType: String, evalType : Type) extends TypedStmt

sealed trait TypedExpr extends TypedStmt {
	override def isExpr() : Boolean = {true}
}
case class TypedBoolExpr(bool : Boolean, evalType : Type) extends TypedExpr
case class TypedNumExpr(num: Numeric, evalType : Type) extends TypedExpr
case class TypedStringExpr(str : String, evalType : Type) extends TypedExpr
case class TypedVarExpr(id: String, evalType : Type) extends TypedExpr
case class TypedFunExpr(id: String, args : List[Expr], evalType : Type) extends TypedExpr
case class TypedBlockExpr(body: List[Stmt], evalType : Type) extends TypedExpr
case class TypedBinOpExpr(op: String, left: Expr, right: Expr, evalType : Type) extends TypedExpr
case class TypedUnOpExpr(op: String, expr: Expr, evalType : Type) extends TypedExpr
case class TypedIfThenExpr(test: Expr, trueClause: Expr, evalType : Type) extends TypedExpr
case class TypedIfThenElseExpr(test: Expr, trueClause: Expr, falseClause: Expr, evalType : Type) extends TypedExpr
case class TypedWhileExpr(test: Expr, body: Expr, evalType : Type) extends TypedExpr

//Classes used for compiling
sealed trait Numeric
case class NInt(num : Int) extends Numeric
case class NDouble(Num : Double) extends Numeric

case class DefWrapper(ids : List[String], varType: String, value : Expr)
case class FunWrapper(name : String, args : List[VarDclStmt], retType : String,  body : Expr)

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

sealed trait Type {
	def isFunc() : Boolean;
	def getType() : String;
	def getArgTypes() : List[Type];
}
case class ObjType(varType : String) extends Type {
	def isFunc() : Boolean = false;
	def getType() : String = varType;
	def getArgTypes() : List[Type] = Nil;
}
case class FuncType(retType : String, argTypes : List[ObjType]) extends Type {
	def isFunc() : Boolean = true;
	def getType() : String = retType;
	def getArgTypes() : List[Type] = argTypes;
}