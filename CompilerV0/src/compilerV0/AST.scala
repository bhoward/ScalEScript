package compilerV0

//Non-Typed AST
sealed trait Stmt {
	def isExpr() : Boolean = {false}
}
case class ValDefStmt(ids : List[String], valType: Type, value : Expr, valTypeflag : String) extends Stmt
case class FunDefStmt(name : String, paramClauses : List[List[ParamDclStmt]], retType : Type, body : Expr) extends Stmt
case class ParamDclStmt(id : String, varType: Type) extends Stmt
case class ClassDefStmt(caseFlag: Boolean,
                        className : String,
                        paramClauses: List[List[ParamDclStmt]],
                        extendClass: ClassInstance,
                        withIds: List[String],
                        body: List[Stmt]) extends Stmt
                        
sealed trait Expr extends Stmt {
	override def isExpr() : Boolean = {true}
}
case class BoolExpr(bool : Boolean) extends Expr
case class NumExpr(num: Numeric) extends Expr
case class StringExpr(str : String) extends Expr
case class CharExpr(ch : Char) extends Expr
case class VarExpr(id: String) extends Expr
case class FunExpr(id: Expr, args : List[Expr]) extends Expr
case class BlockExpr(body: List[Stmt]) extends Expr
case class BinOpExpr(op: String, left: Expr, right: Expr) extends Expr
case class UnOpExpr(op: String, expr: Expr) extends Expr
case class IfThenExpr(test: Expr, trueClause: Expr) extends Expr
case class IfThenElseExpr(test: Expr, trueClause: Expr, falseClause: Expr) extends Expr
case class WhileExpr(test: Expr, body: Expr) extends Expr
case class AnonFuncExpr(args: List[ParamDclStmt], body: Expr) extends Expr
case class AssignExpr(lhs: Expr, rhs: Expr) extends Expr

//Typed AST
sealed trait TypedStmt {
	def isExpr() : Boolean = {false}
	def evalType() : Type;
}
case class TypedValDefStmt(ids : List[String], varType: Type, value : TypedExpr, valTypeflag : String, evalType : Type) extends TypedStmt
case class TypedFunDefStmt(name : String, params : List[TypedParamDclStmt], retType : Type, body : TypedExpr, evalType : Type) extends TypedStmt
case class TypedParamDclStmt(id : String, varType: Type, evalType : Type) extends TypedStmt
case class TypedClassDefStmt(caseFlag: Boolean,
                        className : String,
                        paramClauses: List[List[ParamDclStmt]],
                        extendClass: ClassInstance,
                        withIds: List[String],
                        body: List[Stmt],
                        evalType : Type) extends TypedStmt
                        
sealed trait TypedExpr extends TypedStmt {
	override def isExpr() : Boolean = {true}
}
case class TypedBoolExpr(bool : Boolean, evalType : Type) extends TypedExpr
case class TypedNumExpr(num: Numeric, evalType : Type) extends TypedExpr
case class TypedStringExpr(str : String, evalType : Type) extends TypedExpr
case class TypedCharExpr(ch : Char, evalType : Type) extends TypedExpr
case class TypedVarExpr(id: String, evalType : Type) extends TypedExpr
case class TypedFunExpr(id: TypedExpr, args : List[TypedExpr], evalType : Type) extends TypedExpr
case class TypedBlockExpr(body: List[TypedStmt], evalType : Type) extends TypedExpr
case class TypedBinOpExpr(op: String, left: TypedExpr, right: TypedExpr, evalType : Type) extends TypedExpr
case class TypedUnOpExpr(op: String, expr: TypedExpr, evalType : Type) extends TypedExpr
case class TypedIfThenExpr(test: TypedExpr, trueClause: TypedExpr, evalType : Type) extends TypedExpr
case class TypedIfThenElseExpr(test: TypedExpr, trueClause: TypedExpr, falseClause: TypedExpr, evalType : Type) extends TypedExpr
case class TypedWhileExpr(test: TypedExpr, body: TypedExpr, evalType : Type) extends TypedExpr
case class TypedAnonFuncExpr(args: List[TypedParamDclStmt], body: TypedExpr, evalType : Type) extends TypedExpr
case class TypedAssignExpr(lhs: TypedExpr, rhs: TypedExpr, evalType: Type) extends TypedExpr

//Classes used for compiling
sealed trait Numeric
case class NInt(num : Int) extends Numeric
case class NDouble(Num : Double) extends Numeric

case class DefWrapper(ids : List[String], varType: Type, value : Expr)
case class FunWrapper(name : String, paramClauses : List[List[ParamDclStmt]], retType : Type,  body : Expr)
case class ClassInstance(id: Type, argClauses: List[List[Expr]])

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
	def getRetType() : Type;
}
case class BaseType(varType : String) extends Type {
	def isFunc() : Boolean = false;
	def getType() : String = varType;
	def getArgTypes() : List[Type] = Nil;
	def getRetType() : Type = null;
}
case class FuncType(retType : Type, argTypes : List[Type]) extends Type {
	def isFunc() : Boolean = true;
	def getType() : String = "";
	def getRetType() : Type = retType;
	def getArgTypes() : List[Type] = argTypes;
}
case class ObjType(varType: String) extends Type{
    def isFunc() : Boolean = false;
	def getType() : String = varType;
	def getArgTypes() : List[Type] = Nil;
	def getRetType() : Type = null;
}