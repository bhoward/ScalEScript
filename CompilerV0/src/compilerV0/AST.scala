package compilerV0

//Note: Because of this import all "Map"s will be mutable by default
import scala.collection.mutable.Map;

//Non-Typed AST
sealed trait Stmt {
	def isExpr(): Boolean = {false}
}
case class ValDefStmt(ids : List[String], valType: Type, value : Expr, valTypeflag : String) extends Stmt
case class FunDefStmt(name : String, paramClauses : List[List[ParamDclStmt]], retType : Type, body : Expr) extends Stmt
case class ParamDclStmt(id : String, varType: Type) extends Stmt
case class ClassDefStmt(typeFlag: String,
                        className : String,
                        params: List[ParamDclStmt],
                        extendClass: ClassInstance,
                        withIds: List[Type],
                        body: List[Stmt]) extends Stmt                        
sealed trait Expr extends Stmt {
	override def isExpr(): Boolean = {true}
}
case class BoolExpr(bool: Boolean) extends Expr
case class NumExpr(num: Numeric) extends Expr
case class StringExpr(str: String) extends Expr
case class CharExpr(ch: Char) extends Expr
case class VarExpr(id: String) extends Expr
case class FunExpr(id: Expr, args: List[Expr]) extends Expr
case class BlockExpr(body: List[Stmt]) extends Expr
case class BinOpExpr(op: String, left: Expr, right: Expr) extends Expr
case class UnOpExpr(op: String, expr: Expr) extends Expr
case class IfThenExpr(test: Expr, trueClause: Expr) extends Expr
case class IfThenElseExpr(test: Expr, trueClause: Expr, falseClause: Expr) extends Expr
case class WhileExpr(test: Expr, body: Expr) extends Expr //TODO Implement Do-While loops (should be trivial)
case class AnonFuncExpr(args: List[ParamDclStmt], body: Expr) extends Expr
case class AssignExpr(lhs: Expr, rhs: Expr) extends Expr
case class ClassExpr(name: Type, args : List[Expr]) extends Expr
case class FieldSelectionExpr(selection: String) extends Expr

//Annotated AST
sealed trait Tableless {
	def symbolTable(): Scope = null;
}
sealed trait AnnotStmt {
	def isExpr(): Boolean = {false}
	def symbolTable(): Scope;
}
case class AnnotValDefStmt(ids: List[String], varType: Type, value: AnnotExpr, valTypeflag: String) extends AnnotStmt with Tableless
case class AnnotFunDefStmt(name: String, params: List[AnnotParamDclStmt], retType: Type, body: AnnotExpr, symbolTable: Scope) extends AnnotStmt
case class AnnotParamDclStmt(id: String, varType: Type) extends AnnotStmt with Tableless
case class AnnotClassDefStmt(typeFlag: String, className: String, params: List[AnnotParamDclStmt], extendClassId: String, extendClassArgs: List[AnnotExpr], 
							 withIds: List[String], body: List[AnnotStmt], symbolTable: ClassScope) extends AnnotStmt  
sealed trait AnnotExpr extends AnnotStmt {
	override def isExpr(): Boolean = {true}
}
case class AnnotBoolExpr(bool: Boolean) extends AnnotExpr with Tableless
case class AnnotNumExpr(num: Numeric) extends AnnotExpr with Tableless
case class AnnotStringExpr(str: String) extends AnnotExpr with Tableless
case class AnnotCharExpr(ch: Char) extends AnnotExpr with Tableless
case class AnnotVarExpr(id: String) extends AnnotExpr with Tableless
case class AnnotFunExpr(id: AnnotExpr, args: List[AnnotExpr]) extends AnnotExpr with Tableless
case class AnnotBlockExpr(body: List[AnnotStmt], symbolTable: Scope) extends AnnotExpr
case class AnnotBinOpExpr(op: String, left: AnnotExpr, right: AnnotExpr) extends AnnotExpr with Tableless
case class AnnotUnOpExpr(op: String, expr: AnnotExpr) extends AnnotExpr  with Tableless
case class AnnotIfThenExpr(test: AnnotExpr, trueClause: AnnotExpr) extends AnnotExpr with Tableless
case class AnnotIfThenElseExpr(test: AnnotExpr, trueClause: AnnotExpr, falseClause: AnnotExpr) extends AnnotExpr with Tableless
case class AnnotWhileExpr(test: AnnotExpr, body: AnnotExpr) extends AnnotExpr with Tableless
case class AnnotAnonFuncExpr(args: List[AnnotParamDclStmt], body: AnnotExpr, symbolTable: Scope) extends AnnotExpr
case class AnnotAssignExpr(lhs: AnnotExpr, rhs: AnnotExpr) extends AnnotExpr with Tableless

//Typed AST
sealed trait TypedStmt {
	def isExpr(): Boolean = {false}
	def evalType(): Type = null;
	def symbolTable(): Scope;
}
case class TypedValDefStmt(ids: List[String], varType: Type, value: TypedExpr, valTypeflag: String) extends TypedStmt with Tableless
case class TypedFunDefStmt(name: String, params: List[TypedParamDclStmt], retType: Type, body: TypedExpr, symbolTable: Scope) extends TypedStmt
case class TypedParamDclStmt(id: String, varType: Type) extends TypedStmt with Tableless
case class TypedClassDefStmt(typeFlag: String,
                        className: String,
                        params: List[TypedParamDclStmt],
                        extendClassId: String,
                        extendClassArgs: List[TypedExpr],
                        withIds: List[String],
                        body: List[TypedStmt],
                        symbolTable: ClassScope) extends TypedStmt                 
sealed trait TypedExpr extends TypedStmt {
	override def isExpr(): Boolean = {true}
	override def evalType(): Type;
}
case class TypedBoolExpr(bool: Boolean, override val evalType: Type) extends TypedExpr with Tableless
case class TypedNumExpr(num: Numeric, override val evalType: Type) extends TypedExpr with Tableless
case class TypedStringExpr(str: String, override val evalType: Type) extends TypedExpr with Tableless
case class TypedCharExpr(ch: Char, override val evalType: Type) extends TypedExpr with Tableless
case class TypedVarExpr(id: String, override val evalType: Type) extends TypedExpr with Tableless
case class TypedFunExpr(id: TypedExpr, args: List[TypedExpr], override val evalType: Type) extends TypedExpr with Tableless
case class TypedBlockExpr(body: List[TypedStmt], symbolTable: Scope, override val evalType: Type) extends TypedExpr
case class TypedBinOpExpr(op: String, left: TypedExpr, right: TypedExpr, override val evalType: Type) extends TypedExpr with Tableless
case class TypedUnOpExpr(op: String, expr: TypedExpr, override val evalType: Type) extends TypedExpr with Tableless
case class TypedIfThenExpr(test: TypedExpr, trueClause: TypedExpr, override val evalType: Type) extends TypedExpr with Tableless
case class TypedIfThenElseExpr(test: TypedExpr, trueClause: TypedExpr, falseClause: TypedExpr, override val evalType: Type) extends TypedExpr with Tableless
case class TypedWhileExpr(test: TypedExpr, body: TypedExpr, override val evalType: Type) extends TypedExpr with Tableless
case class TypedAnonFuncExpr(args: List[TypedParamDclStmt], body: TypedExpr, symbolTable: Scope, override val evalType: Type) extends TypedExpr
case class TypedAssignExpr(lhs: TypedExpr, rhs: TypedExpr, override val evalType: Type) extends TypedExpr with Tableless
case class TypedClassExpr(name: Type, args: List[Expr]) extends TypedExpr with Tableless
case class TypedFieldSelectionExpr(obj: String, field: String) extends TypedExpr with Tableless

//Classes used for compiling
sealed trait Numeric
case class NInt(num: Int) extends Numeric
case class NDouble(Num: Double) extends Numeric

case class DefWrapper(ids: List[String], varType: Type, value: Expr)
case class FunWrapper(name: String, paramClauses: List[List[ParamDclStmt]], retType: Type,  body: Expr)
case class ClassInstance(id: Type, args: List[Expr])
case class TraitInstance(id: Type)

sealed trait OpPair {
	def isLeft(): Boolean;
  	def getOp(): String;
  	def getExpr(): Expr;
}
case class LeftOpPair(op: String, expr: Expr) extends OpPair {
	def isLeft(): Boolean = true;
  	def getOp(): String = op;
  	def getExpr(): Expr = expr;
}
case class RightOpPair(op: String, expr: Expr) extends OpPair {
	def isLeft(): Boolean = false;
  	def getOp(): String = op;
  	def getExpr(): Expr = expr;
}

sealed trait Type {
	def isFunc(): Boolean;
	def getType(): String;
	def getArgTypes(): List[Type];
	def getRetType(): Type;
}
case class BaseType(varType: String) extends Type {
	def isFunc(): Boolean = false;
	def getType(): String = varType;
	def getArgTypes(): List[Type] = Nil;
	def getRetType(): Type = null;
}
case class FuncType(retType: Type, argTypes: List[Type]) extends Type {
	def isFunc(): Boolean = true;
	def getType(): String = "";
	def getRetType(): Type = retType;
	def getArgTypes(): List[Type] = argTypes;
}

case class Scope {
	var objects: Map[String, Type] = Map[String, Type]()
	var types: Map[String, ClassScope] = Map[String, ClassScope]()
	override def toString(): String = "Scope(Types="+types+", Objects="+objects+")";
}
case class ClassScope(sups: ()=>List[String], paramTypes: List[Type]) extends Scope {
	lazy val superTypes: List[String] = sups();
	override def toString(): String = "Scope(SuperTypes="+superTypes+",ParamTypes="+paramTypes+",Types="+types+",Objects="+objects+")";
	
	//This is purely for testing purposes. Without overriding, it will compare sups which will never match (they aren't the same reference)
	override def equals(obj: Any): Boolean = {
		if (obj.isInstanceOf[ClassScope]) {
			val otherObj: ClassScope = obj.asInstanceOf[ClassScope];
			return paramTypes == otherObj.paramTypes && superTypes == otherObj.superTypes && super.equals(otherObj);
		} else {
			return false;
		}
	}
}