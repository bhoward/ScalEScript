package compilerV0

//Note: Because of this import all "Map"s will be mutable by default
import scala.collection.mutable.Map;

object ASTConverter {
	def apply(source: Stmt): AnnotStmt = {
		convert(source)
	}
	
	/* Convert functions */
	def convert(ast: Stmt): AnnotStmt = {
		var scopes: List[Scope] = Scope()::ScalaBase.getScope()::Nil;
		
		return convertStmt(ast, scopes);
	}
	def convertStmt(stmt: Stmt, scopes: List[Scope]): AnnotStmt = stmt match {
		case ValDefStmt(listOfValNames, valType, expr, valTypeFlag) => convertValDefStmt(listOfValNames, valType, expr, valTypeFlag, scopes);
    	case ParamDclStmt(id, varType) => convertParamDclStmt(id, varType, scopes);
    	case FunDefStmt(name, params, retType, body) => convertFunDefStmt(name, params, retType, body, scopes);
    	case _ => return convertExpr(stmt, scopes);
	}
	def convertExpr(stmt: Stmt, scopes: List[Scope]): AnnotExpr = stmt match {
		case BlockExpr(listofstatements) => convertBlockExpr(listofstatements, scopes);
	  	case BinOpExpr(op, l, r) => convertBinOpExpr(op, l, r, scopes);
	  	case IfThenExpr(predicate, expr) => convertIfThenExpr(predicate, expr, scopes);
  	    case IfThenElseExpr(predicate, trueValue, falseValue) => convertIfThenElseExpr(predicate, trueValue, falseValue, scopes);
  	    case WhileExpr(predicate, body) => convertWhileExpr(predicate, body, scopes);
	  	case VarExpr(varName) => convertVarExpr(varName, scopes);
    	case FunExpr(id, args) => convertFunExpr(id, args, scopes);
    	case StringExpr(value) => convertStringExpr(value, scopes);
    	case CharExpr(value) => convertCharExpr(value, scopes);
    	case NumExpr(value) => convertNumExpr(value, scopes);
    	case BoolExpr(value) => convertBoolExpr(value, scopes);
    	case AnonFuncExpr(args, body) => convertAnonFuncExpr(args, body, scopes);
    	case AssignExpr(lhs, rhs) => convertAssignExpr(lhs, rhs, scopes);
		case _ => throw new Exception("Unknown expr: "+stmt);
	}
	def convertValDefStmt(ids: List[String], valType: Type, value: Expr, valTypeFlag: String, scopes: List[Scope]): AnnotValDefStmt = {	
		//Add the vals to the scope
		putAllVars(scopes, ids, valType); // TODO handle valTypeFlag (lazy/val/var)
		
		var annotValue: AnnotExpr = convertExpr(value, scopes);
		return AnnotValDefStmt(ids, valType, annotValue, valTypeFlag);
	}
	def convertParamDclStmt(id: String, varType: Type, scopes: List[Scope]): AnnotParamDclStmt = {
		//Add the vals to the scope
		putAllVars(scopes, List(id), varType);

		return AnnotParamDclStmt(id, varType);
	}
	def convertFunDefStmt(name: String, paramClauses: List[List[ParamDclStmt]], retType: Type, body: Expr, scopes: List[Scope]): AnnotFunDefStmt = {
		//To start with, check to see if the function needs to be curried
		if(paramClauses.length > 1){
			var newFunc: FunDefStmt = curryFunc(name, paramClauses, retType, body);
			
			//Completely ignore the old function. Just return the converted form of the curried one
			return convertFunDefStmt(newFunc.name, newFunc.paramClauses, newFunc.retType, newFunc.body, scopes);
		} else {
			//From this point on you can assume there is only one param clause (it would have been curried otherwise)
			var paramClause: List[ParamDclStmt] = paramClauses.head;
		  
			var newScope: Scope = Scope();
			
		    var annotParams: List[AnnotParamDclStmt] = paramClause.map(param => convertParamDclStmt(param.id, param.varType, newScope::scopes));
		    var annotBody: AnnotExpr = convertExpr(body, newScope::scopes);
		    
		    //Add the function to the scope
		    if (paramClauses.length > 0) {
				var paramTypes: List[Type] = paramClause.map(param => param.varType)
				putFunc(scopes, name, paramTypes, retType);
			} else {
				putFunc(scopes, name, Nil, retType);
			}
		    return AnnotFunDefStmt(name, annotParams, retType, annotBody, newScope)
		}
	}
	def convertBlockExpr(stmts: List[Stmt], scopes: List[Scope]): AnnotBlockExpr = {
		var newScope: Scope = Scope();
		var annotStmts: List[AnnotStmt] = stmts.map(stmt => convertStmt(stmt, newScope::scopes));
		return AnnotBlockExpr(annotStmts, newScope);
	}
	def convertBinOpExpr(op: String, l: Expr, r: Expr, scopes: List[Scope]): AnnotBinOpExpr = {
		var annotL: AnnotExpr = convertExpr(l, scopes);
		var annotR: AnnotExpr = convertExpr(r, scopes);
		return AnnotBinOpExpr(op, annotL, annotR);
	}
	def convertIfThenExpr(predicate: Expr, expr: Expr, scopes: List[Scope]): AnnotIfThenExpr = {
		var annotPredicate: AnnotExpr = convertExpr(predicate, scopes);
		var annotExpr: AnnotExpr = convertExpr(expr, scopes);
		return AnnotIfThenExpr(annotPredicate, annotExpr);
	}
	def convertIfThenElseExpr(predicate: Expr, trueValue: Expr, falseValue: Expr, scopes: List[Scope]): AnnotIfThenElseExpr = {
		var annotPredicate: AnnotExpr = convertExpr(predicate, scopes);
		var annotTrue: AnnotExpr = convertExpr(trueValue, scopes);
		var annotFalse: AnnotExpr = convertExpr(falseValue, scopes);
		return AnnotIfThenElseExpr(annotPredicate, annotTrue, annotFalse);
	}
	def convertWhileExpr(predicate: Expr, body: Expr, scopes: List[Scope]): AnnotWhileExpr = {
		var annotPredicate: AnnotExpr = convertExpr(predicate, scopes);
		var annotBody: AnnotExpr = convertExpr(body, scopes);
		return AnnotWhileExpr(annotPredicate, annotBody);
	}
	def convertVarExpr(varName: String, scopes: List[Scope]): AnnotVarExpr = {
		//TODO check if var is defined?
		return AnnotVarExpr(varName);
	}
	def convertFunExpr(id: Expr, args: List[Expr], scopes: List[Scope]): AnnotFunExpr = {
		//TODO check if function is defined? -- But not all the time?
		var annotId: AnnotExpr = convertExpr(id, scopes);
		var annotArgs: List[AnnotExpr] = args.map(arg => convertExpr(arg, scopes));
		return AnnotFunExpr(annotId, annotArgs);
	}
	def convertStringExpr(str: String, scopes: List[Scope]): AnnotStringExpr = {
		return AnnotStringExpr(str);
	}
	def convertCharExpr(ch: Char, scopes: List[Scope]): AnnotCharExpr = {
		return AnnotCharExpr(ch);
	}
	def convertNumExpr(num: Numeric, scopes: List[Scope]): AnnotNumExpr = {
		return AnnotNumExpr(num);
	}
	def convertBoolExpr(bool: Boolean, scopes: List[Scope]): AnnotBoolExpr = {
		return AnnotBoolExpr(bool);
	}
	def convertAnonFuncExpr(args: List[ParamDclStmt], body: Expr, scopes: List[Scope]): AnnotAnonFuncExpr = {
		var newScope: Scope = Scope();
		
		var annotArgs: List[AnnotParamDclStmt] = args.map(arg => convertParamDclStmt(arg.id, arg.varType, newScope::scopes));
		var annotBody: AnnotExpr = convertExpr(body, newScope::scopes);
		return AnnotAnonFuncExpr(annotArgs, annotBody, newScope);
	}
	def convertAssignExpr(lhs: Expr, rhs: Expr, scopes: List[Scope]): AnnotAssignExpr = {
	    var annotLhs: AnnotExpr = convertExpr(lhs, scopes);
		var annotRhs: AnnotExpr = convertExpr(rhs, scopes);
		return AnnotAssignExpr(annotLhs, annotRhs);
	}
	/* End convert functions */
	
	/* Currying functions */
	def curryFuncType(retType: Type, paramClauses: List[List[Type]]): Type = {
		if (paramClauses.length == 0) {
			return retType;
		} else {
			return curryFuncType(new FuncType(retType, paramClauses.head), paramClauses.tail);
		}
	}
	def curryFunc(name: String, paramClauses: List[List[ParamDclStmt]], retType: Type, body: Expr) : FunDefStmt = {
		if (paramClauses.length > 1) {
			var paramClause: List[ParamDclStmt] = paramClauses.head;
			var paramRest: List[List[ParamDclStmt]] = paramClauses.tail;
			var paramRestTypes: List[List[Type]] = paramRest.map(params => params.map(param => param.varType));
			
			//Make sure to call curryFuncH with reversed parameterClauses (more efficient to work from the head than from the tail)
			return FunDefStmt(name, List(paramClause), curryFuncType(retType, paramRestTypes), curryFuncH(body, retType, paramRest.reverse))
		} else {
			//No need to curry the function - there is only 1 param clause
			return FunDefStmt(name, paramClauses, retType, body);
		}
	}
	def curryFuncH(body: Expr, retType: Type, paramClauses: List[List[ParamDclStmt]]): Expr = {
		if (paramClauses.length == 0) {
			return body;
		} else {
			var newType = new FuncType(retType, paramClauses.head.map(param => param.varType));
			return curryFuncH(AnonFuncExpr(paramClauses.head, body), newType, paramClauses.tail);
		}
	}
	/* End currying functions */
	
	/* Helper functions */
	def checkTypes(scopes: List[Scope], varType: Type): Boolean = {
		if (varType.isFunc()) {
			return varType.getArgTypes().foldLeft(checkTypes(scopes, varType.getRetType()))((result, argType) => result && checkTypes(scopes: List[Scope], argType));
		} else {
			return containsType(scopes, varType.getType());
		}
	}
	def containsType(scopes: List[Scope], varName: String): Boolean = scopes match {
	  	case Nil => false;
	  	case currentScope::rest => {
	  		if (currentScope.types.contains(varName))
	  			return true
	  		else
	  			return containsType(rest, varName)
	  	}
	}
	def putFunc(scopes: List[Scope], funcName: String, params: List[Type], retType: Type): Boolean = {
		var map: Map[String, Type] = scopes.head.objects;
		if (!map.contains(funcName)) {
			var paramTypes: List[Type] = params.map(param => param);
			map.put(funcName, FuncType(retType, paramTypes)); 
			return true;
		} else {
			throw new Exception("The function "+funcName+" is already defined in the current scope.");
		}
	}
	def putAllVars(scopes: List[Scope], varNames: List[String], varType: Type): Boolean = {
		var map: Map[String, Type] = scopes.head.objects;
		return putAllVarsH(map, varNames, varType);
	}
	def putAllVarsH(map: Map[String, Type], varNames: List[String], varType: Type): Boolean = varNames match {
	  	case Nil => return true;
	 	case x::xs => {
	 		if (!map.contains(x)) {
	 			map.put(x, varType); 
	 			return putAllVarsH(map, xs, varType);
	 		} else {
	 			throw new Exception("The variable "+x+" is already defined in the current scope.");
	 		}
	 	}
	}
	def prettyPrint(l: List[String]): String = {
		l.tail.fold(l.head)((result, element) => result + ", " + element);
	}
	/* End helper functions */
}