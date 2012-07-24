package compilerV0

import scala.collection.mutable.Map;

object TypeVerifier {
	def apply(source: AnnotStmt): TypedStmt = {
		verify(source)
	}
	def verify(source: AnnotStmt): TypedStmt = {
		var scopes: List[Scope] = Scope()::ScalaBase.getScope()::Nil;
		
		return verifyStmt(source, scopes)
	}
	
	/* Combined Verify Functions start here */
	def verifyStmt(stmt: AnnotStmt, scopes: List[Scope]): TypedStmt = stmt match {
		case AnnotValDefStmt(ids, varType, value, valTypeflag) => verifyValDefStmt(ids, varType, value, valTypeflag, scopes)
    	case AnnotParamDclStmt(id, varType) => verifyParamDclStmt(id, varType, scopes)
    	case AnnotFunDefStmt(name, params, retType, body, symbolTable) => verifyFunDefStmt(name, params, retType, body, symbolTable, scopes)
    	case _ => return verifyExpr(stmt, scopes)
	}
	def verifyExpr(stmt: AnnotStmt, scopes: List[Scope]): TypedExpr = stmt match {
		case AnnotBlockExpr(body, symbolTable) => verifyBlockExpr(body, symbolTable, scopes)
	  	case AnnotBinOpExpr(op, left, right) => verifyBinOpExpr(op, left, right, scopes)
	  	case AnnotIfThenExpr(test, trueClause) => verifyIfThenExpr(test, trueClause, scopes)
  	    case AnnotIfThenElseExpr(test, trueClause, falseClause) => verifyIfThenElseExpr(test, trueClause, falseClause, scopes)
  	    case AnnotWhileExpr(test, body) => verifyWhileExpr(test, body, scopes)
	  	case AnnotVarExpr(id) => verifyVarExpr(id, scopes)
    	case AnnotFunExpr(id, args) => verifyFunExpr(id, args, scopes)
    	case AnnotStringExpr(str) => verifyStringExpr(str, scopes)
    	case AnnotCharExpr(ch) => verifyCharExpr(ch, scopes)
    	case AnnotNumExpr(num) => verifyNumExpr(num, scopes)
    	case AnnotBoolExpr(bool) => verifyBoolExpr(bool, scopes)
    	case AnnotAnonFuncExpr(args, body, symbolTable) => verifyAnonFuncExpr(args, body, symbolTable, scopes)
    	case AnnotAssignExpr(lhs, rhs) => verifyAssignExpr(lhs, rhs, scopes)
		case _ => throw new Exception("Unknown expr: "+stmt)
	}
	
	/* Specific Verify Functions start here */
	def verifyValDefStmt(ids: List[String], valType: Type, value: AnnotExpr, valTypeFlag: String, scopes: List[Scope]): TypedValDefStmt = {	
		var exprTyped = verifyExpr(value, scopes);
		if (!checkTypes(scopes, valType)) {
			throw new Exception("Type "+valType+" is not defined in the scope for vals "+prettyPrint(ids)+".")
		} else {
	  		if (!checkType(exprTyped.evalType(), valType, scopes)) {
	  			throw new Exception("Type "+exprTyped.evalType()+" does not match the required type "+valType+" for vals "+prettyPrint(ids)+".")
	  		}
		}
  		return TypedValDefStmt(ids, valType, exprTyped, valTypeFlag);
	}
	def verifyParamDclStmt(id: String, varType: Type, scopes: List[Scope]): TypedParamDclStmt = {
		if (!checkTypes(scopes, varType)) {
			throw new Exception("Type "+varType+" is not defined in the scope for parameter "+id+".")
		}
		return TypedParamDclStmt(id, varType);
	}
	def verifyFunDefStmt(name: String, paramClause: List[AnnotParamDclStmt], retType: Type, body: AnnotExpr, symbolTable: Scope, scopes: List[Scope]): TypedFunDefStmt = {
		var newScopes: List[Scope] = symbolTable::scopes;
		if (!checkTypes(newScopes, retType)){
			throw new Exception("Type "+retType+" is not defined in the scope for return type of function "+name+".")
		}
		
		var typedStmt: TypedFunDefStmt = null;
		var typedParamClause = paramClause.map(param => verifyParamDclStmt(param.id, param.varType, newScopes));
		
		var typedBody = verifyExpr(body, newScopes);
		if (!checkType(typedBody.evalType(), retType, scopes)) { //TODO - should this be scopes or newScopes?
  			throw new Exception("Body type "+typedBody.evalType()+" does not match the required return type "+retType+" for function "+name+".")
  		}
		
	    return TypedFunDefStmt(name, typedParamClause, retType, typedBody, symbolTable)
	}
	def verifyBlockExpr(listofstatements: List[AnnotStmt], symbolTable: Scope, scopes: List[Scope]): TypedBlockExpr = {
		var newScopes: List[Scope] = symbolTable::scopes;
  		var typedStmts: List[TypedStmt] = listofstatements.map(stmt => verifyStmt(stmt, newScopes));
  		var retType: Type = null;
  		if (typedStmts.length > 0) {
  			var lastStmt = typedStmts.last;
	  		var lastStmtType = typedStmts.last.evalType();
	  		if (lastStmt.isExpr()) {
	  			retType = lastStmt.evalType();
	  		} else {
	  			throw new Exception("The last line in the block is a Stmt, expected an Expr");
	  		}
  		} else {
  			retType = BaseType("Unit");
  		}
  		return TypedBlockExpr(typedStmts, symbolTable, retType);
	}
	def verifyBinOpExpr(op: String, l: AnnotExpr, r: AnnotExpr, scopes: List[Scope]): TypedBinOpExpr = {
		var left = verifyExpr(l, scopes);
  		var right = verifyExpr(r, scopes);
  		var typeL: Type = left.evalType();
  		var typeR: Type = right.evalType();
  		
  		var retType: Type = null;
		
  		if (typeL != typeR) {
  			if ((op == "+") && (typeL == BaseType("String") || typeR == BaseType("String"))) {
				retType = BaseType("String")
			} else if (!typeL.isFunc() && !typeR.isFunc()) {
				//Check the views
				if (ScalaBase.views.contains(typeL.getType()) && ScalaBase.views.get(typeL.getType()).get.contains(typeR.getType())) {
					retType = typeR;
				} else if (ScalaBase.views.contains(typeR.getType()) && ScalaBase.views.get(typeR.getType()).get.contains(typeL.getType())) {
					retType = typeL;
				}
			}
  		} else {
  			op match {
  			  	case ">=:" | "<=:" | ">:" | "<:" => retType = BaseType("Boolean")
  			  	case ">=" | "<=" | ">" | "<" => retType = BaseType("Boolean")
  			  	case "==:" | "!=:" => retType = BaseType("Boolean")
  			  	case "==" | "!=" => retType = BaseType("Boolean")
  			  	case _ => retType = typeL;
  			}
  		}
  		if (retType == null) {
  			throw new Exception("Binary operator "+op+" cannot be applied to types "+typeL+" and "+typeR+".");
  		}
  		return TypedBinOpExpr(op, left, right, retType);
	}
	def verifyIfThenExpr(predicate: AnnotExpr, expr: AnnotExpr, scopes: List[Scope]): TypedIfThenExpr = {
		var predicateTyped = verifyExpr(predicate, scopes)
  		var exprTyped = verifyExpr(expr, scopes)
  		
  		if (predicateTyped.evalType() != BaseType("Boolean")) {
  			throw new Exception("If statements require a predicate of type Boolean");
  		} else {
  			var commonType: Type = firstCommonSuperType(exprTyped.evalType(), BaseType("Unit"), scopes);
  			return TypedIfThenExpr(predicateTyped, exprTyped, commonType);
  		}
	}
	def verifyIfThenElseExpr(predicate: AnnotExpr, trueValue: AnnotExpr, falseValue: AnnotExpr, scopes: List[Scope]): TypedIfThenElseExpr = {
		var predicateTyped = verifyExpr(predicate, scopes)
  		var trueTyped = verifyExpr(trueValue, scopes)
  		var falseTyped = verifyExpr(falseValue, scopes)
  		if (predicateTyped.evalType() != BaseType("Boolean")) {
  			throw new Exception("If statements require a predicate of type Boolean");
  		} else {
  			var commonType = firstCommonSuperType(trueTyped.evalType(), falseTyped.evalType(), scopes);
  			if (commonType == "") {
  				throw new Exception("No common supertype found for "+trueTyped.evalType()+" and "+falseTyped.evalType()+". (Why not Any?)")
  			} else {
  				return TypedIfThenElseExpr(predicateTyped, trueTyped, falseTyped, commonType);
  			}
  		}
	}
	def verifyWhileExpr(predicate: AnnotExpr, body: AnnotExpr, scopes: List[Scope]): TypedWhileExpr = {
		var predicateTyped = verifyExpr(predicate, scopes)
    	var bodyTyped = verifyExpr(body, scopes)
    	if (predicateTyped.evalType() != BaseType("Boolean")) {
  			throw new Exception("While loops require a predicate of type Boolean");
  		} else {
  			return TypedWhileExpr(predicateTyped, bodyTyped, BaseType("Unit"));
  		}
	}
	def verifyVarExpr(varName: String, scopes: List[Scope]): TypedVarExpr = {
		return TypedVarExpr(varName, getObjType(scopes, varName));
	}
	def verifyFunExpr(id: AnnotExpr, args: List[AnnotExpr], scopes: List[Scope]): TypedFunExpr = {
		var idStr: String = "";
		var idTyped = verifyExpr(id, scopes);
		
		var funcType: Type = idTyped match {
		  	case TypedVarExpr(str, evalType) => getObjType(scopes, str)
		  	case _ => idTyped.evalType() 
		}
		if (!funcType.isFunc()){
			throw new Exception("Type "+funcType+" of expr "+id+" is not a function. It cannot be called on with arguements.")
		}
		
		var retType: Type = funcType.getRetType();
		var paramTypes: List[Type] = funcType.getArgTypes();
		var argsTyped: List[TypedExpr] = args.map(arg => verifyExpr(arg, scopes))
		var argTypes: List[Type] = argsTyped.map(arg => arg.evalType())
	    while (argTypes.length > 0 && paramTypes.length > 0) {
	    	if (!checkType(argTypes.head, paramTypes.head, scopes)){
	    		throw new Exception("Arguement type "+argTypes.head+" does not match the required type "+paramTypes.head+" for function "+id+".")
	    	} else {
	    		argTypes = argTypes.tail;
	    		paramTypes = paramTypes.tail;
	    	}
	    }
    	if (argTypes.length > 0) {
    		throw new Exception("Too many arguements specified for function "+id+".")
    	} else if (paramTypes.length > 0) {
    		throw new Exception("Not enough arguements specified for function "+id+".")
    	}
		return TypedFunExpr(idTyped, argsTyped, retType);
	}
	def verifyStringExpr(str: String, scopes: List[Scope]): TypedStringExpr = {
		return TypedStringExpr(str, BaseType("String"))
	}
	def verifyCharExpr(ch: Char, scopes: List[Scope]): TypedCharExpr = {
		return TypedCharExpr(ch, BaseType("Char"))
	}
	def verifyNumExpr(num: Numeric, scopes: List[Scope]): TypedNumExpr = {
		var retType: Type = null;
		num match {
		  	case NInt(num) => retType = BaseType("Int")
		  	case NDouble(num) => retType = BaseType("Double")
		}
		if (retType == null) {
			throw new Exception("Unknown numeric subtype: "+num);
		} else {
			return TypedNumExpr(num, retType);
		}
	}
	def verifyBoolExpr(bool: Boolean, scopes: List[Scope]): TypedBoolExpr = {
		return TypedBoolExpr(bool, BaseType("Boolean"))
	}
	def verifyAnonFuncExpr(args: List[AnnotParamDclStmt], body: AnnotExpr, symbolTable: Scope, scopes: List[Scope]): TypedAnonFuncExpr = {
		var newScopes: List[Scope] = symbolTable::scopes;
		
		var typedParams: List[TypedParamDclStmt] = args.map(param => verifyParamDclStmt(param.id, param.varType, newScopes))
		var typedBody = verifyExpr(body, newScopes)
		
		var bodyType = typedBody.evalType();
		var paramTypes = typedParams.map(param => param.varType)
		
		return TypedAnonFuncExpr(typedParams, typedBody, symbolTable, FuncType(bodyType, paramTypes))
	}
	def verifyAssignExpr(lhs: AnnotExpr, rhs: AnnotExpr, scopes: List[Scope]): TypedAssignExpr = {
	    val AnnotVarExpr(id) = lhs // TODO generalize to non-variable lhs
	    val lhsTyped = verifyExpr(lhs, scopes);
	    val rhsTyped = verifyExpr(rhs, scopes);
	    if (!checkType(rhsTyped.evalType(), lhsTyped.evalType(), scopes)) {
	    	throw new Exception("Illegal assignment of " + rhsTyped.evalType() + " to " + id + " of type " + lhsTyped.evalType());
	    }
	    return TypedAssignExpr(lhsTyped, rhsTyped, BaseType("Unit"))
	}
	
	
	
	/* Helper functions */
	def getObjType(scopes: List[Scope], varName: String): Type = scopes match {
	  	case Nil => throw new Exception("Unknown variable "+varName+".");
	  	case currentScope::rest => {
	  		var currentObjects = currentScope.objects;
	  		if (currentObjects.contains(varName)) {
	  			return currentObjects.get(varName).get
  			} else {
	  			return getObjType(rest, varName)
	  		}
	  	}
	}
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
			if (!checkTypes(scopes, retType)) { //Unknown return type
				throw new Exception("Unknown return type "+retType+" for function "+funcName+".");
			} else if (params.foldLeft(false)((result, param) => result || !checkTypes(scopes, param))) { //Unknown param type
				throw new Exception("Unknown parameter type found in parameters: "+params+" for function "+funcName+".");
			} else {
				var paramTypes: List[Type] = params.map(param => param);
				map.put(funcName, FuncType(retType, paramTypes)); 
				return true;
			}
		} else {
			throw new Exception("The function "+funcName+" is already defined in the current scope.");
		}
	}
	def putAllVars(scopes: List[Scope], varNames: List[String], varType: Type): Boolean = {
		var map: Map[String, Type] = scopes.head.objects;
		if (!checkTypes(scopes, varType)) { //Unknown type
			throw new Exception("Unknown type "+varType+" for vars "+prettyPrint(varNames)+".");
		}
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
	
	def checkTypeS(exprType: String, paramType: String, scopes: List[Scope]): Boolean = {
		if (containsType(scopes, paramType)) { //Make sure the paramType is a defined type
			if (exprType == paramType) { //They match
				return true; 
			} else { //They don't match, check the parent types
				if (containsType(scopes, exprType)){
					//TODO check not only scalaTypes for the type hierarchy
					return (ScalaBase.typeHierarchy.get(exprType).get).contains(paramType);
				} else {
					throw new Exception("Unknown type "+exprType+".")
				}
			}
		} else {
			throw new Exception("Unknown type "+paramType+".")
		}
	}
	def checkType(exprType: Type, paramType: Type, scopes: List[Scope]): Boolean = {
		if (paramType.isFunc()) {
			if (exprType.isFunc()) {
				var paramRetType: Type = paramType.getRetType();
				var exprRetType: Type = exprType.getRetType();
			  	var paramArgTypes: List[Type] = paramType.getArgTypes();
				var exprArgTypes: List[Type] = exprType.getArgTypes();
				
				if (exprArgTypes.length != paramArgTypes.length) {
					//Functions take a different number of arguments - they don't match
					return false;
				} else {
					var result: Boolean = true;
					result = result && checkType(exprRetType, paramRetType, scopes);
					while (result && exprArgTypes.length > 0) {
						//Left hand side is the backwards relationship (call checkType with the opposite order of parameters)
						result = result && checkType(paramArgTypes.head, exprArgTypes.head, scopes);
						paramArgTypes = paramArgTypes.tail;
						exprArgTypes = exprArgTypes.tail;
					}
					return result;
				}
			} else {
				//Attempted to compare a FuncType to a BaseType - they don't match
				return false;
			}
		} else {
			if (exprType.isFunc()) {
				//Attempted to compare a BaseType to a FuncType - they don't match
				return false;
			} else {
				var paramTypeS: String = paramType.getType();
				var exprTypeS: String = exprType.getType();
				return checkTypeS(exprTypeS, paramTypeS, scopes);
			}
		}
	}
	
	def firstCommonSuperTypeS(type1: String, type2: String, scopes: List[Scope]): String = {
		if (containsType(scopes, type1)) {
			if (type1 == type2) {
				return type1; //Common superType
			} else {
				if (containsType(scopes, type2)) {
					//TODO check not only scalaTypes for the type hierarchy
					var types1: List[String] = ScalaBase.typeHierarchy.get(type1).get
					var types2: List[String] = ScalaBase.typeHierarchy.get(type2).get
					var temp: String = "";
					while (types1.length > 0 && types2.length > 0 && types1.head == types2.head) {
						temp = types1.head;
						types1 = types1.tail;
						types2 = types2.tail;
					}
					return temp;
				} else {
					throw new Exception("Unknown type "+type2+".")
				}
			}
		} else {
			throw new Exception("Unknown type "+type1+".")
		}
	}
	def firstCommonSuperType(type1: Type, type2: Type, scopes: List[Scope]): Type = {
		if (type1.isFunc()) {
			if (type2.isFunc()) {
				var retType1: Type = type1.getRetType();
				var retType2: Type = type2.getRetType();
			  	var argTypes1: List[Type] = type1.getArgTypes();
				var argTypes2: List[Type] = type2.getArgTypes();
				
				if (argTypes1.length != argTypes2.length) {
					//Functions take a different number of arguments - they don't match - super type is Function
					return BaseType("Function");
				} else {
					if (argTypes1 == argTypes2){
						return FuncType(firstCommonSuperType(retType1, retType2, scopes), argTypes1);
					} else {
						//Left hand sides must be the same types - super type is Function
						return BaseType("Function");
					}
				}
			} else {
				//Attempted to compare a FuncType to a BaseType - find super type using Function
				return BaseType(firstCommonSuperTypeS("Function", type2.getType(), scopes));
			}
		} else {
			if (type2.isFunc()) {
				//Attempted to compare a FuncType to a BaseType - find super type using Function
				return BaseType(firstCommonSuperTypeS(type1.getType(), "Function", scopes));
			} else {
				return BaseType(firstCommonSuperTypeS(type1.getType(), type2.getType(), scopes));
			}
		}
	}
	/* End helper functions */

	
}