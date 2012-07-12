package compilerV0

import scala.collection.mutable.Map;

object TypeVerifier {
	//ScalaTypes maps a string which is a type name to the list of its parent types (including itself), starting from Any
	var scalaTypes : Map[String, List[String]] = Map[String, List[String]]()
	var verifyStack : List[Stmt] = Nil;
  
	def initScalaTypes(): Unit = {
		//Only init if they don't already exist
		if(scalaTypes.isEmpty){
		  	addType("Any", "");
		  	addType("AnyVal", "Any");
		  	addType("Double", "AnyVal");
		  	addType("Int", "AnyVal");
		  	addType("Boolean", "AnyVal");
		  	addType("Unit", "AnyVal");
		  	addType("AnyRef", "Any");
		  	addType("String", "AnyRef");
		  	addType("Function", "AnyRef");
		}
	}
	
	def addType(name : String, superType : String) : Unit = {
		if (!scalaTypes.contains(name)) {
			if (superType == "") {
				scalaTypes.put(name, List(name));
			} else {
				if (scalaTypes.contains(superType)){
					scalaTypes.put(name, scalaTypes.get(superType).get++List(name))
				} else {
					throw new Exception("The super type "+name+" for type "+name+" is not defined.");
				}
			}
		} else {
			throw new Exception("The type "+name+" is already defined.");
		}
	}
	def initSymbolTable(): Map[String, Type] = {
		var map : Map[String, Type] = scala.collection.mutable.Map[String, Type]()
		putFunc(map, "println", List("Any"), "Unit");
		putFunc(map, "print", List("Any"), "Unit");
		return map;
	}
	
	def checkTypeS(exprType : String, paramType : String): Boolean = {
		if (scalaTypes.contains(paramType)) { //Make sure the exprType is a defined type
			if (exprType == paramType) { //They match
				return true; 
			} else { //They don't match, check the parent types
				if (scalaTypes.contains(exprType)){
					return (scalaTypes.get(exprType).get).contains(paramType);
				} else {
					throw new Exception("Unknown type "+exprType+".")
				}
			}
		} else {
			throw new Exception("Unknown type "+paramType+".")
		}
	}
	def checkType(exprType : Type, paramType : Type): Boolean = {
		if (paramType.isFunc()) {
			if (exprType.isFunc()) {
				var paramRetType : Type = paramType.getRetType();
				var exprRetType : Type = exprType.getRetType();
			  	var paramArgTypes : List[Type] = paramType.getArgTypes();
				var exprArgTypes : List[Type] = exprType.getArgTypes();
				
				if (exprArgTypes.length != paramArgTypes.length) {
					//Functions take a different number of arguments - they don't match
					return false;
				} else {
					var result : Boolean = true;
					result = result && checkType(exprRetType, paramRetType);
					while (result && exprArgTypes.length > 0) {
						//Left hand side is the backwards relationship (call checkType with the opposite order of parameters)
						result = result && checkType(paramArgTypes.head, exprArgTypes.head);
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
				var paramTypeS : String = paramType.getType();
				var exprTypeS : String = exprType.getType();
				return checkTypeS(exprTypeS, paramTypeS);
			}
		}
	}
	
	def firstCommonSuperTypeS(type1 : String, type2 : String) : String = {
		if (scalaTypes.contains(type1)) {
			if (type1 == type2) {
				return type1; //Common superType
			} else {
				if (scalaTypes.contains(type2)) {
					var types1 : List[String] = scalaTypes.get(type1).get
					var types2 : List[String] = scalaTypes.get(type2).get
					var temp : String = "";
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
	def firstCommonSuperType(type1 : Type, type2 : Type) : Type = {
		if (type1.isFunc()) {
			if (type2.isFunc()) {
				var retType1 : Type = type1.getRetType();
				var retType2 : Type = type2.getRetType();
			  	var argTypes1 : List[Type] = type1.getArgTypes();
				var argTypes2 : List[Type] = type2.getArgTypes();
				
				if (argTypes1.length != argTypes2.length) {
					//Functions take a different number of arguments - they don't match - super type is Function
					return new BaseType("Function");
				} else {
					if (argTypes1 == argTypes2){
						return new FuncType(firstCommonSuperType(retType1, retType2), argTypes1);
					} else {
						//Left hand sides must be the same types - super type is Function
						return new BaseType("Function");
					}
				}
			} else {
				//Attempted to compare a FuncType to a BaseType - find super type using Function
				return new BaseType(firstCommonSuperTypeS("Function", type2.getType()));
			}
		} else {
			if (type2.isFunc()) {
				//Attempted to compare a FuncType to a BaseType - find super type using Function
				return new BaseType(firstCommonSuperTypeS(type1.getType(), "Function"));
			} else {
				return new BaseType(firstCommonSuperTypeS(type1.getType(), type2.getType()));
			}
		}
	}
	
	def processVerifyStack(maps : List[Map[String, Type]]) : List[TypedStmt] = {
		var result : List[TypedStmt] = Nil;
		while (verifyStack.length > 0) {
			var stmt : Stmt = verifyStack.head;
			verifyStack = verifyStack.tail;
			stmt match {
				case FunDefStmt(name, params, retType, body) => {
					var myMaps : List[Map[String, Type]] = scala.collection.mutable.Map[String, Type]()::maps
		    		//add params to the new map (by verifying them)
		    		var typedParamStats = params.map(param => verifyStmt(param, myMaps))
		    		var typedParams : List[TypedVarDclStmt] = Nil;
					
					//This whole chunk is just for typecasting
		    		while (typedParamStats.length > 0) {
		    			var typedParam = typedParamStats.head;
		    			typedParamStats = typedParamStats.tail;
		    			typedParam match {
		    			  	case TypedVarDclStmt(ids, varType, evalType) => typedParams = new TypedVarDclStmt(ids, varType, evalType) :: typedParams
		    			  	case _ => ; //This should never happen... just needing to use the match for typecasting
		    			}
		    		}
		    		typedParams = typedParams.reverse;
		    		
		    		var typedBody = verifyExpr(body, myMaps);
			  		if (!checkType(typedBody.evalType(), new BaseType(retType))) {
			  			throw new Exception("Body type "+typedBody.evalType()+" does not match the required return type "+retType+" for function "+name+".")
			  		}
			  		result = TypedFunDefStmt(name, typedParams, retType, typedBody, null) :: result
				}
				case _ => {
					throw new Exception("Unknown statement type on the verify stack.")
				}
			}
		}
		//Flip the list around so they are in reverse order - used later
		return result.reverse;
	}
	
	def pushVerifyStack(stmt : Stmt) : Unit = {
		verifyStack = stmt :: verifyStack;
	}
	
	def verifyStmt(ast : Stmt, maps : List[Map[String, Type]]) : TypedStmt = ast match {
		case ValDefStmt(listOfValNames, valType, expr) => {
	  		var exprTyped = verifyExpr(expr, maps);
	  		if (checkType(exprTyped.evalType(), new BaseType(valType))) {
	  			putAllVars(maps.head, listOfValNames, valType)
	  		} else {
	  			throw new Exception("Type "+exprTyped.evalType()+" does not match the required type "+valType+" for vals "+prettyPrint(listOfValNames)+".")
	  		}
	  		return new TypedValDefStmt(listOfValNames, valType, exprTyped, null);
	  	}
    	case VarDefStmt(listOfVarNames, varType, expr) => {
    		var exprTyped = verifyExpr(expr, maps);
	  		if (checkType(exprTyped.evalType(), new BaseType(varType))) {
	  			putAllVars(maps.head, listOfVarNames, varType)
	  		} else {
	  			throw new Exception("Type "+exprTyped.evalType()+" does not match the required type "+varType+" for vars "+prettyPrint(listOfVarNames)+".")
	  		}
	  		return new TypedVarDefStmt(listOfVarNames, varType, exprTyped, null);
    	}
    	case VarDclStmt(ids, varType) => {
	  		putAllVars(maps.head, ids, varType)
	  		return new TypedVarDclStmt(ids, varType, null);
    	}
    	case _ => return verifyExpr(ast, maps)
	}
	
	def verifyExpr(ast : Stmt, maps : List[Map[String, Type]]) : TypedExpr = ast match {
		case FunDefStmt(name, params, retType, body) => {
    		var paramTypes = params.map(param => param.varType)
  			//add to map
  			putFunc(maps.head, name, paramTypes, retType);
    		
    		//verify body vs return type (with the new map)
    		pushVerifyStack(new FunDefStmt(name, params, retType, body));
    		
    		//This should never be used, the real expr should come from the verifyStack
    	    return null
    	}
		case BlockExpr(listofstatements) => {
			var myMaps : List[Map[String, Type]] = scala.collection.mutable.Map[String, Type]()::maps
			var stmts = listofstatements;
	  		var typedStmts : List[TypedStmt] = Nil;
	  		while (stmts.length > 0) {
	  			var stmt : Stmt = stmts.head;
	  			stmts = stmts.tail;
	  			stmt match {
	  			  	case FunDefStmt(name, params, retType, body) => {
	  			  		verifyStmt(stmt, myMaps)
	  			  	}
	  			  	case _ => {
	  			  		typedStmts = processVerifyStack(myMaps) ++ typedStmts
	  			  		typedStmts = verifyStmt(stmt, myMaps) :: typedStmts
	  			  	}
	  			}
	  		}
	  		typedStmts = processVerifyStack(maps) ++ typedStmts;
	  		//Flip the list around so they are in order
	  		typedStmts = typedStmts.reverse;
	  		var retType : Type = null;
	  		if (typedStmts.length > 0) {
		  		var lastStmtType = typedStmts.last.evalType();
		  		if (lastStmtType == null) {
		  			retType = new BaseType("Unit");
		  		} else {
		  			retType = lastStmtType
		  		}
	  		} else {
	  			retType = new BaseType("Unit");
	  		}
	  		return new TypedBlockExpr(typedStmts, retType);
	  	}
	  	case BinOpExpr(op, l, r) => {
	  		var left = verifyExpr(l, maps);
	  		var right = verifyExpr(r, maps);
	  		var typeL : Type = left.evalType();
	  		var typeR : Type = right.evalType();
	  		
	  		if (typeL != typeR) {
	  			throw new Exception("Binary operator "+op+" cannot be applied to types "+typeL+" and "+typeR+".");
	  		} else {
	  			var retType : Type = null;
	  			op match {
	  			  	case ">=:" | "<=:" | ">:" | "<:" => retType = BaseType("Boolean")
	  			  	case ">=" | "<=" | ">" | "<" => retType = BaseType("Boolean")
	  			  	case "==:" | "!=:" => retType = BaseType("Boolean")
	  			  	case "==" | "!=" => retType = BaseType("Boolean")
	  			  	case _ => retType = typeL;
	  			}
	  			return new TypedBinOpExpr(op, left, right, retType);
	  		}
	  	}
	  	case IfThenExpr(predicate, expr) => {
	  		var predicateTyped = verifyExpr(predicate, maps)
	  		var exprTyped = verifyExpr(expr, maps)
	  		
	  		if (predicateTyped.evalType() != BaseType("Boolean")) {
	  			throw new Exception("If statements require a predicate of type Boolean");
	  		} else {
	  			var commonType : Type = firstCommonSuperType(exprTyped.evalType(), BaseType("Unit"));
	  			return new TypedIfThenExpr(predicateTyped, exprTyped, commonType);
	  		}
	  	}
  	    case IfThenElseExpr(predicate, trueValue, falseValue) => {
	  		var predicateTyped = verifyExpr(predicate, maps)
	  		var trueTyped = verifyExpr(trueValue, maps)
	  		var falseTyped = verifyExpr(falseValue, maps)
	  		if (predicateTyped.evalType() != BaseType("Boolean")) {
	  			throw new Exception("If statements require a predicate of type Boolean");
	  		} else {
	  			var commonType = firstCommonSuperType(trueTyped.evalType(), falseTyped.evalType());
	  			if (commonType == "") {
	  				throw new Exception("No common supertype found for "+trueTyped.evalType()+" and "+falseTyped.evalType()+". (Why not Any?)")
	  			} else {
	  				return new TypedIfThenElseExpr(predicateTyped, trueTyped, falseTyped, commonType);
	  			}
	  		}
	  	}
  	    case WhileExpr(predicate, body) => {
  	    	var predicateTyped = verifyExpr(predicate, maps)
  	    	var bodyTyped = verifyExpr(body, maps)
  	    	if (predicateTyped.evalType() != BaseType("Boolean")) {
	  			throw new Exception("While loops require a predicate of type Boolean");
	  		} else {
	  			return new TypedWhileExpr(predicateTyped, bodyTyped, BaseType("Unit"));
	  		}
  	    }
	  	case VarExpr(varName) => {
	  		return new TypedVarExpr(varName, getVarType(maps, varName));
	  	}
    	case FunExpr(id, args) => {
    		var idStr : String = "";
    		var idTyped = verifyExpr(id, maps);
    		idTyped match {
    		  	case TypedVarExpr(str, evalType) => {idStr = str}
    		  	case _ => {throw new Exception("Not yet typechecking non-free functions")} 
    		}
    		var funcType : Type = getFuncType(maps, idStr);
    		var retType : Type = funcType.getRetType();
    		var paramTypes : List[Type] = funcType.getArgTypes();
    		var argsTyped : List[TypedExpr] = args.map(arg => verifyExpr(arg, maps))
    		var argTypes : List[Type] = argsTyped.map(arg => arg.evalType())
    	    while (argTypes.length > 0 && paramTypes.length > 0) {
    	    	if (!checkType(argTypes.head, paramTypes.head)){
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
    		return new TypedFunExpr(idTyped, argsTyped, retType);
    	}
    	case StringExpr(value) => TypedStringExpr(value, new BaseType("String"))
    	case NumExpr(value) => {
    		var retType : Type = null;
    		value match {
    		  	case NInt(num) => retType = new BaseType("Int")
    		  	case NDouble(num) => retType = new BaseType("Double")
    		}
    		if (retType == null) {
    			throw new Exception("Unknown numeric subtype: "+ast);
    		} else {
    			return new TypedNumExpr(value, retType);
    		}
    	}
    	case BoolExpr(value) => new TypedBoolExpr(value, new BaseType("Boolean"))
		case _ => throw new Exception("Unknown expr: "+ast)
	}
	
	/*
	def contains(maps : List[Map[String, String]], varName: String) : Boolean = maps match {
	  	case Nil => false;
	  	case currentScope::rest => {
	  		if (currentScope.contains(varName))
	  			return true
	  		else
	  			return contains(rest, varName)
	  	}
	} */
	def getVarType(maps : List[Map[String, Type]], varName: String) : Type = maps match {
	  	case Nil => throw new Exception("Unknown variable "+varName+".");
	  	case currentScope::rest => {
	  		if (currentScope.contains(varName)) {
	  			var theType : Type = currentScope.get(varName).get
	  			if (!theType.isFunc()) {
	  				return theType;
	  			} else {
	  				return theType;
	  				//throw new Exception("Tried to use the function "+varName+" in a variable context (no parentheses).")
	  			}
  			} else {
	  			return getVarType(rest, varName)
	  		}
	  	}
	}
	def getFuncType(maps : List[Map[String, Type]], funcName: String) : Type = maps match {
	  	case Nil => throw new Exception("Unknown function "+funcName+".");
	  	case currentScope::rest => {
	  		if (currentScope.contains(funcName)) {
	  			var theType : Type = currentScope.get(funcName).get
	  			if (theType.isFunc()) {
	  				return theType;
	  			} else {
	  				throw new Exception("Tried to use the variable "+funcName+" in a function context (with parentheses).")
	  			}
  			} else {
	  			return getFuncType(rest, funcName)
	  		}
	  	}
	}
	def putFunc(map : Map[String, Type], funcName: String, params : List[String], retType : String): Boolean = {
		if (!map.contains(funcName)) {
			if (!scalaTypes.contains(retType)) { //Unknown return type
				throw new Exception("Unknown return type "+retType+" for function "+funcName+".")
			} else if (params.foldLeft(false)((result, param) => result || !scalaTypes.contains(param))) { //Unknown param type
				throw new Exception("Unknown parameter type found in parameters: "+params+" for function "+funcName+".")
			} else {
				var paramTypes : List[BaseType] = params.map(param => new BaseType(param))
				//Quick fix
				//map.put(funcName, new FuncType(retType, paramTypes));
				map.put(funcName, new FuncType(new BaseType(retType), paramTypes)); 
				return true;
			}
		} else {
			throw new Exception("The function "+funcName+" is already defined in the current scope.")
		}
	}
	def putAllVars(map : Map[String, Type], varNames: List[String], varType : String): Boolean = {
		if (!scalaTypes.contains(varType)) { //Unknown type
			throw new Exception("Unknown type "+varType+" for vars "+prettyPrint(varNames)+".")
		}
		return putAllVarsH(map, varNames, varType);
	}
	def putAllVarsH(map : Map[String, Type], varNames: List[String], varType : String): Boolean = varNames match {
	  	case Nil => return true;
	 	case x::xs => {
	 		if (!map.contains(x)) {
	 			map.put(x, new BaseType(varType)); 
	 			return putAllVars(map, xs, varType);
	 		} else {
	 			throw new Exception("The variable "+x+" is already defined in the current scope.")
	 		}
	 	}
	}
	
	def prettyPrint(l : List[String]) : String = {
		l.tail.fold(l.head)((result, element) => result + ", " + element)
	}
	
	def apply(source: Expr): TypedStmt = {
		initScalaTypes;
		verifyExpr(source, initSymbolTable()::Nil)
	}
}