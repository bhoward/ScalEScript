package compilerV0

import scala.collection.mutable.Map;

object TypeVerifier {
	//ScalaTypes maps a string which is a type name to the list of its parent types (including itself), starting from Any
	var scalaTypes : Map[String, List[String]] = Map[String, List[String]]()
	var scalaViews : Map[String, List[String]] = Map[String, List[String]]()
	var verifyStack : List[Stmt] = Nil;
	
	def initScalaTypes(): Unit = {
		//Only init if they don't already exist
		if(scalaTypes.isEmpty){
		  	addType("Any", "");
		  	addType("AnyVal", "Any");
		  	addType("Double", "AnyVal");
		  	addType("Int", "AnyVal");
		  	addType("Boolean", "AnyVal");
		  	addType("Char", "AnyVal");
		  	addType("Unit", "AnyVal");
		  	addType("AnyRef", "Any");
		  	addType("String", "AnyRef");
		  	addType("Function", "AnyRef");
		  	
		  	addView("Double", "");
			addView("Float", "Double");
			addView("Long", "Float");
			addView("Int", "Long");
			addView("Char", "Int");
			addView("Short", "Int");
			addView("Byte", "Short");
		}
	}
	
	def addView(name : String, convertType : String) : Unit = {
		if (!scalaViews.contains(name)) {
			if (convertType == "") {
				scalaViews.put(name, Nil);
			} else {
				if (scalaViews.contains(convertType)){
					scalaViews.put(name, convertType::scalaViews.get(convertType).get)
				} else {
					throw new Exception("The views for type "+convertType+" are not defined.");
				}
			}
		} else {
			throw new Exception("The views for "+name+" are already defined.");
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
					throw new Exception("The super type "+superType+" for type "+name+" is not defined.");
				}
			}
		} else {
			throw new Exception("The type "+name+" is already defined.");
		}
	}
	def initSymbolTable(): Map[String, Type] = {
		var map : Map[String, Type] = scala.collection.mutable.Map[String, Type]()
		putFunc(map, "println", List(BaseType("Any")), BaseType("Unit"));
		putFunc(map, "print", List(BaseType("Any")), BaseType("Unit"));
		return map;
	}
	
	def checkTypeS(exprType : String, paramType : String): Boolean = {
		if (scalaTypes.contains(paramType)) { //Make sure the paramType is a defined type
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
					return BaseType("Function");
				} else {
					if (argTypes1 == argTypes2){
						return FuncType(firstCommonSuperType(retType1, retType2), argTypes1);
					} else {
						//Left hand sides must be the same types - super type is Function
						return BaseType("Function");
					}
				}
			} else {
				//Attempted to compare a FuncType to a BaseType - find super type using Function
				return BaseType(firstCommonSuperTypeS("Function", type2.getType()));
			}
		} else {
			if (type2.isFunc()) {
				//Attempted to compare a FuncType to a BaseType - find super type using Function
				return BaseType(firstCommonSuperTypeS(type1.getType(), "Function"));
			} else {
				return BaseType(firstCommonSuperTypeS(type1.getType(), type2.getType()));
			}
		}
	}
	
	def processVerifyStack(maps : List[Map[String, Type]]) : List[TypedStmt] = {
		var result : List[TypedStmt] = Nil;
		while (verifyStack.length > 0) {
			var stmt : Stmt = verifyStack.head;
			verifyStack = verifyStack.tail;
			stmt match {
				case FunDefStmt(name, paramClauses, retType, body) => {
					var typedStmt : TypedFunDefStmt = null;
					var myMaps : List[Map[String, Type]] = scala.collection.mutable.Map[String, Type]()::maps
		    		//add params to the new map (by verifying them)
		    		var typedParamClauses = paramClauses.map(params => params.map(param => verifyParamDclStmt(param.id, param.varType, myMaps)))
		    		
		    		var typedBody = verifyExpr(body, myMaps);
			  		if (!checkType(typedBody.evalType(), retType)) {
			  			throw new Exception("Body type "+typedBody.evalType()+" does not match the required return type "+retType+" for function "+name+".")
			  		}
			  		
			  		if (typedParamClauses.length > 1) {
			  			//Curry the function here
			  			var paramClause = typedParamClauses.head;
			  			var restParamClause = typedParamClauses.tail
			  			var newBody = curryFunc(typedBody, retType, restParamClause.reverse)
			  			var newType = FuncType(newBody.evalType(), paramClause.map(param => param.varType))
			  			typedStmt = TypedFunDefStmt(name, paramClause, newType, newBody, null)
			  		} else if (typedParamClauses.length == 1) {
			  			//Normal old function
			  			var paramClause = typedParamClauses.head;
			  			typedStmt = TypedFunDefStmt(name, paramClause, retType, typedBody, null)
			  		} else {
			  			//No paramClauses
			  			typedStmt = TypedFunDefStmt(name, Nil, retType, typedBody, null)
			  		}
			  		
			  		result = typedStmt :: result
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
	
	/* Specific Verify Functions start here */
	def verifyValDefStmt(ids : List[String], valType: Type, value : Expr, valTypeFlag : String, maps : List[Map[String, Type]]) : TypedValDefStmt = {	
		var exprTyped = verifyExpr(value, maps);
  		if (checkType(exprTyped.evalType(), valType)) {
  			putAllVars(maps.head, ids, valType)
  		} else {
  			throw new Exception("Type "+exprTyped.evalType()+" does not match the required type "+valType+" for vals "+prettyPrint(ids)+".")
  		}
  		return TypedValDefStmt(ids, valType, exprTyped, valTypeFlag, null);
	}
	def verifyParamDclStmt(id : String, varType: Type, maps : List[Map[String, Type]]) : TypedParamDclStmt = {
		putAllVars(maps.head, List(id), varType)
		return TypedParamDclStmt(id, varType, null);
	}
	def verifyFunDefStmt(name : String, paramClauses : List[List[ParamDclStmt]], retType : Type, body : Expr, maps : List[Map[String, Type]]) : TypedFunDefStmt = {
		if (paramClauses.length > 0) {
			var paramClauseTypes = paramClauses.map(params => params.map(param => param.varType))
			var paramTypes = paramClauseTypes.head;
			
			//add to map
			putFunc(maps.head, name, paramTypes, curryFuncType(retType, paramClauseTypes.tail));
		} else {
			//add to map
			putFunc(maps.head, name, Nil, retType);
		}
		
		//verify body vs return type (with the new map)
		pushVerifyStack(FunDefStmt(name, paramClauses, retType, body));
		
		//This should never be used, the real expr should come from the verifyStack
	    return null
	}
	def verifyBlockExpr(listofstatements: List[Stmt], maps : List[Map[String, Type]]) : TypedBlockExpr = {
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
	  			retType = BaseType("Unit");
	  		} else {
	  			retType = lastStmtType
	  		}
  		} else {
  			retType = BaseType("Unit");
  		}
  		return TypedBlockExpr(typedStmts, retType);
	}
	def verifyBinOpExpr(op: String, l: Expr, r: Expr, maps : List[Map[String, Type]]) : TypedBinOpExpr = {
		var left = verifyExpr(l, maps);
  		var right = verifyExpr(r, maps);
  		var typeL : Type = left.evalType();
  		var typeR : Type = right.evalType();
  		
  		var retType : Type = null;
		
  		if (typeL != typeR) {
  			if ((op == "+") && (typeL == BaseType("String") || typeR == BaseType("String"))) {
				retType = BaseType("String")
			} else if (!typeL.isFunc() && !typeR.isFunc()) {
				//Check the views
				if (scalaViews.contains(typeL.getType()) && scalaViews.get(typeL.getType()).get.contains(typeR.getType())) {
					retType = typeR;
				} else if (scalaViews.contains(typeR.getType()) && scalaViews.get(typeR.getType()).get.contains(typeL.getType())) {
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
	def verifyIfThenExpr(predicate: Expr, expr: Expr, maps : List[Map[String, Type]]) : TypedIfThenExpr = {
		var predicateTyped = verifyExpr(predicate, maps)
  		var exprTyped = verifyExpr(expr, maps)
  		
  		if (predicateTyped.evalType() != BaseType("Boolean")) {
  			throw new Exception("If statements require a predicate of type Boolean");
  		} else {
  			var commonType : Type = firstCommonSuperType(exprTyped.evalType(), BaseType("Unit"));
  			return TypedIfThenExpr(predicateTyped, exprTyped, commonType);
  		}
	}
	def verifyIfThenElseExpr(predicate: Expr, trueValue: Expr, falseValue: Expr, maps : List[Map[String, Type]]) : TypedIfThenElseExpr = {
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
  				return TypedIfThenElseExpr(predicateTyped, trueTyped, falseTyped, commonType);
  			}
  		}
	}
	def verifyWhileExpr(predicate: Expr, body: Expr, maps : List[Map[String, Type]]) : TypedWhileExpr = {
		var predicateTyped = verifyExpr(predicate, maps)
    	var bodyTyped = verifyExpr(body, maps)
    	if (predicateTyped.evalType() != BaseType("Boolean")) {
  			throw new Exception("While loops require a predicate of type Boolean");
  		} else {
  			return TypedWhileExpr(predicateTyped, bodyTyped, BaseType("Unit"));
  		}
	}
	def verifyVarExpr(varName: String, maps : List[Map[String, Type]]) : TypedVarExpr = {
		return TypedVarExpr(varName, getVarType(maps, varName));
	}
	def verifyFunExpr(id: Expr, args : List[Expr], maps: List[Map[String, Type]]) : TypedFunExpr = {
		var idStr : String = "";
		var idTyped = verifyExpr(id, maps);
		var funcType : Type = idTyped match {
		  	case TypedVarExpr(str, evalType) => getFuncType(maps, str)
		  	case TypedAnonFuncExpr(args, body, evalType) => evalType
		  	case TypedFunExpr(id, args, evalType) => evalType
		  	case _ => {throw new Exception("Not yet typechecking this: "+idTyped)} 
		}
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
		return TypedFunExpr(idTyped, argsTyped, retType);
	}
	def verifyStringExpr(str : String, maps: List[Map[String, Type]]) : TypedStringExpr = {
		return TypedStringExpr(str, BaseType("String"))
	}
	def verifyCharExpr(ch : Char, maps: List[Map[String, Type]]) : TypedCharExpr = {
		return TypedCharExpr(ch, BaseType("Char"))
	}
	def verifyNumExpr(num: Numeric, maps: List[Map[String, Type]]) : TypedNumExpr = {
		var retType : Type = null;
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
	def verifyBoolExpr(bool : Boolean, maps: List[Map[String, Type]]) : TypedBoolExpr = {
		return TypedBoolExpr(bool, BaseType("Boolean"))
	}
	def verifyAnonFuncExpr(args: List[ParamDclStmt], body: Expr, maps: List[Map[String, Type]]) : TypedAnonFuncExpr = {
		var myMaps : List[Map[String, Type]] = scala.collection.mutable.Map[String, Type]()::maps
		var typedParams : List[TypedParamDclStmt] = args.map(param => verifyParamDclStmt(param.id, param.varType, myMaps))
		var typedBody = verifyExpr(body, myMaps)
		
		var bodyType = typedBody.evalType();
		var paramTypes = typedParams.map(param => param.varType)
		
		return TypedAnonFuncExpr(typedParams, typedBody, FuncType(bodyType, paramTypes))
	}
	def verifyAssignExpr(lhs: Expr, rhs: Expr, maps: List[Map[String, Type]]): TypedAssignExpr = {
	    val VarExpr(id) = lhs // TODO generalize to non-variable lhs
	    val lhsTyped = verifyExpr(lhs, maps);
	    val rhsTyped = verifyExpr(rhs, maps);
	    if (!checkType(rhsTyped.evalType(), lhsTyped.evalType())) {
	      throw new Exception("Illegal assignment of " + rhsTyped.evalType() + " to " + id + " of type " + lhsTyped.evalType());
	    }
	    return TypedAssignExpr(lhsTyped, rhsTyped, BaseType("Unit"))
	}
	
	/* Combined Verify Functions start here */
	def verifyStmt(ast : Stmt, maps : List[Map[String, Type]]) : TypedStmt = ast match {
		case ValDefStmt(listOfValNames, valType, expr, valTypeFlag) => verifyValDefStmt(listOfValNames, valType, expr, valTypeFlag, maps)
    	case ParamDclStmt(id, varType) => verifyParamDclStmt(id, varType, maps)
    	case FunDefStmt(name, params, retType, body) => verifyFunDefStmt(name, params, retType, body, maps)
    	case _ => return verifyExpr(ast, maps)
	}
	def verifyExpr(ast : Stmt, maps : List[Map[String, Type]]) : TypedExpr = ast match {
		case BlockExpr(listofstatements) => verifyBlockExpr(listofstatements, maps)
	  	case BinOpExpr(op, l, r) => verifyBinOpExpr(op, l, r, maps)
	  	case IfThenExpr(predicate, expr) => verifyIfThenExpr(predicate, expr, maps)
  	    case IfThenElseExpr(predicate, trueValue, falseValue) => verifyIfThenElseExpr(predicate, trueValue, falseValue, maps)
  	    case WhileExpr(predicate, body) => verifyWhileExpr(predicate, body, maps)
	  	case VarExpr(varName) => verifyVarExpr(varName, maps)
    	case FunExpr(id, args) => verifyFunExpr(id, args, maps)
    	case StringExpr(value) => verifyStringExpr(value, maps)
    	case CharExpr(value) => verifyCharExpr(value, maps)
    	case NumExpr(value) => verifyNumExpr(value, maps)
    	case BoolExpr(value) => verifyBoolExpr(value, maps)
    	case AnonFuncExpr(args, body) => verifyAnonFuncExpr(args, body, maps)
    	case AssignExpr(lhs, rhs) => verifyAssignExpr(lhs, rhs, maps)
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
	def putFunc(map : Map[String, Type], funcName: String, params : List[Type], retType : Type): Boolean = {
		if (!map.contains(funcName)) {
			if (!checkScalaTypes(retType)) { //Unknown return type
				throw new Exception("Unknown return type "+retType+" for function "+funcName+".")
			} else if (params.foldLeft(false)((result, param) => result || !checkScalaTypes(param))) { //Unknown param type
				throw new Exception("Unknown parameter type found in parameters: "+params+" for function "+funcName+".")
			} else {
				var paramTypes : List[Type] = params.map(param => param)
				//Quick fix
				//map.put(funcName, new FuncType(retType, paramTypes));
				map.put(funcName, FuncType(retType, paramTypes)); 
				return true;
			}
		} else {
			throw new Exception("The function "+funcName+" is already defined in the current scope.")
		}
	}
	def putAllVars(map : Map[String, Type], varNames: List[String], varType : Type): Boolean = {
		if (!checkScalaTypes(varType)) { //Unknown type
			throw new Exception("Unknown type "+varType+" for vars "+prettyPrint(varNames)+".")
		}
		return putAllVarsH(map, varNames, varType);
	}
	def putAllVarsH(map : Map[String, Type], varNames: List[String], varType : Type): Boolean = varNames match {
	  	case Nil => return true;
	 	case x::xs => {
	 		if (!map.contains(x)) {
	 			map.put(x, varType); 
	 			return putAllVars(map, xs, varType);
	 		} else {
	 			throw new Exception("The variable "+x+" is already defined in the current scope.")
	 		}
	 	}
	}
	
	def checkScalaTypes(varType : Type) : Boolean = {
		if (varType.isFunc()) {
			return varType.getArgTypes().foldLeft(checkScalaTypes(varType.getRetType()))((result, argType) => result && checkScalaTypes(argType));
		} else {
			return scalaTypes.contains(varType.getType());
		}
	}
	
	def curryFuncType(retType : Type, paramClauses: List[List[Type]]) : Type = {
		if (paramClauses.length == 0) {
			return retType
		} else {
			return curryFuncType(new FuncType(retType, paramClauses.head), paramClauses.tail)
		}
	}
	def curryFunc(body: TypedExpr, retType : Type, paramClauses: List[List[TypedParamDclStmt]]) : TypedExpr = {
		if (paramClauses.length == 0) {
			return body;
		} else {
			var newType = new FuncType(retType, paramClauses.head.map(param => param.varType))
			return curryFunc(TypedAnonFuncExpr(paramClauses.head, body, newType), newType, paramClauses.tail)
		}
	}
	
	def prettyPrint(l : List[String]) : String = {
		l.tail.fold(l.head)((result, element) => result + ", " + element)
	}
	
	def apply(source: Stmt): TypedStmt = {
		initScalaTypes;
		verifyStmt(source, initSymbolTable()::Nil)
	}
}