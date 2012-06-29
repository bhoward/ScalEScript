package compilerV0

import scala.collection.mutable.Map;

object TypeVerifier {
	//ScalaTypes maps a string which is a type name to the list of its parent types (including itself), starting from Any
	var scalaTypes : Map[String, List[String]] = Map[String, List[String]]()
  
	def initScalaTypes {
	  	scalaTypes.put("Any", List[String]("Any"));
	  	
	  	scalaTypes.put("AnyVal", List[String]("Any", "AnyVal"));
	  	scalaTypes.put("Double", List[String]("Any", "AnyVal", "Double"));
	  	scalaTypes.put("Int", List[String]("Any", "AnyVal", "Int"));
	  	scalaTypes.put("Boolean", List[String]("Any", "AnyVal", "Boolean"));
	  	scalaTypes.put("Unit", List[String]("Any", "AnyVal", "Unit"));
	  	
	  	scalaTypes.put("AnyRef", List[String]("Any", "AnyRef"));
	  	scalaTypes.put("String", List[String]("Any", "AnyRef", "String"));
	}
	
	def checkType(exprType : String, paramType : String): Boolean = {
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
	
	def firstCommonSuperType(type1 : String, type2 : String) : String = {
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
	
	def verifyInit(ast : Expr) : Boolean = {
		var map : Map[String, Type] = scala.collection.mutable.Map[String, Type]()
		var maps : List[Map[String, Type]] = map :: Nil
		try { 
			verify(ast, maps)
		} catch {
			case e: Exception => {println(e.getMessage()+"\nException: "+e); return false}
		}
		return true;
	}
	
	def verify(ast : Stmt, maps : List[Map[String, Type]]) : String = ast match {
	  	case BlockExpr(listofstatements) => {
	  		var myMaps : List[Map[String, Type]] = scala.collection.mutable.Map[String, Type]()::maps
	  		var stmtTypes : List[String] = listofstatements.map(stmt => verify(stmt, myMaps))
	  		var lastStmtType = stmtTypes.last;
	  		if (lastStmtType == "") {
	  			return "Unit"
	  		} else {
	  			return lastStmtType
	  		}
	  	}
	  	case BinOpExpr(op, l, r) => {
	  		var type1 = verify(l, maps)
	  		var type2 = verify(r, maps)
	  		if (type1 != type2) {
	  			throw new Exception("Binary operator "+op+" cannot be applied to types "+type1+" and "+type2+".");
	  		} else {
	  			op match {
	  			  	case ">=:" | "<=:" | ">:" | "<:" => return "Boolean"
	  			  	case ">=" | "<=" | ">" | "<" => return "Boolean"
	  			  	case "==:" | "!=:" => return "Boolean"
	  			  	case "==" | "!=" => return "Boolean"
	  			  	case _ => return type1;
	  			}
	  		}
	  	}
	  	case IfThenExpr(predicate, expr) => {
	  		var predicateType = verify(predicate, maps)
	  		var exprType = verify(expr, maps)
	  		if (predicateType != "Boolean") {
	  			throw new Exception("If statements require a predicate of type Boolean");
	  		} else {
	  			return firstCommonSuperType(exprType, "Unit")
	  		}
	  	}
  	    case IfThenElseExpr(predicate, trueValue, falseValue) => {
	  		var predicateType = verify(predicate, maps)
	  		var trueType = verify(trueValue, maps)
	  		var falseType = verify(falseValue, maps)
	  		if (predicateType != "Boolean") {
	  			throw new Exception("If statements require a predicate of type Boolean");
	  		} else {
	  			var commonType = firstCommonSuperType(trueType, falseType);
	  			if (commonType == "") {
	  				throw new Exception("No common supertype found for "+trueType+" and "+falseType+". (Why not Any?)")
	  			} else {
	  				return commonType
	  			}
	  		}
	  	}
  	    case WhileExpr(predicate, body) => {
  	    	var predicateType = verify(predicate, maps)
  	    	var bodyType = verify(body, maps)
  	    	if (predicateType != "Boolean") {
	  			throw new Exception("While loops require a predicate of type Boolean");
	  		} else {
	  			return "Unit";
	  		}
  	    }
        case PrintExpr(msg) => {
        	var msgType = verify(msg, maps)
        	return "Unit"
        }
        case PrintlnExpr(msg) => {
        	var msgType = verify(msg, maps)
        	return "Unit"
        }
	  	case VarExpr(varName) => {
	  		return getVarType(maps, varName);
	  	}
	  	case ValDefStmt(listOfValNames, valtype, expr) => {
	  		var exprType = verify(expr, maps);
	  		if (checkType(exprType, valtype)) {
	  			putAllVars(maps.head, listOfValNames, valtype)
	  		} else {
	  			throw new Exception("Type "+exprType+" does not match the required type "+valtype+" for vals "+prettyPrint(listOfValNames)+".")
	  		}
	  		return "";
	  	}
    	case VarDefStmt(listOfVarNames, vartype, expr) => {
    		var exprType = verify(expr, maps);
	  		if (checkType(exprType, vartype)) {
	  			putAllVars(maps.head, listOfVarNames, vartype)
	  		} else {
	  			throw new Exception("Type "+exprType+" does not match the required type "+vartype+" for vars "+prettyPrint(listOfVarNames)+".")
	  		}
	  		return "";
    	}
    	case FunDefStmt(name, params, retType, body) => {
    	    
    	  
    	    return ""
    	}
    	case FunExpr(id, args) => {
    		var funcTypes : (String, List[String]) = getFuncType(maps, id);
    		var retType : String = funcTypes._1
    		var paramTypes : List[String] = funcTypes._2
    		var argTypes : List[String] = args.map(arg => verify(arg, maps))
    	    while (argTypes.length > 0 && paramTypes.length > 0) {
    	    	if (!checkType(argTypes.head, paramTypes.head)){
    	    		throw new Exception("Arguement type "+argTypes.head+" does not match the required type "+paramTypes.head+" for function "+id+".")
    	    	} else {
    	    		argTypes = argTypes.tail;
    	    		paramTypes = paramTypes.tail;
    	    	}
    	    	if (argTypes.length > 0) {
    	    		throw new Exception("Too many arguements specified for function "+id+".")
    	    	} else if (paramTypes.length > 0) {
    	    		throw new Exception("Not enough arguements specified for function "+id+".")
    	    	}
    	    }
    		return retType;
    	}
    	case StringExpr(value) => "String"
    	case NumExpr(value) => {
    		value match {
    		  	case Nint(num) => return "Int"
    		  	case Ndouble(num) => return "Double"
    		}
    		return "";
    	}
    	case BoolExpr(value) => "Boolean"
		case _ => ""
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
	def getVarType(maps : List[Map[String, Type]], varName: String) : String = maps match {
	  	case Nil => throw new Exception("Unknown variable "+varName+".");
	  	case currentScope::rest => {
	  		if (currentScope.contains(varName)) {
	  			var theType : Type = currentScope.get(varName).get
	  			if (theType.isVar()) {
	  				return theType.getType();
	  			} else {
	  				throw new Exception("Tried to use the function "+varName+" in a variable context (no parentheses).")
	  			}
  			} else {
	  			return getVarType(rest, varName)
	  		}
	  	}
	}
	def getFuncType(maps : List[Map[String, Type]], funcName: String) : (String, List[String]) = maps match {
	  	case Nil => throw new Exception("Unknown function "+funcName+".");
	  	case currentScope::rest => {
	  		if (currentScope.contains(funcName)) {
	  			var theType : Type = currentScope.get(funcName).get
	  			if (!theType.isVar()) {
	  				var argTypes : List[VarType] = theType.getArgTypes();
	  				return (theType.getType(), argTypes.map(argType => argType.getType()));
	  			} else {
	  				throw new Exception("Tried to use the variable "+funcName+" in a function context (with parentheses).")
	  			}
  			} else {
	  			return getFuncType(rest, funcName)
	  		}
	  	}
	}
	def putAllVars(map : Map[String, Type], varNames: List[String], varType : String): Boolean = {
		if (!scalaTypes.contains(varType)) { //Unknown type
			throw new Exception("Unknown type "+varType+".")
		}
		return putAllVarsH(map, varNames, varType);
	}
	def putAllVarsH(map : Map[String, Type], varNames: List[String], varType : String): Boolean = varNames match {
	  	case Nil => return true;
	 	case x::xs => {
	 		if (!map.contains(x)) {
	 			map.put(x, new VarType(varType)); 
	 			return putAllVars(map, xs, varType);
	 		} else {
	 			throw new Exception("The variable "+x+" is already defined in the current scope.")
	 		}
	 	}
	}
	
	def prettyPrint(l : List[String]) : String = {
		l.tail.fold(l.head)((result, element) => result + ", " + element)
	}
	
	def apply(source: Expr): Boolean = {
		initScalaTypes;
		/*
		println(firstCommonSuperType("Any", "Any"))
		println(firstCommonSuperType("Any", "Boolean"))
		println(firstCommonSuperType("Boolean", "Int"))
		println(firstCommonSuperType("Unit", "String"))
		println(checkType("Any", "Any"))
		println(checkType("Boolean", "Any"))
		println(checkType("AnyVal", "Any"))
		println(checkType("Int", "AnyVal"))
		println(checkType("Int", "AnyRef"))
		*/
		verifyInit(source);
	}
}