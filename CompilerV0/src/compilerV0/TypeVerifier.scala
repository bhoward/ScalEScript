package compilerV0

import scala.collection.mutable.Map;

object TypeVerifier {
	var scalaTypes : Map[String, List[String]] = Map[String, List[String]]()
  
	def initScalaTypes {
	  	scalaTypes.put("Any", Nil);
	  	
	  	scalaTypes.put("AnyVal", List[String]("Any"));
	  	scalaTypes.put("Double", List[String]("AnyVal"));
	  	scalaTypes.put("Int", List[String]("AnyVal"));
	  	scalaTypes.put("Boolean", List[String]("AnyVal"));
	  	scalaTypes.put("Unit", List[String]("AnyVal"));
	  	
	  	scalaTypes.put("AnyRef", List[String]("Any"));
	  	scalaTypes.put("String", List[String]("AnyRef"));
	}
	
	def checkType(exprType : String, paramType : String): Boolean = {
		if (scalaTypes.contains(exprType)) {
			if (exprType == paramType) {
				return true;
			} else {
				return (scalaTypes.get(exprType).get).foldLeft(false)((result, parentType) => result || checkType(parentType, paramType))
			}
		}
		
		//Unknown type
		return false;
	}
	
	def verifyInit(ast : Expr) : Boolean = {
		var map : Map[String, String] = scala.collection.mutable.Map[String, String]()
		var maps : List[Map[String, String]] = map :: Nil
		return verify(ast, maps);
	}
	
	def verify(ast : Stmt, maps : List[Map[String, String]]) : Boolean = ast match {
	  	case BlockExpr(listofstatements) => {
	  		var myMaps : List[Map[String, String]] = scala.collection.mutable.Map[String, String]()::maps
	  		listofstatements.foldLeft(true)((result, stmt) => result && verify(stmt, myMaps))
	  	}
	  	case BinOpExpr(op, l, r) => verify(l, maps) && verify(r, maps)
	  	case IfThenExpr(predicate, expr) => verify(predicate, maps) && verify(expr, maps)
  	    case IfThenElseExpr(predicate, truevalue, falsevalue) => verify(predicate, maps) && verify(truevalue, maps) && verify(falsevalue, maps)
  	    case WhileExpr(predicate, body) => verify(predicate, maps) && verify(body, maps)
        case PrintExpr(msg) => verify(msg, maps)
        case PrintlnExpr(msg) => verify(msg, maps)
	  	
	  	case VarExpr(varName) => contains(maps, varName)
	  	case ValDefStmt(listOfValNames, valtype, expr) => putAll(maps.head, listOfValNames, valtype)
    	case VarDefStmt(listOfVarNames, valtype, expr) => putAll(maps.head, listOfVarNames, valtype)
		case _ => true
	}
	
	def contains(maps : List[Map[String, String]], varName: String) : Boolean = maps match {
	  	case Nil => false;
	  	case x::xs => {
	  		if (x.contains(varName))
	  			return true
	  		else
	  			return contains(xs, varName)
	  	}
	}
	def putAll(map : Map[String, String], varNames: List[String], varType : String): Boolean = {
		if (!scalaTypes.contains(varType)) { //Unknown type
		  return false;
		}
		return putAllH(map, varNames, varType);
	}
	def putAllH(map : Map[String, String], varNames: List[String], varType : String): Boolean = varNames match {
	  	case Nil => return true;
	 	case x::xs => {
	 		if (!map.contains(x)) {
	 			map.put(x, varType); 
	 			return putAll(map, xs, varType);
	 		} else {
	 			return false;
	 		}
	 	}
	}
	
	def apply(source: Expr): Boolean = {
		initScalaTypes;
		/*
		println(scalaTypes);
		println(checkType("Boolean", "Boolean"))
		println(checkType("Boolean", "AnyVal"))
		println(checkType("Boolean", "Any"))
		println(checkType("Boolean", "Bloop"))
		println(checkType("Bloop", "Any"))
		*/
		verifyInit(source);
	}
}