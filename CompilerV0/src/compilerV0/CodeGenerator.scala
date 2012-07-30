package compilerV0

object CodeGenerator {
	def generate (ast : TypedStmt, cObj:String):String = ast match {
	    case TypedAnonFuncExpr(args, body, symbolTable, rettype) => "(function (" + commaSeparatedProcess(args, cObj) + 
	                                                   " ) { return " + generate(body, cObj) + " }) "
	    case TypedAssignExpr(lhs, rhs, valtype) => "(" + generate(lhs, cObj) + " = " + generate(rhs, cObj) + ")"
        case TypedBinOpExpr(op, l, r, rettype) =>  "(" + generate(l, cObj) + " " + (op match {case "==" => "==="
    	                                                                  case "!=" => "!=="
    	                                                                  case _ => op}) + " " + generate(r, cObj) + ")"

    	
    	case TypedBlockExpr(listofstatements, symbolTable, rettype) => "(function() { \n" + blockProcess(listofstatements, cObj) + " })()"
    	case TypedBoolExpr(value, valtype) => value.toString
    	case TypedIfThenExpr(predicate, expr, valtype) => "ifThen( " + 
		     thunkify(generate(predicate, cObj)) + ", " +
		     thunkify(generate(expr, cObj)) + ")"
		case TypedIfThenElseExpr(predicate, truevalue, falsevalue, valtype) => 
    		 "ifThenElse( " +
		     thunkify(generate(predicate, cObj)) + ", " +
    	   	 thunkify(generate(truevalue, cObj)) + ", " +
    		 thunkify(generate(falsevalue, cObj)) + " )"     
    	case TypedFunExpr(name, args, rettype) => generate(name, cObj) + "(" + commaSeparatedProcess(args, cObj) + ")"
    	case TypedNumExpr(wrappedval, valtype) => wrappedval match{case NInt(value) => value.toString
    	                                                           case NDouble(value) => value.toString}
    	case TypedStringExpr(value, valtype) => "\"" + (value.map(escapify).mkString) + "\""
    	case TypedCharExpr(value, valtype) => "\"" + escapify(value) + "\""
    	case TypedVarExpr(varName, valtype) => varName
    	//case TypedFieldSelectionExpr(obj:String, field:String) => obj + " " + field
    	
    	case TypedWhileExpr(predicate, body, valtype) => "whileLoop( " +
		     thunkify(generate(predicate, cObj)) + ", " + 
		     thunkify(generate(body, cObj)) + " )"
    	
    	case TypedClassDefStmt(objType, name, constrParams, whatExtends, extendsWith, body, symTable, supers) => 
    	   objType match {
    	     case "trait" => traitGenerator(name, whatExtends, extendsWith, body, cObj)
    	     case _ => "fail"
    	     //"class" => classGenerator()
    	     //"object" => objectGenerator()
    	}
    	
    	     
    	case TypedFunDefStmt(name, args, retType, body, symbolTable) => "var " + name + " = function ( " + commaSeparatedProcess(args, cObj) + " )\n {" +
                                                                                   (if(retType == "Unit") generate(body, cObj) + "; return "
    	                                                                           else "return " + generate(body, cObj)) +
    	                                                                           "; \n }"
    	case TypedParamDclStmt(id, vartype) => id
    	case TypedValDefStmt(listofvaldecs, valtype, expr, valTypeFlag) => "var " + varProcess(listofvaldecs, expr) + 
                                                         " " + varProcessAux(listofvaldecs, expr, cObj)
        case _ => throw new Exception("No match found for pattern")
    
	}
	def thunkify(code: String): String = "(function() {\n return " + code + "})"
	
	def escapify(ch: Char): String = ch match {
	    case '\b' => "\\b"
	    case '\f' => "\\f"
	    case '\n' => "\\n"
	    case '\r' => "\\r"
	    case '\t' => "\\t"
	    case '"' => "\\\""
	    case '\\' => "\\\\"
	    case _ => ch.toString
	}
	
	def commaSeparatedProcess(lost : List[TypedStmt], cObj : String):String = lost match {
	  	case List() => ""
	  	case List(x) => generate(x, cObj)
	  	case x::xs => generate(x, cObj) + ", " + commaSeparatedProcess(xs, cObj)
	}
	def blockProcess(lost : List[TypedStmt], cObj : String):String = lost match {
    	case List() => "return ;"
    	case List(x) => if (x.isExpr()) "return " + generate(x, cObj) + 
    	                                " ;" else throw new Exception("The last line in the block is a Stmt, expected an Expr")
    	case x::xs => generate(x, cObj) + "; \n" + blockProcess(xs, cObj)
	}
	def varProcess(los : List[String], expr : TypedExpr):String = los match{
    	case Nil => ""
    	case x::Nil => x + " ; \n"
    	case x::xs => x + " , " + varProcess(xs, expr)
	} 
	def traitGenerator(name : String, extendsWhat : ClassInstance, extendsWith : List[String], body : List[TypedStmt], cObj : String) = 
	  cObj + "." + name + " = " + "{}; \n" +
	  cObj + "." + name + "_initProto = " + initProto(name, body, cObj) + "\n" +
	  cObj + "." + name + "_init = " 
	
	
	def varProcessAux(los : List[String], expr : TypedExpr, cObj : String):String = los match{
  		case Nil => generate(expr, cObj)
  		case x::xs => x + " = " + varProcessAux(xs, expr, cObj)
	}
	
	def lookupOp(op : String, table : Map[String, String]):String = table.get(op) match{
	   case Some(operator) => operator
	   case None => op
	}
	
	def processTraitBody(body : List[TypedStmt]) = body match {
	   case Nil => ""
	   case x::xs => x match {
	     case TypedFunDefStmt(name, args, retType, body, symbolTable) => ""
	     case _ => ""
	   }
	}
	
	
	
	def initProto(name : String, body : List[TypedStmt], cObj : String) = "function(p) {" + body.map(x => gen(x, cObj)) + "p.supers[" + name + "] = true; };"
	
	  
	def gen(stmt : TypedStmt, cObj : String) = stmt match {
	  case TypedFunDefStmt(name, params, retType, body, symTable) => name + " = function(" + commaSeparatedProcess(params, cObj) + "){ \n" +
	  																 "var self = this; \n" + "return ...." //TODO// Take body of function
	  
	  case _ => ""
	}
	
	//def classGen(name : Type, )
	def apply(source: TypedStmt, currentObj: String): String = generate (source, currentObj)
}
