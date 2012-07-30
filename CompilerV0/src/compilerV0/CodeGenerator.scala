package compilerV0

object CodeGenerator {
	def generate (ast : TypedStmt):String = ast match {
	    case TypedAnonFuncExpr(args, body, symbolTable, rettype) => "(function (" + commaSeparatedProcess(args) + 
	                                                   " ) { return " + generate(body) + " }) "
	    case TypedAssignExpr(lhs, rhs, valtype) => "(" + generate(lhs) + " = " + generate(rhs) + ")"
        case TypedBinOpExpr(op, l, r, rettype) =>  "(" + generate(l) + " " + (op match {case "==" => "==="
    	                                                                  case "!=" => "!=="
    	                                                                  case _ => op}) + " " + generate(r) + ")"

    	
    	case TypedBlockExpr(listofstatements, symbolTable, rettype) => "(function() { \n" + blockProcess(listofstatements) + " })()"
    	case TypedBoolExpr(value, valtype) => value.toString
    	case TypedIfThenExpr(predicate, expr, valtype) => "ifThen( " + 
		     thunkify(generate(predicate)) + ", " +
		     thunkify(generate(expr)) + ")"
		case TypedIfThenElseExpr(predicate, truevalue, falsevalue, valtype) => 
    		 "ifThenElse( " +
		     thunkify(generate(predicate)) + ", " +
    	   	 thunkify(generate(truevalue)) + ", " +
    		 thunkify(generate(falsevalue)) + " )"     
    	case TypedFunExpr(name, args, rettype) => generate(name) + "(" + commaSeparatedProcess(args) + ")"
    	case TypedNumExpr(wrappedval, valtype) => wrappedval match{case NInt(value) => value.toString
    	                                                           case NDouble(value) => value.toString}
    	case TypedStringExpr(value, valtype) => "\"" + (value.map(escapify).mkString) + "\""
    	case TypedCharExpr(value, valtype) => "\"" + escapify(value) + "\""
    	case TypedVarExpr(varName, valtype) => varName
    	//case TypedFieldSelectionExpr(selection) => 
    	
    	case TypedWhileExpr(predicate, body, doFlag, valtype) => {if (doFlag) "doLoop( " else "whileLoop( "} +
		     thunkify(generate(predicate)) + ", " + 
		     thunkify(generate(body)) + " )"
    	
    	case TypedFunDefStmt(name, args, retType, body, symbolTable) => "var " + name + " = function ( " + commaSeparatedProcess(args) + " )\n {" +
                                                                                   (if(retType == "Unit") generate(body) + "; return "
    	                                                                           else "return " + generate(body)) +
    	                                                                           "; \n }"
    	case TypedParamDclStmt(id, vartype) => id
    	case TypedValDefStmt(listofvaldecs, valtype, expr, valTypeFlag) => "var " + varProcess(listofvaldecs, expr) + 
                                                         " " + varProcessAux(listofvaldecs, expr)
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
	
	def commaSeparatedProcess(lost : List[TypedStmt]):String = lost match {
	  	case List() => ""
	  	case List(x) => generate(x)
	  	case x::xs => generate(x) + ", " + commaSeparatedProcess(xs)
	}
	def blockProcess(lost : List[TypedStmt]):String = lost match {
    	case List() => "return ;"
    	case List(x) => if (x.isExpr()) "return " + generate(x) + 
    	                                " ;" else throw new Exception("The last line in the block is a Stmt, expected an Expr")
    	case x::xs => generate(x) + "; \n" + blockProcess(xs)
	}
	def varProcess(los : List[String], expr : TypedExpr):String = los match{
    	case Nil => ""
    	case x::Nil => x + " ; \n"
    	case x::xs => x + " , " + varProcess(xs, expr)
	} 
	
	def varProcessAux(los : List[String], expr : TypedExpr):String = los match{
  		case Nil => generate(expr)
  		case x::xs => x + " = " + varProcessAux(xs, expr)
	}
	
	def lookupOp(op : String, table : Map[String, String]):String = table.get(op) match{
	   case Some(operator) => operator
	   case None => op
	}
	
	//def classGen(name : Type, )
	def apply(source: TypedStmt, currentObj: String): String = generate (source)
}
