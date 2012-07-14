package compilerV0

object CodeGenerator {
	def generate (ast : Stmt):String = ast match {
    	case NumExpr(wrappedval) => wrappedval match{case NInt(value) => value.toString
    	                                             case NDouble(value) => value.toString}
    	case BoolExpr(value) => value.toString
    	
    	case BinOpExpr(op, l, r) =>  "(" + generate(l) + " " + (op match {case "==" => "==="
    	                                                                  case "!=" => "!=="
    	                                                                  case _ => op}) + " " + generate(r) + ")"
	    
    	case ValDefStmt(listofvaldecs, valtype, expr) => "var " + varProcess(listofvaldecs, expr) + 
                                                         " " + varProcessAux(listofvaldecs, expr)
    	case VarDefStmt(listofvardecs, valtype, expr) => "var " + varProcess(listofvardecs, expr) + 
                                                         " " + varProcessAux(listofvardecs, expr)
    	case VarExpr(varName) => varName
    	case IfThenExpr(predicate, expr) => "ifThen( " + 
		     thunkify(generate(predicate)) + ", " +
		     thunkify(generate(expr)) + ")"
    	case IfThenElseExpr(predicate, truevalue, falsevalue) => 
    		 "ifThenElse( " +
		     thunkify(generate(predicate)) + ", " +
    	   	 thunkify(generate(truevalue)) + ", " +
    		 thunkify(generate(falsevalue)) + " )"
    	case WhileExpr(predicate, body) => "whileLoop( " +
		     thunkify(generate(predicate)) + ", " + 
		     thunkify(generate(body)) + " )"
    	case BlockExpr(listofstatements) => "(function() { \n" + blockProcess(listofstatements) + " })()"
    	case StringExpr(value) => "\"" + value + "\""
    	case FunDefStmt(name, args, retType, body) => "var " + name + " = function ( " + commaSeparatedProcess(args) + " )\n {" +
                                                                                   (if(retType == "Unit") generate(body) + "; return "
    	                                                                           else "return " + generate(body)) +
    	                                                                           "; \n }"
    	case ParamDclStmt(id, vartype) => id
    	case FunExpr(name, args) => generate(name) + "(" + commaSeparatedProcess(args) + ")"
    	case AnonFuncExpr(args, body) => "(function (" + commaSeparatedProcess(args) + " ) { return " + generate(body) + " }) "                                               
    	case _ => throw new Exception("No match found for pattern")
	}
	def thunkify(code: String): String = "(function() {\n return " + code + "})"
	
	def commaSeparatedProcess(lost : List[Stmt]):String = lost match {
	  	case List() => ""
	  	case List(x) => generate(x)
	  	case x::xs => generate(x) + ", " + commaSeparatedProcess(xs)
<<<<<<< HEAD
    }
	def exprsProcess(loe : List[Expr]):String = loe match {
	  	case Nil => ""
	  	case x::Nil => generate(x)
	  	case x::xs => generate(x) + ", " + exprsProcess(xs)
=======
>>>>>>> 3442ec05f968175d390e31cad289c51aa0d2d0e9
	}
	def blockProcess(lost : List[Stmt]):String = lost match {
    	case List() => "return ;"
    	case List(x) => if (x.isExpr()) "return " + generate(x) + 
    	                                " ;" else throw new Exception("The last line in the block is a Stmt, expected an Expr")
    	case x::xs => generate(x) + "; \n" + blockProcess(xs)
	}
	def varProcess(los : List[String], expr : Expr):String = los match{
    	case Nil => ""
    	case x::Nil => x + " ; \n"
    	case x::xs => x + " , " + varProcess(xs, expr)
	} 
	def varProcessAux(los : List[String], expr : Expr):String = los match{
  		case Nil => generate(expr)
  		case x::xs => x + " = " + varProcessAux(xs, expr)
	}
	def apply(source: Expr): String = generate (source)
}
