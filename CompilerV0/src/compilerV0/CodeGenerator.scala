package compilerV0

object CodeGenerator {
	def generate (ast : Stmt):String = ast match {
    	case NumExpr(wrappedval) => wrappedval match{case NInt(value) => value.toString
    	                                             case NDouble(value) => value.toString}
    	case BoolExpr(value) => value.toString
    	case BinOpExpr(op, l, r) =>  "(" + generate(l) + " " + (if(op == "==") "===" else op) + " " + generate(r) + ")"
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
    	case StringExpr(value) => value
    	case FunDefStmt(name, args, retType, body) => "var " + name + " = function ( " + funArgProcess(args) + " )\n {" +
                                                       (if(retType == "Unit") generate(body) + "; return "
    	                                               else "return " + generate(body)) +
    	                                               "; \n }"
    	case VarDclStmt(listofIdentifier, vartype) => listofIdentifier.foldLeft("")((acc, str) => acc + str) // TODO ???
    	case FunExpr(name, args) => generate(name) + "(" + exprsProcess(args) + ")"
    	case AnonFuncExpr(args, body) => "(function (" + funArgProcess(args) + " ) { return " + generate(body) + " }) "
    	case _ => "failure" // TODO Throw an exception?
	}
	def thunkify(code: String): String = "(function() {\n return " + code + "})";
	def exprsProcess(loe : List[Expr]):String = loe match {
	  	case Nil => ""
	  	case x::Nil => generate(x)
	  	case x::xs => generate(x) + ", " + exprsProcess(xs)
	}
	def blockProcess(loe : List[Stmt]):String = loe match {
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
	def funArgProcess(lov : List[VarDclStmt]):String = lov match{
	  case List() => ""
	  case List(x) => generate(x)
	  case x::xs => generate(x) + ", " + funArgProcess(xs)
	}
	def apply(source: Expr): String = generate (source)
}
