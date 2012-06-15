package compilerV0

object Parser extends SimpleParser[Expr] with JavaComments with CommonLiterals {
	def top = expr
  
	lazy val expr: P[Expr] =
	( iE7
	)
	
	lazy val expr1: P[Expr] = 
	( "if" ~ "(" ~ expr ~ ")" ~ expr ^^ 
	      {case _ ~ _ ~ test~ _ ~ trueClause => IfThenStmt(test, trueClause)}
	| "if" ~ "(" ~ expr ~ ")" ~ expr ~ "else" ~ expr ^^
		{case _ ~ _ ~ test~ _ ~ trueClause ~ _ ~ falseClause => IfThenElseStmt(test, trueClause, falseClause)}
	| "while" ~ "(" ~ expr ~ ")" ~ expr ^^
		{case _ ~ _ ~ test~ _ ~ body=> WhileStmt(test, body)}
	| postfixExpr
	)
	
	lazy val postfixExpr: P[Expr] = 
	( infixExpr
	)
	
	lazy val infixExpr: P[Expr] =
	( iE7
	)
	
	lazy val iE7: P[Expr] =
	( iE8 ~ iE7Rest ^^
	    {case base ~ rest => 
	      	if (rest.size == 0) {
	      		base;
	      	} else if (rest.head.isInstanceOf[LeftOpPair]) {
	      		buildLeft(base, rest.asInstanceOf[List[LeftOpPair]])
	      	} else {
	      		buildRight(base, rest.asInstanceOf[List[RightOpPair]])
	      	}
	    }
	)
	lazy val iE7Rest: P[List[OpPair]] = 
	( op7R ~ iE8 ~ iE7R ^^
	    {case op ~ left ~ right => RightOpPair(op, left) :: right}
	| op7 ~ iE8 ~ iE7L ^^
		{case op ~ left ~ right => LeftOpPair(op, left) :: right}
	| "" ^^ {case _ => List[OpPair]()}
	)
	lazy val iE7R: P[List[RightOpPair]] =
	( op7R ~ iE8 ~ iE7R ^^
	    {case op ~ left ~ right => RightOpPair(op, left) :: right}
	| "" ^^ {case _ => List[RightOpPair]()}
	)
	lazy val iE7L: P[List[LeftOpPair]] =
	( op7 ~ iE8 ~ iE7L ^^
	    {case op ~ left ~ right => LeftOpPair(op, left) :: right}
	| "" ^^ {case _ => List[LeftOpPair]()}
	)
	
	lazy val iE8: P[Expr] =
	( iE9 ~ iE8Rest ^^
	    {case base ~ rest => 
	      	if (rest.size == 0) {
	      		base;
	      	} else if (rest.head.isInstanceOf[LeftOpPair]) {
	      		buildLeft(base, rest.asInstanceOf[List[LeftOpPair]])
	      	} else {
	      		buildRight(base, rest.asInstanceOf[List[RightOpPair]])
	      	}
	    }
	)
	lazy val iE8Rest: P[List[OpPair]] = 
	( op8R ~ iE9 ~ iE8R ^^
	    {case op ~ left ~ right => RightOpPair(op, left) :: right}
	| op8 ~ iE9 ~ iE8L ^^
		{case op ~ left ~ right => LeftOpPair(op, left) :: right}
	| "" ^^ {case _ => List[OpPair]()}
	)
	lazy val iE8R: P[List[RightOpPair]] =
	( op8R ~ iE9 ~ iE8R ^^
	    {case op ~ left ~ right => RightOpPair(op, left) :: right}
	| "" ^^ {case _ => List[RightOpPair]()}
	)
	lazy val iE8L: P[List[LeftOpPair]] =
	( op8 ~ iE9 ~ iE8L ^^
	    {case op ~ left ~ right => LeftOpPair(op, left) :: right}
	| "" ^^ {case _ => List[LeftOpPair]()}
	)
	
	lazy val iE9: P[Expr] =
	( prefixExpr
	)
	
	lazy val prefixExpr: P[Expr] = 
	( "-" ~ simpleExpr ^^ 
		{case op ~ expr => UnOpExpr(op, expr)}
	| simpleExpr
	)
	
	lazy val simpleExpr: P[Expr] = 
	( floatingPointNumber ^^
		{case numLit => NumExpr(Ndouble(numLit.toDouble))}
	| wholeNumber ^^
	  	{case numLit => NumExpr(Nint(numLit.toInt)); }
	)
	
  lazy val op7: P[String] = "+" | "-"
  lazy val op7R: P[String] = "+:" | "-:"
  
  lazy val op8: P[String] = "*" | "/" | "%"
  lazy val op8R: P[String] = "*:" | "/:" | "%:"
  
  def buildLeft(base : Expr, rest : List[LeftOpPair]) : Expr =
	rest match {
        case Nil => base
        case LeftOpPair(op, expr) :: tail => buildLeft(BinOpExpr(op, base, expr), tail)
    }
  def buildRight(base : Expr, rest : List[RightOpPair]) : Expr = 
	rest match {
        case Nil => base
        case RightOpPair(op, expr) :: tail => buildRight(BinOpExpr(op, expr, base), tail)
    }
}
