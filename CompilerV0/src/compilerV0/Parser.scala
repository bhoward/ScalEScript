package compilerV0

object Parser extends SimpleParser[Expr] with JavaComments with CommonLiterals {
	def top = expr
  
	lazy val expr: P[Expr] =
	( simpleExpr
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
	    {left ~ rest => }
	)
	lazy val iE7Rest: P[Expr] = 
	( op7R ~ iE8 ~ iE7R ^^
	    {op ~ left ~ right => }
	| op7 iE8 ~ iE7L ^^
		{op ~ left ~ right => }
	|   
	)
	lazy val iE7R: P[Expr] =
	( op7R ~ iE8 ~ iE7R ^^
	    {op ~ left ~ right => }
	|   
	)
	lazy val ie7L: P[Expr] =
	( op7 ~ iE8 ~ iE7L ^^
	    {op ~ left ~ right => }
	|
	)
	
	lazy val iE8: P[Expr] =
	( prefixExpr
	)
	
	lazy val prefixExpr: P[Expr] = 
	( "-" ~ simpleExpr ^^ 
		{case op ~ expr => println("Negative matched"); UnOpExpr(op, expr)}
	| simpleExpr
	)
	
	lazy val simpleExpr: P[Expr] = 
	( floatingPointNumber ^^
		{case numLit => println("Double matched"); NumExpr(Ndouble(numLit.toDouble))}
	| wholeNumber ^^
	  	{case numLit => println("Int matched: "+numLit); NumExpr(Nint(numLit.toInt)); }
	)
	
  lazy val op7: P[String] = "+" | "-"
  lazy val op7R: P[String] = "+:" | "-:"

  lazy val op8: P[String] = "*" | "/" | "%"
}
