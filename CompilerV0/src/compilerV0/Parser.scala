package compilerV0

object Parser extends SimpleParser[Expr] with JavaComments with CommonLiterals {
	def top = expr
  
	lazy val expr: P[Expr] =
	( expr1
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
	( iE8
	| iE7L ~ op7 ~ iE8 ^^
		{case left ~ op ~ right => println(op+" matched"); BinOpExpr(op, left, right)}
	| iE8 ~ op7 ~ ":" ~ iE7R ^^
		{case left ~ op ~ _ ~ right => BinOpExpr(op, right, left)}
	)
	lazy val iE7L: P[Expr] =
	( iE8 
	| iE7L ~ op7 ~ iE8 ^^
		{case left ~ op  ~ right => BinOpExpr(op, left, right)}
	)
	lazy val iE7R: P[Expr] =
	( iE8
	| iE8 ~ op7 ~ ":" ~ iE7R ^^
		{case left ~ op ~ _ ~ right => BinOpExpr(op, right, left)}
	)
	
	lazy val iE8: P[Expr] =
	( iE9
	| iE8L ~ op8 ~ iE9 ^^
		{case left ~ op ~ right => println(op+" matched"); BinOpExpr(op, left, right)}
	| iE9 ~ op8 ~ ":" ~ iE8R ^^
		{case left ~ op ~ _ ~ right => BinOpExpr(op, right, left)}
	)
	lazy val iE8L: P[Expr] =
	( iE9 
	| iE8L ~ op8 ~ iE9 ^^
		{case left ~ op ~ right => BinOpExpr(op, left, right)}
	)
	lazy val iE8R: P[Expr] =
	( iE9
	| iE9 ~ op8 ~ ":" ~ iE8R ^^
		{case left ~ op ~ _ ~ right => BinOpExpr(op, right, left)}
	)
	
	lazy val iE9: P[Expr] = 
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
	  	{case numLit => println("Int matched"); NumExpr(Nint(numLit.toInt)); }
	)

  lazy val op7: P[String] = "+" | "-"

  lazy val op8: P[String] = "*" | "/" | "%"
}
