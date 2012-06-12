package compilerV0

object Parser extends SimpleParser[Expr] with JavaComments with CommonLiterals {
  def top = expr
  
  lazy val expr: P[Expr] =
  ( stmt
  | expr ~ "||" ~ expr ^^ 
  	  {case l ~ _ ~ r => BinOpExpr("||", l, r)}
  | expr ~ "&&" ~ expr ^^ 
  	  {case l ~ _ ~ r => BinOpExpr("&&", l, r)}
  | expr ~ compareop ~ expr ^^ 
  	  {case l ~ op ~ r => BinOpExpr(op, l, r)}
  | expr ~ equalityop ~ expr ^^ 
  	  {case l ~ op ~ r => BinOpExpr(op, l, r)}
  |	expr ~ addop ~ term ^^
      {case e ~ op ~ t => BinOpExpr(op, e, t)}
  | term
  )

  lazy val term: P[Expr] =
  ( term ~ mulop ~ factor ^^
      {case t ~ op ~ f => BinOpExpr(op, t, f)}
  | factor
  )

  lazy val factor: P[Expr] =
  ( "-" ~ factor ^^
		{case op ~ f => UnOpExpr(op, f)}
  | "(" ~ expr ~ ")" ^^
  		{case _ ~ e ~ _ => e}
  | "{" ~ rep1(expr) ~ "}" ^^ 
		{case _ ~ body ~ _ => BlockExpr(body)}
  | floatingPointNumber ^^ 
  		{case numLit => NumExpr(Ndouble(numLit.toDouble))}
  | wholeNumber ^^
  		{case numLit => NumExpr(Nint(numLit.toInt))}
  | ("true" | "false") ^^
  		{case boolLit => BoolExpr(boolLit.toBoolean)}
  )
  
  lazy val stmt: P[Expr] =
  ( "if" ~ "(" ~ expr ~ ")" ~ expr ~ "else" ~ expr ^^
      {case _ ~ _ ~ test ~ _ ~ trueClause ~ _ ~ falseClause => IfThenElseStmt(test, trueClause, falseClause)}
  | "if" ~ "(" ~ expr ~ ")" ~ expr ^^
      {case _ ~ _ ~ test ~ _ ~ trueClause => IfThenStmt(test, trueClause)}
  | "while" ~ "(" ~ expr ~ ")" ~ expr ^^
      {case _ ~ _ ~ test ~ _ ~ body => WhileStmt(test, body)}
  )

  lazy val addop: P[String] = "+" | "-"

  lazy val mulop: P[String] = "*" | "/" | "%"
  
  lazy val compareop : P[String] = ">=" | "<=" | ">" | "<"
  
  lazy val equalityop : P[String] = "==" | "!="
}
