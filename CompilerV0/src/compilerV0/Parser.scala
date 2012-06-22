package compilerV0

object Parser extends SimpleParser[Expr] with JavaComments with CommonLiterals {
	def top = expr
  
	lazy val expr: P[Expr] =
	( expr1
	)
	
	lazy val expr1: P[Expr] = 
	( "if" ~ "(" ~ expr ~ ")" ~ expr ~ "else" ~ expr ^^
		{case _ ~ _ ~ test ~ _ ~ trueClause ~ _ ~ falseClause => IfThenElseExpr(test, trueClause, falseClause)}
	| "if" ~ "(" ~ expr ~ ")" ~ expr ^^ 
	      {case _ ~ _ ~ test ~ _ ~ trueClause => IfThenExpr(test, trueClause)} 
	| "while" ~ "(" ~ expr ~ ")" ~ expr ^^
		{case _ ~ _ ~ test ~ _ ~ body=> WhileExpr(test, body)}
	| postfixExpr
	)
	
	lazy val postfixExpr: P[Expr] = 
	( infixExpr
	)
	
	lazy val infixExpr: P[Expr] =
	( iE0
	)
	
	//Start of infix expression precedence groups
	//iE0 through iE9 are not defined in the scala grammar. 
	//These are used to enforce precedence of infix operators in the parsing process.
    //iE# = infix expression of precedence group #, iE#Rest = helper for iE#, 
	//iE#L = left associative operator of an infix expression of precedence group #, iE#R = same as iE#L, but right associative
	lazy val iE0: P[Expr] =
	( iE1
	)
	
	lazy val iE1: P[Expr] =
	( iE2 ~ iE1Rest ^^
	    {case base ~ rest => 
	      	if (rest.size == 0) {
	      		base;
	      	} else if (rest.head.isLeft()) {
	      		buildLeft(base, rest)
	      	} else {
	      		buildRight(base, rest)
	      	}
	    }
	)
	lazy val iE1Rest: P[List[OpPair]] = 
	( op1R ~ iE2 ~ iE1R ^^
	    {case op ~ left ~ right => RightOpPair(op, left) :: right}
	| op1 ~ iE2 ~ iE1L ^^
		{case op ~ left ~ right => LeftOpPair(op, left) :: right}
	| "" ^^ {case _ => List[OpPair]()}
	)
	lazy val iE1R: P[List[RightOpPair]] =
	( op1R ~ iE2 ~ iE1R ^^
	    {case op ~ left ~ right => RightOpPair(op, left) :: right}
	| "" ^^ {case _ => List[RightOpPair]()}
	)
	lazy val iE1L: P[List[LeftOpPair]] =
	( op1 ~ iE2 ~ iE1L ^^
	    {case op ~ left ~ right => LeftOpPair(op, left) :: right}
	| "" ^^ {case _ => List[LeftOpPair]()}
	)
	
	lazy val iE2: P[Expr] =
	( iE3
	)
	
	lazy val iE3: P[Expr] =
	( iE4 ~ iE3Rest ^^
	    {case base ~ rest => 
	      	if (rest.size == 0) {
	      		base;
	      	} else if (rest.head.isLeft()) {
	      		buildLeft(base, rest)
	      	} else {
	      		buildRight(base, rest)
	      	}
	    }
	)
	lazy val iE3Rest: P[List[OpPair]] = 
	( op3R ~ iE4 ~ iE3R ^^
	    {case op ~ left ~ right => RightOpPair(op, left) :: right}
	| op3 ~ iE4 ~ iE3L ^^
		{case op ~ left ~ right => LeftOpPair(op, left) :: right}
	| "" ^^ {case _ => List[OpPair]()}
	)
	lazy val iE3R: P[List[RightOpPair]] =
	( op3R ~ iE4 ~ iE3R ^^
	    {case op ~ left ~ right => RightOpPair(op, left) :: right}
	| "" ^^ {case _ => List[RightOpPair]()}
	)
	lazy val iE3L: P[List[LeftOpPair]] =
	( op3 ~ iE4 ~ iE3L ^^
	    {case op ~ left ~ right => LeftOpPair(op, left) :: right}
	| "" ^^ {case _ => List[LeftOpPair]()}
	)
	
	lazy val iE4: P[Expr] =
	( iE5 ~ iE4Rest ^^
	    {case base ~ rest => 
	      	if (rest.size == 0) {
	      		base;
	      	} else if (rest.head.isLeft()) {
	      		buildLeft(base, rest)
	      	} else {
	      		buildRight(base, rest)
	      	}
	    }
	)
	lazy val iE4Rest: P[List[OpPair]] = 
	( op4R ~ iE5 ~ iE4R ^^
	    {case op ~ left ~ right => RightOpPair(op, left) :: right}
	| op4 ~ iE5 ~ iE4L ^^
		{case op ~ left ~ right => LeftOpPair(op, left) :: right}
	| "" ^^ {case _ => List[OpPair]()}
	)
	lazy val iE4R: P[List[RightOpPair]] =
	( op4R ~ iE5 ~ iE4R ^^
	    {case op ~ left ~ right => RightOpPair(op, left) :: right}
	| "" ^^ {case _ => List[RightOpPair]()}
	)
	lazy val iE4L: P[List[LeftOpPair]] =
	( op4 ~ iE5 ~ iE4L ^^
	    {case op ~ left ~ right => LeftOpPair(op, left) :: right}
	| "" ^^ {case _ => List[LeftOpPair]()}
	)
	
	lazy val iE5: P[Expr] =
	( iE6 ~ iE5Rest ^^
	    {case base ~ rest => 
	      	if (rest.size == 0) {
	      		base;
	      	} else if (rest.head.isLeft()) {
	      		buildLeft(base, rest)
	      	} else {
	      		buildRight(base, rest)
	      	}
	    }
	)
	lazy val iE5Rest: P[List[OpPair]] = 
	( op5R ~ iE6 ~ iE5R ^^
	    {case op ~ left ~ right => RightOpPair(op, left) :: right}
	| op5 ~ iE6 ~ iE5L ^^
		{case op ~ left ~ right => LeftOpPair(op, left) :: right}
	| "" ^^ {case _ => List[OpPair]()}
	)
	lazy val iE5R: P[List[RightOpPair]] =
	( op5R ~ iE6 ~ iE5R ^^
	    {case op ~ left ~ right => RightOpPair(op, left) :: right}
	| "" ^^ {case _ => List[RightOpPair]()}
	)
	lazy val iE5L: P[List[LeftOpPair]] =
	( op5 ~ iE6 ~ iE5L ^^
	    {case op ~ left ~ right => LeftOpPair(op, left) :: right}
	| "" ^^ {case _ => List[LeftOpPair]()}
	)
	
	lazy val iE6: P[Expr] =
	( iE7
	)
	
	lazy val iE7: P[Expr] =
	( iE8 ~ iE7Rest ^^
	    {case base ~ rest => 
	      	if (rest.size == 0) {
	      		base;
	      	} else if (rest.head.isLeft()) {
	      		buildLeft(base, rest)
	      	} else {
	      		buildRight(base, rest)
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
	      	} else if (rest.head.isLeft()) {
	      		buildLeft(base, rest)
	      	} else {
	      		buildRight(base, rest)
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
	//End of infix expression precedence groups
	
	lazy val prefixExpr: P[Expr] = 
	( "-" ~ simpleExpr ^^ 
		{case op ~ expr => UnOpExpr(op, expr)}
	| simpleExpr
	)
	
	lazy val simpleExpr: P[Expr] =
	( blockExpr
	| simpleExpr1
	)
	
	lazy val blockExpr: P[Expr] =
	( "{" ~ block ~ "}" ^^
		{case _ ~ stmt ~ _ => BlockExpr(stmt)}
	)
	
	lazy val block: P[List[Stmt]] =
	( rep(block1) ~ expr ^^
		{case stmt ~ expr  => stmt ++ List[Expr](expr)}
	| rep(block1) ^^
		{case stmt => stmt}
	)
	
	//Not defined in scala grammar. Used to match multiple blockStats seperated by a semicolon
	lazy val block1: P[Stmt] =
	( blockStat ~ ";" ^^
	    {case stmt ~ _ => stmt}
	)
	
	lazy val blockStat: P[Stmt] = 
	( defG
	| expr1
    )
    
    //Called def in the grammar (which is a reserved word, so I used defG)
    lazy val defG: P[Stmt] =
    ( patVarDef
    )
	
    lazy val patVarDef: P[Stmt] =
    ( "val" ~ patDef ^^
    	{case _ ~ DefStmt(pats, typeG, expr) => ValDefStmt(pats, typeG, expr)}
    | "var" ~ varDef ^^
        {case _ ~ DefStmt(pats, typeG, expr) => VarDefStmt(pats, typeG, expr)}
    )
    
    lazy val varDef: P[DefStmt] =
    ( patDef
    )
    
    lazy val patDef: P[DefStmt] =
    ( pattern2 ~ rep(pattern2s) ~ ":" ~ typeG ~ "=" ~ expr ^^
        {case pat ~ pats ~ _ ~ typeG ~ _ ~ expr => DefStmt(pat :: pats, typeG, expr)}
    )
    
    lazy val pattern2: P[String] =
    ( varid
    | pattern3
    )
    
    //Not defined in scala grammar. Used to match multiple ids seperated by a comma
    lazy val pattern2s: P[String] =
    ( "," ~ pattern2 ^^
        {case _ ~ pat => pat}
    )
    
    lazy val pattern3: P[String] =
    ( simplePattern
    )
    
    lazy val simplePattern: P[String] =
    ( qualid
    )
    
    lazy val qualid: P[String] =
    ( id
    )
    
    //Called type in the grammar (which is a reserved word, so I used typeG)
    lazy val typeG: P[String] =
    ( infixType
    )
    
    lazy val infixType: P[String] =
    ( simpleType
    )
    
    lazy val simpleType: P[String] =
    ( qualId 
    )
    
    lazy val qualId: P[String] = 
    ( id
    )
    
    lazy val id: P[String] =
    ( plainid    
    )
    
    lazy val plainid: P[String] =
    ( upperid
    | varid
    )
    
    def upperid: Parser[String] = ("""[A-Z][A-Za-z0-9]*""").r
    def varid: Parser[String] = ("""[a-z][A-Za-z0-9]*""").r
    
    //Put println here for the moment, it should just be implemented in to a function when they are added
	lazy val simpleExpr1: P[Expr] = 
	( "println" ~ "(" ~ expr ~ ")" ^^ 
	    {case _ ~ _ ~ expr ~ _ => PrintlnExpr(expr)}
	| "print" ~ "(" ~ expr ~ ")" ^^ 
	    {case _ ~ _ ~ expr ~ _ => PrintExpr(expr)}
	| ("true" | "false") ^^
	    {case boolLit => BoolExpr(boolLit.toBoolean)}
	| floatingPointNumber ^^
		{case numLit => NumExpr(Ndouble(numLit.toDouble))}
	| wholeNumber ^^
	  	{case numLit => NumExpr(Nint(numLit.toInt)); }
	| stringLiteral ^^
		{case strLit => StringExpr(strLit)}
	| path
	)
	
	lazy val path: P[Expr] = 
	( qualId ^^
	    {case id => VarExpr(id)}
	)
	
	lazy val op1: P[String] = "||" | "||"
	lazy val op1R: P[String] = "||:" | "||:"
	
	lazy val op3: P[String] = "&&" | "&&"
	lazy val op3R: P[String] = "&&:" | "&&:"
	
	lazy val op4: P[String] = "==" | "!="
	lazy val op4R: P[String] = "==:" | "!=:"
	
	lazy val op5: P[String] = ">=" | "<=" | ">" | "<"
	lazy val op5R: P[String] = ">=:" | "<=:" | ">:" | "<:"
	
	lazy val op7: P[String] = "+" | "-"
	lazy val op7R: P[String] = "+:" | "-:"
  
	lazy val op8: P[String] = "*" | "/" | "%"
	lazy val op8R: P[String] = "*:" | "/:" | "%:"
  
	//Used to turn the list of left or right associative expressions in to nested expressions of the correct associativity
	//These Could be replaced with a foldL or foldR eventually
	def buildLeft(base : Expr, rest : List[OpPair]) : Expr =
		rest match {
			case Nil => base
			case (opPair : OpPair) :: tail => buildLeft(BinOpExpr(opPair.getOp(), base, opPair.getExpr()), tail)
    	}
	def buildRight(base : Expr, rest : List[OpPair]) : Expr = 
		rest match {
        	case Nil => base
        	case (opPair : OpPair) :: tail => buildRight(BinOpExpr(opPair.getOp(), opPair.getExpr(), base), tail)
    	}
}
