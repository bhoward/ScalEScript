package compilerV0

import scala.util.parsing.combinator._
import java.io.Reader

object Parser extends RegexParsers with PackratParsers {
  type P[T] = PackratParser[T]

  // Treat comments as whitespace
  override val whiteSpace = """(\s|(//(?d:.)*\n)|(/\*(?s:.)*?\*/))*""".r

  //Should be templateStat eventually
  def top = templateStat;

  /* Start of grammar rules (roughly following the order from the scala spec */

  lazy val op1: Parser[String] = """\|[!#%&*+\-/:<=>?@\\^|~]*[!#%&*+\-/<>?@\\^|~]|\|""".r
  lazy val op1R: Parser[String] = """\|[!#%&*+\-/:<=>?@\\^|~]*:""".r

  lazy val op2: Parser[String] = """\^[!#%&*+\-/:<=>?@\\^|~]*[!#%&*+\-/<>?@\\^|~]|\^""".r
  lazy val op2R: Parser[String] = """\^[!#%&*+\-/:<=>?@\\^|~]*:""".r

  lazy val op3: Parser[String] = """&[!#%&*+\-/:<=>?@\\^|~]*[!#%&*+\-/<>?@\\^|~]|&""".r
  lazy val op3R: Parser[String] = """&[!#%&*+\-/:<=>?@\\^|~]*:""".r

  lazy val op4: Parser[String] = """=[!#%&*+\-/:<=>?@\\^|~]+>|=[!#%&*+\-/:<=>?@\\^|~]*[!#%&*+\-/<=?@\\^|~]|![!#%&*+\-/:<=>?@\\^|~]*[!#%&*+\-/<>?@\\^|~]|!=|!""".r
  lazy val op4R: Parser[String] = """[=!][!#%&*+\-/:<=>?@\\^|~]*:""".r

  lazy val op5: Parser[String] = """<[!#%&*+\-/:<=>?@\\^|~]+[%\-]|<[!#%&*+\-/:<=>?@\\^|~]*[!#&*+/<>?@\\^|~]|>[!#%&*+\-/:<=>?@\\^|~]*[!#%&*+\-/<>?@\\^|~]|<=|>=|<|>""".r
  lazy val op5R: Parser[String] = """[<>][!#%&*+\-/:<=>?@\\^|~]+:""".r

  lazy val op6: Parser[String] = """:[!#%&*+\-/:<=>?@\\^|~]*[!#%&*+\-/<>?@\\^|~]""".r
  lazy val op6R: Parser[String] = """:[!#%&*+\-/:<=>?@\\^|~]*:""".r

  lazy val op7: Parser[String] = """[+\-][!#%&*+\-/:<=>?@\\^|~]*[!#%&*+\-/<>?@\\^|~]|\+|-""".r
  lazy val op7R: Parser[String] = """[+\-][!#%&*+\-/:<=>?@\\^|~]*:""".r

  lazy val op8: Parser[String] = """[*\/%][!#%&*+\-/:<=>?@\\^|~]*[!#%&*+\-/<>?@\\^|~]|\*|/|%""".r
  lazy val op8R: Parser[String] = """[*\/%][!#%&*+\-/:<=>?@\\^|~]*:""".r

  lazy val op9: Parser[String] = """[#?@\\~][!#%&*+\-/:<=>?@\\^|~]*[!#%&*+\-/<>?@\\^|~]|\?|\\|~""".r
  lazy val op9R: Parser[String] = """[#?@\\~][!#%&*+\-/:<=>?@\\^|~]*:""".r

  // TODO need to remember to turn a op= b into a = a op b
  lazy val assignop: Parser[String] = """[#%&*+\-/:?@\\^|~][!#%&*+\-/:<=>?@\\^|~]*=|[!<>][!#%&*+\-/:<=>?@\\^|~]+=|=""".r

  def varid: Parser[String] = ("""[a-z][A-Za-z0-9$_]*""").r

  //This differs a bit from the scala grammar. I combined upper and idrest to make upperid
  lazy val plainid: P[String] =
    (upperid
      | varid // op // TODO
      )
  //Doesn't handle the _ op at this moment // TODO

  def upperid: Parser[String] = """[A-Z$][A-Za-z0-9$_]*|_[A-Za-z0-9$_]+""".r

  lazy val id: Parser[String] =
    (plainid)

  def integerLiteral: Parser[String] =
    """\d+""".r

  def floatingPointLiteral: Parser[String] =
    """(\d+\.(\d*)?|\d*\.\d+)([eE][+-]?\d+)?""".r

  def booleanLiteral: Parser[String] =
    """(true|false)""".r

  def characterLiteral: Parser[String] =
    ("\'" + """([^\p{Cntrl}\\]|\\[\\"'bfnrt])""" + "\'").r

  def stringLiteral: Parser[String] =
    ("\"" + """([^"\p{Cntrl}\\]|\\[\\"'bfnrt])*""" + "\"").r

  def multilineStringLiteral: Parser[String] =
    ("\"\"\"" + """("?"?[^"])*"*""" + "\"\"\"").r

  lazy val literal: P[Expr] =
    ("-" ~ floatingPointLiteral ^^
      { case _ ~ numLit => NumExpr(NDouble(-numLit.toDouble)); }
      | floatingPointLiteral ^^
      { case numLit => NumExpr(NDouble(numLit.toDouble)) }
      | "-" ~ integerLiteral ^^
      { case _ ~ numLit => NumExpr(NInt(-numLit.toInt)); }
      | integerLiteral ^^
      { case numLit => NumExpr(NInt(numLit.toInt)); }
      | booleanLiteral ^^
      { case boolLit => BoolExpr(boolLit.toBoolean) }
      | characterLiteral ^^
      { case charLit => CharExpr(deQuotify(charLit).charAt(0)) }
      | multilineStringLiteral ^^
      { case strLit => StringExpr(deTriquotify(strLit)) }
      | stringLiteral ^^
      { case strLit => StringExpr(deQuotify(strLit)) }
      | "null" ^^
      { case _ => null })

  lazy val qualId: P[String] =
    (id ~ "." ~ qualId ^^
      { case id ~ "." ~ qual => id+"."+qual }
      | id)

  lazy val ids: P[List[String]] =
    (id ~ "," ~ ids ^^
      { case id ~ _ ~ ids => id :: ids }
      | id ^^
      { case id => List(id) })

  lazy val path: P[String] =
    (qualId
      | "this")

  //Called type in the grammar (which is a reserved word, so I used typeG)
  lazy val typeG: P[Type] =
    (functionArgTypes ~ "=>" ~ typeG ^^
      { case argTypes ~ _ ~ resultType => FuncType(resultType, argTypes) }
      | infixType)

  lazy val functionArgTypes: P[List[Type]] =
    ("(" ~ ")" ^^
      { case _ ~ _ => Nil }
      | "(" ~ functionArgTypesH ~ ")" ^^
      { case _ ~ types ~ _ => types }
      | infixType ^^
      { case infixType => List(infixType) })
  lazy val functionArgTypesH: P[List[Type]] =
    (paramType ~ "," ~ functionArgTypesH ^^
      { case paramType ~ _ ~ moreTypes => paramType :: moreTypes }
      | paramType ^^
      { case paramType => List(paramType) })

  lazy val infixType: P[Type] =
    (simpleType ~ id ~ infixType ^^
      { case _ => null }
      | simpleType)

  lazy val simpleType: P[Type] =
    (simpleType ~ typeArgs ^^
      { case _ => null }
      | simpleType ~ "#" ~ id ^^
      { case _ => null }
      | qualId ^^
      { case qualId => BaseType(qualId) }
      | "(" ~ types ~ ")" ^^
      { case _ => null })

  lazy val typeArgs: P[List[String]] =
    ("[" ~ types ~ "]" ^^
      { case _ => null })

  lazy val types: P[List[String]] =
    (typeG ~ "," ~ types ^^
      { case _ => null }
      | typeG ^^
      { case _ => null })

  lazy val typePat: P[Type] =
    (typeG)

  lazy val expr: P[Expr] =
    (bindings ~ "=>" ~ expr ^^
      { case args ~ _ ~ body => AnonFuncExpr(args, body) }
      | id ~ "=>" ~ expr ^^
      { case _ => null }
      | "_" ~ "=>" ~ expr ^^
      { case _ => null }
      | expr1)

  lazy val expr1: P[Expr] =
    ("if" ~ "(" ~ expr ~ ")" ~ expr ~ ";" ~ "else" ~ expr ^^
      { case _ ~ _ ~ test ~ _ ~ trueClause ~ _ ~ _ ~ falseClause => IfThenElseExpr(test, trueClause, falseClause) }
      | "if" ~ "(" ~ expr ~ ")" ~ expr ~ "else" ~ expr ^^
      { case _ ~ _ ~ test ~ _ ~ trueClause ~ _ ~ falseClause => IfThenElseExpr(test, trueClause, falseClause) }
      | "if" ~ "(" ~ expr ~ ")" ~ expr ^^
      { case _ ~ _ ~ test ~ _ ~ trueClause => IfThenExpr(test, trueClause) }
      | "while" ~ "(" ~ expr ~ ")" ~ expr ^^
      { case _ ~ _ ~ test ~ _ ~ body => WhileExpr(test, body, false) }
      | "do" ~ expr ~ ";" ~ "while" ~ "(" ~ expr ~ ")" ^^
      { case _ ~ body ~ _ ~ _ ~ _ ~ test ~ _ => WhileExpr(test, body, true) }
      | "do" ~ expr ~ "while" ~ "(" ~ expr ~ ")" ^^
      { case _ ~ body ~ _ ~ _ ~ test ~ _ => WhileExpr(test, body, true) }
      | "for" ~ "(" ~ enumerators ~ ")" ~ expr ^^
      { case _ => null }
      | "for" ~ "{" ~ enumerators ~ "}" ~ "yield" ~ expr ^^
      { case _ => null }
      | "throw" ~ expr ^^
      { case _ => null }
      | "return" ~ expr ^^
      { case _ => null }
      | "return" ^^
      { case _ => null }
     
      //These following lines were commented out in order to allow expressions such as 'this.first = 5'.
      //When the parser is refactored to remove Packrat Parsers, this should be refactored too.
      /*
      | simpleExpr1 ~ "." ~ id ~ "=" ~ expr ^^
      { case _ => null }
      | id ~ "=" ~ expr ^^
      { case id ~ _ ~ e => AssignExpr(VarExpr(id), e) }
      */
      //Workaround to allow expressions such as 'this.first = 5'
      | simpleExpr1 ~ "=" ~ expr ^^
      { case p ~ _ ~ e => AssignExpr(p, e)}
      
      | simpleExpr1 ~ argumentExprs ~ "=" ~ expr ^^
      { case _ => null }
      | postfixExpr
      | postfixExpr ~ "match" ~ "{" ~ caseClauses ~ "}" ^^
      { case _ => null })

  lazy val postfixExpr: P[Expr] =
    /* Leaving this rule out right now. It is capturing the else keyword of an if-then-else (without semicolons)
	( infixExpr ~ id ^^
		{case _ => null} */
    (infixExpr)

  //Here is where the parser deviates from the scala grammar, because it handles operator precedence. Grammar resumes at prefixExpr 
  lazy val infixExpr: P[Expr] =
    (iE0)

  //Start of infix expression precedence groups
  //iE0 through iE9 are not defined in the scala grammar. 
  //These are used to enforce precedence of infix operators in the parsing process.
  //iE# = infix expression of precedence group #, iE#Rest = helper for iE#, 
  //iE#L = left associative operator of an infix expression of precedence group #, iE#R = same as iE#L, but right associative
  lazy val iE0: P[Expr] =
    (iE1)

  lazy val iE1: P[Expr] =
    (iE2 ~ iE1Rest ^^
      {
        case base ~ rest =>
          if (rest.size == 0) {
            base;
          } else if (rest.head.isLeft()) {
            buildLeft(base, rest)
          } else {
            buildRight(base, rest)
          }
      })
  lazy val iE1Rest: P[List[OpPair]] =
    (op1R ~ iE2 ~ iE1R ^^
      { case op ~ left ~ right => RightOpPair(op, left) :: right }
      | op1 ~ iE2 ~ iE1L ^^
      { case op ~ left ~ right => LeftOpPair(op, left) :: right }
      | "" ^^ { case _ => List[OpPair]() })
  lazy val iE1R: P[List[RightOpPair]] =
    (op1R ~ iE2 ~ iE1R ^^
      { case op ~ left ~ right => RightOpPair(op, left) :: right }
      | "" ^^ { case _ => List[RightOpPair]() })
  lazy val iE1L: P[List[LeftOpPair]] =
    (op1 ~ iE2 ~ iE1L ^^
      { case op ~ left ~ right => LeftOpPair(op, left) :: right }
      | "" ^^ { case _ => List[LeftOpPair]() })

  lazy val iE2: P[Expr] =
    (iE3 ~ iE2Rest ^^
      {
        case base ~ rest =>
          if (rest.size == 0) {
            base;
          } else if (rest.head.isLeft()) {
            buildLeft(base, rest)
          } else {
            buildRight(base, rest)
          }
      })
  lazy val iE2Rest: P[List[OpPair]] =
    (op2R ~ iE3 ~ iE2R ^^
      { case op ~ left ~ right => RightOpPair(op, left) :: right }
      | op2 ~ iE3 ~ iE2L ^^
      { case op ~ left ~ right => LeftOpPair(op, left) :: right }
      | "" ^^ { case _ => List[OpPair]() })
  lazy val iE2R: P[List[RightOpPair]] =
    (op2R ~ iE3 ~ iE2R ^^
      { case op ~ left ~ right => RightOpPair(op, left) :: right }
      | "" ^^ { case _ => List[RightOpPair]() })
  lazy val iE2L: P[List[LeftOpPair]] =
    (op2 ~ iE3 ~ iE2L ^^
      { case op ~ left ~ right => LeftOpPair(op, left) :: right }
      | "" ^^ { case _ => List[LeftOpPair]() })

  lazy val iE3: P[Expr] =
    (iE4 ~ iE3Rest ^^
      {
        case base ~ rest =>
          if (rest.size == 0) {
            base;
          } else if (rest.head.isLeft()) {
            buildLeft(base, rest)
          } else {
            buildRight(base, rest)
          }
      })
  lazy val iE3Rest: P[List[OpPair]] =
    (op3R ~ iE4 ~ iE3R ^^
      { case op ~ left ~ right => RightOpPair(op, left) :: right }
      | op3 ~ iE4 ~ iE3L ^^
      { case op ~ left ~ right => LeftOpPair(op, left) :: right }
      | "" ^^ { case _ => List[OpPair]() })
  lazy val iE3R: P[List[RightOpPair]] =
    (op3R ~ iE4 ~ iE3R ^^
      { case op ~ left ~ right => RightOpPair(op, left) :: right }
      | "" ^^ { case _ => List[RightOpPair]() })
  lazy val iE3L: P[List[LeftOpPair]] =
    (op3 ~ iE4 ~ iE3L ^^
      { case op ~ left ~ right => LeftOpPair(op, left) :: right }
      | "" ^^ { case _ => List[LeftOpPair]() })

  lazy val iE4: P[Expr] =
    (iE5 ~ iE4Rest ^^
      {
        case base ~ rest =>
          if (rest.size == 0) {
            base;
          } else if (rest.head.isLeft()) {
            buildLeft(base, rest)
          } else {
            buildRight(base, rest)
          }
      })
  lazy val iE4Rest: P[List[OpPair]] =
    (op4R ~ iE5 ~ iE4R ^^
      { case op ~ left ~ right => RightOpPair(op, left) :: right }
      | op4 ~ iE5 ~ iE4L ^^
      { case op ~ left ~ right => LeftOpPair(op, left) :: right }
      | "" ^^ { case _ => List[OpPair]() })
  lazy val iE4R: P[List[RightOpPair]] =
    (op4R ~ iE5 ~ iE4R ^^
      { case op ~ left ~ right => RightOpPair(op, left) :: right }
      | "" ^^ { case _ => List[RightOpPair]() })
  lazy val iE4L: P[List[LeftOpPair]] =
    (op4 ~ iE5 ~ iE4L ^^
      { case op ~ left ~ right => LeftOpPair(op, left) :: right }
      | "" ^^ { case _ => List[LeftOpPair]() })

  lazy val iE5: P[Expr] =
    (iE6 ~ iE5Rest ^^
      {
        case base ~ rest =>
          if (rest.size == 0) {
            base;
          } else if (rest.head.isLeft()) {
            buildLeft(base, rest)
          } else {
            buildRight(base, rest)
          }
      })
  lazy val iE5Rest: P[List[OpPair]] =
    (op5R ~ iE6 ~ iE5R ^^
      { case op ~ left ~ right => RightOpPair(op, left) :: right }
      | op5 ~ iE6 ~ iE5L ^^
      { case op ~ left ~ right => LeftOpPair(op, left) :: right }
      | "" ^^ { case _ => List[OpPair]() })
  lazy val iE5R: P[List[RightOpPair]] =
    (op5R ~ iE6 ~ iE5R ^^
      { case op ~ left ~ right => RightOpPair(op, left) :: right }
      | "" ^^ { case _ => List[RightOpPair]() })
  lazy val iE5L: P[List[LeftOpPair]] =
    (op5 ~ iE6 ~ iE5L ^^
      { case op ~ left ~ right => LeftOpPair(op, left) :: right }
      | "" ^^ { case _ => List[LeftOpPair]() })

  lazy val iE6: P[Expr] =
    (iE7 ~ iE6Rest ^^
      {
        case base ~ rest =>
          if (rest.size == 0) {
            base;
          } else if (rest.head.isLeft()) {
            buildLeft(base, rest)
          } else {
            buildRight(base, rest)
          }
      })
  lazy val iE6Rest: P[List[OpPair]] =
    (op6R ~ iE7 ~ iE6R ^^
      { case op ~ left ~ right => RightOpPair(op, left) :: right }
      | op6 ~ iE7 ~ iE6L ^^
      { case op ~ left ~ right => LeftOpPair(op, left) :: right }
      | "" ^^ { case _ => List[OpPair]() })
  lazy val iE6R: P[List[RightOpPair]] =
    (op6R ~ iE7 ~ iE6R ^^
      { case op ~ left ~ right => RightOpPair(op, left) :: right }
      | "" ^^ { case _ => List[RightOpPair]() })
  lazy val iE6L: P[List[LeftOpPair]] =
    (op6 ~ iE7 ~ iE6L ^^
      { case op ~ left ~ right => LeftOpPair(op, left) :: right }
      | "" ^^ { case _ => List[LeftOpPair]() })

  lazy val iE7: P[Expr] =
    (iE8 ~ iE7Rest ^^
      {
        case base ~ rest =>
          if (rest.size == 0) {
            base;
          } else if (rest.head.isLeft()) {
            buildLeft(base, rest)
          } else {
            buildRight(base, rest)
          }
      })
  lazy val iE7Rest: P[List[OpPair]] =
    (op7R ~ iE8 ~ iE7R ^^
      { case op ~ left ~ right => RightOpPair(op, left) :: right }
      | op7 ~ iE8 ~ iE7L ^^
      { case op ~ left ~ right => LeftOpPair(op, left) :: right }
      | "" ^^ { case _ => List[OpPair]() })
  lazy val iE7R: P[List[RightOpPair]] =
    (op7R ~ iE8 ~ iE7R ^^
      { case op ~ left ~ right => RightOpPair(op, left) :: right }
      | "" ^^ { case _ => List[RightOpPair]() })
  lazy val iE7L: P[List[LeftOpPair]] =
    (op7 ~ iE8 ~ iE7L ^^
      { case op ~ left ~ right => LeftOpPair(op, left) :: right }
      | "" ^^ { case _ => List[LeftOpPair]() })

  lazy val iE8: P[Expr] =
    (iE9 ~ iE8Rest ^^
      {
        case base ~ rest =>
          if (rest.size == 0) {
            base;
          } else if (rest.head.isLeft()) {
            buildLeft(base, rest)
          } else {
            buildRight(base, rest)
          }
      })
  lazy val iE8Rest: P[List[OpPair]] =
    (op8R ~ iE9 ~ iE8R ^^
      { case op ~ left ~ right => RightOpPair(op, left) :: right }
      | op8 ~ iE9 ~ iE8L ^^
      { case op ~ left ~ right => LeftOpPair(op, left) :: right }
      | "" ^^ { case _ => List[OpPair]() })
  lazy val iE8R: P[List[RightOpPair]] =
    (op8R ~ iE9 ~ iE8R ^^
      { case op ~ left ~ right => RightOpPair(op, left) :: right }
      | "" ^^ { case _ => List[RightOpPair]() })
  lazy val iE8L: P[List[LeftOpPair]] =
    (op8 ~ iE9 ~ iE8L ^^
      { case op ~ left ~ right => LeftOpPair(op, left) :: right }
      | "" ^^ { case _ => List[LeftOpPair]() })

  lazy val iE9: P[Expr] =
    (prefixExpr ~ iE9Rest ^^
      {
        case base ~ rest =>
          if (rest.size == 0) {
            base;
          } else if (rest.head.isLeft()) {
            buildLeft(base, rest)
          } else {
            buildRight(base, rest)
          }
      })
  lazy val iE9Rest: P[List[OpPair]] =
    (op9R ~ prefixExpr ~ iE9R ^^
      { case op ~ left ~ right => RightOpPair(op, left) :: right }
      | op9 ~ prefixExpr ~ iE9L ^^
      { case op ~ left ~ right => LeftOpPair(op, left) :: right }
      | "" ^^ { case _ => List[OpPair]() })
  lazy val iE9R: P[List[RightOpPair]] =
    (op9R ~ prefixExpr ~ iE9R ^^
      { case op ~ left ~ right => RightOpPair(op, left) :: right }
      | "" ^^ { case _ => List[RightOpPair]() })
  lazy val iE9L: P[List[LeftOpPair]] =
    (op9 ~ prefixExpr ~ iE9L ^^
      { case op ~ left ~ right => LeftOpPair(op, left) :: right }
      | "" ^^ { case _ => List[LeftOpPair]() })
  //End of infix expression precedence groups

  lazy val prefixExpr: P[Expr] =
    ("-" ~ simpleExpr ^^
      { case op ~ expr => UnOpExpr(op, expr) }
      | "+" ~ simpleExpr ^^
      { case _ => null }
      | "~" ~ simpleExpr ^^
      { case _ => null }
      | "!" ~ simpleExpr ^^
      { case _ => null }
      | simpleExpr)

  lazy val simpleExpr: P[Expr] =
    ("new" ~ classTemplate ^^
      { case _ ~ Tuple3(ClassInstance(name, args), types, stms) => ClassExpr(name, args)  }
      | blockExpr
      | simpleExpr1 ~ "_" ^^
      { case simpleExpr1 ~ "_" => simpleExpr1 }
      | simpleExpr1)

  lazy val simpleExpr1: P[Expr] =
    (literal
      | simpleExpr1 ~ argumentExprs ^^
      { case funcName ~ args => FunExpr(funcName, args) }
      | path ^^
      { case str => {if (str.contains('.')) FieldSelectionExpr(str) else VarExpr(str)} }
      | "(" ~ exprs ~ ")" ^^
      { case _ ~ exprs ~ _ => {if (exprs.length == 1) exprs.head else null } }
      | "(" ~ ")" ^^
      { case _ => null }
      | simpleExpr ~ "." ~ id ^^
      { case _ => null }
      | simpleExpr ~ typeArgs ^^
      { case _ => null })

  lazy val exprs: P[List[Expr]] =
    (expr ~ "," ~ exprs ^^
      { case expr ~ _ ~ exprs => expr :: exprs }
      | expr ^^
      { case expr => List(expr) })
      
 
  lazy val argumentExprs: P[List[Expr]] =
    ("(" ~ exprs ~ ")" ^^
      { case _ ~ exprs ~ _ => exprs }
      | "(" ~ ")" ^^
      { case _ ~ _ => Nil }
      | blockExpr ^^
      { case blockExpr => null})

  lazy val blockExpr: P[Expr] =
    ("{" ~ caseClauses ~ "}" ^^
      { case _ => null }
      | "{" ~ block ~ "}" ^^
      { case _ ~ stmt ~ _ => BlockExpr(stmt) })

  //Filter out the nulls (empty statements)
  lazy val block: P[List[Stmt]] =
    (rep(blockH) ~ expr ^^
      { case stmt ~ expr => stmt.filter((x => x != null)) ++ List[Expr](expr) }
      | rep(blockH) ^^
      { case stmt => stmt.filter((x => x != null)) })
  //Not defined in scala grammar. Used to match multiple blockStats separated by a semicolon
  lazy val blockH: P[Stmt] =
    (blockStat ~ ";" ^^
      { case stmt ~ _ => stmt })

  lazy val blockStat: P[Stmt] =
    (importG ^^
      { case _ => null }
      | "implicit" ~ defG ^^
      { case _ => null }
      | "lazy" ~ defG ^^
      { case _ => null }
      | defG
      | rep1(localModifier) ~ tmplDef ^^
      { case _ => null }
      | tmplDef ^^
      { case _ => null }
      | expr1
      | "" ^^
      { case _ => null })

  lazy val enumerators: P[Expr] =
    (generator ~ enumeratorsH ^^
      { case _ => null })
  lazy val enumeratorsH: P[List[Expr]] =
    (";" ~ enumerator ~ enumeratorsH ^^
      { case _ => null }
      | ";" ~ enumerator ^^
      { case _ => null })

  lazy val enumerator: P[Expr] =
    (generator ^^
      { case _ => null }
      | guard ^^
      { case _ => null }
      | "val" ~ pattern1 ~ "=" ~ expr ^^
      { case _ => null })

  lazy val generator: P[Expr] =
    (pattern1 ~ "<-" ~ expr ^^
      { case _ => null })

  lazy val caseClauses: P[List[Expr]] =
    (caseClause ~ caseClauses ^^
      { case _ => null }
      | caseClause ^^
      { case _ => null })

  lazy val caseClause: P[Expr] =
    ("case" ~ pattern ~ guard ~ "=>" ~ block ^^
      { case _ => null }
      | "case" ~ pattern ~ "=>" ~ block ^^
      { case _ => null })

  lazy val guard: P[Expr] =
    ("if" ~ postfixExpr ^^
      { case _ => null })

  lazy val pattern: P[Expr] =
    (pattern1 ~ "|" ~ pattern ^^
      { case _ => null }
      | pattern1 ^^
      { case _ => null })

  lazy val pattern1: P[Expr] =
    (pattern2 ^^
      { case _ => null } //may add the following in the future:
      // varid ~ ":" ~ typePat
      // "_" ~ ":" ~ typePat
      )

  lazy val pattern2: P[String] =
    (varid
      | pattern3)

  lazy val pattern3: P[String] =
    (simplePattern ~ id ~ pattern3 ^^
      { case _ => null }
      | simplePattern)

  lazy val simplePattern: P[String] =
    ("_" ^^
      { case _ => null }
      | varid ^^
      { case _ => null }
      | literal ^^
      { case _ => null }
      | qualId
      | qualId ~ "(" ~ patterns ~ ")" ^^
      { case _ => null }
      | qualId ~ "(" ~ ")" ^^
      { case _ => null }
      | "(" ~ patterns ~ ")" ^^
      { case _ => null })

  lazy val patterns: P[String] =
    (pattern ~ "," ~ pattern ^^
      { case _ => null }
      | pattern ^^
      { case _ => null })

  lazy val paramClauses: P[List[List[ParamDclStmt]]] =
    (rep1(paramClause)
      | "" ^^
      { case _ => Nil })

  lazy val paramClause: P[List[ParamDclStmt]] =
    ("(" ~ params ~ ")" ^^
      { case _ ~ params ~ _ => params }
      | "(" ~ ")" ^^
      { case _ ~ _ => Nil })

  lazy val params: P[List[ParamDclStmt]] =
    (param ~ "," ~ params ^^
      { case param ~ _ ~ params => param :: params }
      | param ^^
      { case param => List(param) })

  lazy val param: P[ParamDclStmt] =
    (id ~ ":" ~ paramType ^^
      { case id ~ _ ~ paramType => ParamDclStmt(id, paramType) })

  lazy val paramType: P[Type] =
    (typeG
      | "=>" ~ typeG ^^
      { case _ => null })

  lazy val bindings: P[List[ParamDclStmt]] =
    ("(" ~ binding ~ bindingsH ~ ")" ^^
      { case _ ~ arg ~ args ~ _ => arg :: args }
      | "(" ~ binding ~ ")" ^^
      { case _ ~ arg ~ _ => List(arg) })
  lazy val bindingsH: P[List[ParamDclStmt]] =
    ("," ~ binding ~ bindingsH ^^
      { case _ ~ arg ~ args => arg :: args }
      | "," ~ binding ^^
      { case _ ~ arg => List(arg) })

  lazy val binding: P[ParamDclStmt] =
    (id ~ ":" ~ typeG ^^
      { case arg ~ _ ~ typeG => ParamDclStmt(arg, typeG) }
      | "_" ~ ":" ~ typeG ^^
      { case _ => null }
      | "_" ^^
      { case _ => null })

  lazy val modifier: P[Expr] =
    (localModifier ^^
      { case _ => null }
      | accessModifier ^^
      { case _ => null }
      | "override" ^^
      { case _ => null })

  lazy val localModifier: P[Expr] =
    ("sealed" ^^
      { case _ => null }
      | "implicit" ^^
      { case _ => null }
      | "lazy" ^^
      { case _ => null })

  lazy val accessModifier: P[Expr] =
    ("private" ^^
      { case _ => null })

  lazy val templateBody: P[List[Stmt]] =
    ("{" ~ selfType ~ templateStats ~ "}" ^^
      { case _ => null }
      | "{" ~ templateStats ~ "}" ^^ //Strip out the nulls
      { case _ ~ stmts ~ _ => stmts.filter((x => x != null)) }
    )

  lazy val templateStats : P[List[Stmt]] = 
    ( templateStat ~ ";" ~ templateStats ^^
        {case stat ~ _ ~ stats => stat :: stats}
    | templateStat ^^
        {case stat => List(stat)}
    )
  lazy val templateStat: P[Stmt] =
    (importG ^^
      { case _ => null }
      | modifier ~ defG ^^
      { case _ => null }
      | defG ^^
      { case defG => defG }
      | modifier ~ dcl ^^
      { case _ => null }
      | dcl ^^
      { case _ => null }
      | expr
      | "" ^^
      { case _ => null })

  lazy val selfType: P[Expr] =
    (id ~ "=>" ^^
      { case _ => null })

  lazy val importG: P[Expr] =
    ("import" ~ importExpr ^^
      { case _ => null })

  lazy val importExpr: P[Expr] =
    (qualId ~ "." ~ id ^^
      { case _ => null }
      | qualId ~ "." ~ "_" ^^
      { case _ => null })

  lazy val dcl: P[Expr] =
    ("val" ~ valDcl ^^
      { case _ => null }
      | "var" ~ varDcl ^^
      { case _ => null }
      | "def" ~ funDcl ^^
      { case _ => null })

  lazy val valDcl: P[Expr] =
    (ids ~ ":" ~ typeG ^^
      { case _ => null })

  lazy val varDcl: P[Expr] =
    (ids ~ ":" ~ typeG ^^
      { case _ => null })

  lazy val funDcl: P[Expr] =
    (funSig ~ ":" ~ typeG ^^
      { case _ => null })

  lazy val funSig: P[(String, List[List[ParamDclStmt]])] =
    (id ~ paramClauses ^^
      { case id ~ paramClauses => (id, paramClauses) })

  //Called def in the grammar (which is a reserved word, so I used defG)
  lazy val defG: P[Stmt] =
    ("val" ~ patDef ^^
      { case _ ~ DefWrapper(pats, typeG, expr) => ValDefStmt(pats, typeG, expr, "val") }
      | "var" ~ varDef ^^
      { case _ ~ DefWrapper(pats, typeG, expr) => ValDefStmt(pats, typeG, expr, "var") }
      | "def" ~ funDef ^^
      { case _ ~ FunWrapper(name, paramClauses, retType, body) => FunDefStmt(name, paramClauses, retType, body) }
      | tmplDef ^^
      { case tmplDef => tmplDef })

  lazy val patDef: P[DefWrapper] =
    (pattern2 ~ patDefH ~ ":" ~ typeG ~ "=" ~ expr ^^
      { case pat ~ pats ~ _ ~ typeG ~ _ ~ expr => DefWrapper(pat :: pats, typeG, expr) }
      | pattern2 ~ ":" ~ typeG ~ "=" ~ expr ^^
      { case pat ~ _ ~ typeG ~ _ ~ expr => DefWrapper(List(pat), typeG, expr) })
  //Not defined in scala grammar. Used to match multiple ids seperated by a comma
  lazy val patDefH: P[List[String]] =
    ("," ~ pattern2 ~ patDefH ^^
      { case _ ~ pat ~ pats => pat :: pats }
      | "," ~ pattern2 ^^
      { case _ ~ pat => List(pat) })

  lazy val varDef: P[DefWrapper] =
    (patDef)

  lazy val funDef: P[FunWrapper] =
    (funSig ~ ":" ~ typeG ~ "=" ~ expr ^^
      { case (name, paramClauses) ~ _ ~ retType ~ _ ~ body => FunWrapper(name, paramClauses, retType, body) })

  lazy val tmplDef: P[Stmt] =
    //Assemble the classDefStmt Here
    ("case" ~ "class" ~ classDef ^^
      { case _ ~ _ ~ Tuple3(name, args, Tuple3(whatExtends, extendsWith, body)) 
             => ClassDefStmt("case class", name, args, whatExtends, extendsWith, body ) }
      | "class" ~ classDef ^^
      { case _ ~ Tuple3(name, args, Tuple3(whatExtends, extendsWith, body))
             => ClassDefStmt("class", name, args, whatExtends, extendsWith, body ) }
      | "case" ~ "object" ~ objectDef ^^
      { case _ => null }
      | "object" ~ objectDef ^^
      { case _ ~ Tuple3(name, args, Tuple3(whatExtends, extendsWith, body)) 
             => ClassDefStmt("object", name, args, whatExtends, extendsWith, body ) }
      | "trait" ~ traitDef ^^
      { case _ ~ Tuple2(name, Tuple3(whatExtends, extendsWith, body)) 
             => ClassDefStmt("trait", name, Nil, whatExtends, extendsWith, body) })

  lazy val classDef: P[(String, List[ParamDclStmt], (ClassInstance, List[Type], List[Stmt]))] =
    (id ~ paramClause ~ classTemplateOpt ^^
      { case name ~ params ~ extra  => (name, params, extra) }
      |id ~ classTemplateOpt ^^
      { case name ~ classTemplateOpt => (name, Nil, classTemplateOpt)})

  lazy val traitDef: P[(String, (ClassInstance, List[Type], List[Stmt]))] =
    (id ~ traitTemplateOpt ^^
      { case name ~ traitTemplate => (name, traitTemplate) })

  lazy val objectDef: P[(String, List[ParamDclStmt],(ClassInstance, List[Type], List[Stmt]))] =
    (id ~ classTemplateOpt ^^
      { case name ~ classTemplateOpt => (name, Nil, classTemplateOpt) })

  lazy val classTemplateOpt: P[(ClassInstance, List[Type], List[Stmt])] =
    ("extends" ~ classTemplate ^^
      { case _ ~ cTemplate => cTemplate   }
      | templateBody ^^
      { case stmts => (ClassInstance(BaseType("AnyRef"), Nil), Nil, stmts) }
      | "" ^^
      { case _ => (ClassInstance(BaseType("AnyRef"), Nil), Nil, Nil)})

  lazy val traitTemplateOpt: P[(ClassInstance, List[Type], List[Stmt])] =
    ("extends" ~ traitTemplate ^^
      { case _ ~ tTemplate => tTemplate }
      | templateBody ^^
      { case stms => (ClassInstance(BaseType("AnyRef"), Nil), Nil, stms) }
      | "" ^^
      { case _ => (ClassInstance(BaseType("AnyRef"), Nil), Nil, Nil)})

  lazy val classTemplate: P[(ClassInstance, List[Type], List[Stmt])] =
    (classParents ~ templateBody ^^
      { case (cInstance, withTypes) ~ body => (cInstance, withTypes, body) }
      | classParents ^^
      { case (cInstance, withTypes) => (cInstance, withTypes, Nil) })

  lazy val traitTemplate: P[(ClassInstance, List[Type], List[Stmt])] =
    (traitParents ~ templateBody ^^
      { case (cInstance, withTypes) ~ body => (cInstance, withTypes, body) }
      | traitParents ^^
      { case (cInstance, withTypes) => (cInstance, withTypes, Nil) })

  lazy val classParents: P[(ClassInstance, List[Type])] =
    (constr ~ classParentsH ^^
      { case (name, args) ~ types => (ClassInstance(name, args), types) }
      | constr ^^
      { case (name, args) => (ClassInstance(name, args), Nil) })
  
  lazy val classParentsH: P[List[Type]] =
    ("with" ~ simpleType ~ classParentsH ^^
      { case _ ~ name ~ names => name::names }
      | "with" ~ simpleType ^^
      { case _ ~ name  => List(name) })

  lazy val traitParents: P[(ClassInstance, List[Type])] =
    (simpleType ~ traitParentsH ^^
      { case name ~ types => (ClassInstance(name, Nil), types) }
      | simpleType ^^
      { case name => (ClassInstance(name, Nil), Nil) })
  lazy val traitParentsH: P[List[Type]] =
    ("with" ~ simpleType ~ traitParentsH ^^
      { case _ ~ name ~ names  => name::names }
      | "with" ~ simpleType ^^
      { case _ ~ name => List(name) })

  lazy val constr: P[(Type, List[Expr])] =
    (simpleType ~ "(" ~ exprs ~ ")" ^^
      { case simpleType  ~ _ ~ args ~ _  => (simpleType, args) }
      | simpleType ~ "(" ~ ")" ^^ 
      { case simpleType ~ _ ~ _ => (simpleType, Nil)}
      | simpleType  ^^
      { case simpleType => (simpleType, Nil) })

  lazy val topStatSeq: P[Expr] =
    (topStat ~ topStatSeqH ^^
      { case _ => null }
      | topStat)
  lazy val topStatSeqH: P[Expr] =
    (";" ~ topStat ~ topStatSeqH ^^
      { case _ => null }
      | ";" ~ topStat ^^
      { case _ => null })

  lazy val topStat: P[Expr] =
    (modifier ~ tmplDef ^^
      { case _ => null }
      | tmplDef ^^
      { case _ => null }
      | importG ^^
      { case _ => null }
      | "" ^^
      { case _ => null })

  lazy val compilationUnit: P[Expr] =
    ("package" ~ qualId ~ ";" ~ topStatSeq ^^
      { case _ => null }
      | topStatSeq ^^
      { case _ => null })

  //Used to turn the list of left or right associative expressions in to nested expressions of the correct associativity
  def buildLeft(base: Expr, rest: List[OpPair]): Expr =
    rest.foldLeft(base)((acc, pair) => BinOpExpr(pair.getOp(), acc, pair.getExpr()))

  def buildRight(base: Expr, rest: List[OpPair]): Expr = rest match {
    case Nil => base
    case op :: ops => BinOpExpr(op.getOp(), buildRight(op.getExpr(), ops), base)
  }

  def apply(source: String): Stmt = parseAll(top, source) match {
    case Success(result, _) => result
    case ns: NoSuccess => throw new Exception(ns.msg)
  }


  // Expects first and last characters to be either " or '
  def deQuotify(s: String): String = {
    val buf = new StringBuilder
    var i = 1 // Skip first character
    while (i < s.length - 1) {
      s.charAt(i) match {
        case '\\' => s.charAt(i + 1) match {
          case 'b' => buf.append('\b'); i += 1
          case 'f' => buf.append('\f'); i += 1
          case 'n' => buf.append('\n'); i += 1
          case 'r' => buf.append('\r'); i += 1
          case 't' => buf.append('\t'); i += 1
          case c => buf.append(c); i += 1 // Includes \ " and ', plus any other
        }

        case c => buf.append(c)
      }
      i += 1
    }
    buf.toString
  }

  // Expects first and last _three_ characters to be "
  def deTriquotify(s: String): String = {
    s.substring(3, s.length - 3)
  }
}
