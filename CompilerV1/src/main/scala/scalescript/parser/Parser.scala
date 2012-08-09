package scalescript.parser

import scalescript.ast._

object Parser extends RegexParsers {
  def apply(source: String): Stmt = parseAll(top, source) match {
    case Success(result, _) => result
    case ns: Failure => throw new Exception(ns.msg)
  }
  
  def top: Parser[Stmt] = null // TODO
  
  override val whitespace =
    """(\s|(//(?d:.)*\n)|(/\*(?s:.)*?\*/))*""".r
  
  lazy val op: Parser[String] =
    """[!#%&*+\-/:<=>?@\\^|~]*[!#%&*+\-/:<>?@\\^|~]|[!<>]=|=[!#%&*+\-/:<=>?@\\^|~]*=""".r
    
  lazy val assignop: Parser[String] =
    """[#%&*+\-/:?@\\^|~][!#%&*+\-/:<=>?@\\^|~]*=|[!<>][!#%&*+\-/:<=>?@\\^|~]+=|=""".r
  
  lazy val varid: Parser[String] =
    """[a-z][A-Za-z0-9$_]*(_[!#%&*+\-/:<=>?@\\^|~]+)?""".r - """else|while|match""".r
  
  lazy val upperid: Parser[String] =
    """([A-Z$]|_[A-Za-z0-9$_])[A-Za-z0-9$_]*(_[!#%&*+\-/:<=>?@\\^|~]+)?""".r
  
  lazy val plainid: Parser[String] =
    ( upperid
    | varid
    | op
    )
  
  lazy val id: Parser[String] =
    ( plainid
    | ("`" + """([^`\p{Cntrl}\\]|\\[\\"'bfnrt])+""" + "`").r ^^ {
        case strLit => deQuotify(strLit)
      }
    )
  
  lazy val integerLiteral: Parser[String] =
    """\d+""".r
  
  lazy val floatingPointLiteral: Parser[String] =
    """(\d+\.(\d*)?|\d*\.\d+)([eE][+-]?\d+)?|\d+([eE][+-]?\d+)""".r

  lazy val booleanLiteral: Parser[String] =
    """(true|false)""".r

  lazy val characterLiteral: Parser[String] =
    ("\'" + """([^\p{Cntrl}\\]|\\[\\"'bfnrt])""" + "\'").r

  lazy val stringLiteral: Parser[String] =
    ("\"" + """([^"\p{Cntrl}\\]|\\[\\"'bfnrt])*""" + "\"").r

  lazy val multilineStringLiteral: Parser[String] =
    ("\"\"\"" + """("?"?[^"])*"*""" + "\"\"\"").r
    
  lazy val literal: Parser[Expr] =
    ( "-" ~> floatingPointLiteral ^^ {
        case numLit => NumExpr(NDouble(-numLit.toDouble))
      }
    | floatingPointLiteral ^^ {
        case numLit => NumExpr(NDouble(numLit.toDouble))
      }
    | "-" ~> integerLiteral ^^ {
        case numLit => NumExpr(NInt(-numLit.toInt))
      }
    | integerLiteral ^^ {
        case numLit => NumExpr(NInt(numLit.toInt))
      }
    | booleanLiteral ^^ {
        case boolLit => BoolExpr(boolLit.toBoolean)
      }
    | characterLiteral ^^ {
        case charLit => CharExpr(deQuotify(charLit).charAt(0))
      }
    | multilineStringLiteral ^^ {
        case strLit => StringExpr(deTriquotify(strLit))
      }
    | stringLiteral ^^ {
        case strLit => StringExpr(deQuotify(strLit))
      }
    | "null" ^^ {
        case _ => null // TODO
      }
    )
    
  lazy val qualId: Parser[List[String]] =
    ( id ~ ("." ~> qualId) ^^ {
        case i ~ qual => i :: qual
      }
    | id ^^ {
        case i => List(i)
      }
    )
  
  lazy val ids: Parser[List[String]] =
    ( id ~ ("," ~> ids) ^^ {
        case i ~ is => i :: is
      }
    | id ^^ { 
        case i => List(i)
      }
    )

  lazy val path: Parser[List[String]] =
    ( qualId
    | "this" ^^ {
        case _ => List("this")
      }
    )

  //Called type in the grammar (which is a reserved word, so I used typeG)
  lazy val typeG: Parser[Type] =
    ( functionArgTypes ~ ("=>" ~> typeG) ^^ {
        case argTypes ~ resultType => FuncType(resultType, argTypes)
      }
    | infixType
    )

  lazy val functionArgTypes: Parser[List[Type]] =
    ( "(" ~> ")" ^^ {
        case _ => Nil
      }
    | "(" ~> functionArgTypesH <~ ")"
    | infixType ^^ {
        case iType => List(iType)
      }
    )
    
  lazy val functionArgTypesH: Parser[List[Type]] =
    ( paramType ~ ("," ~> functionArgTypesH) ^^ {
        case pType ~ moreTypes => pType :: moreTypes
      }
    | paramType ^^ {
        case pType => List(pType)
      }
    )

  lazy val infixType: Parser[Type] =
    ( simpleType ~ id ~ infixType ^^ {
        case _ => null // TODO
      }
    | simpleType
    )

  lazy val simpleType: Parser[Type] =
    ( simpleType ~ typeArgs ^^ {
        case _ => null // TODO
      }
    | simpleType ~ "#" ~ id ^^ {
        case _ => null // TODO do we need this?
      }
    | qualId ^^ {
        case qid => BaseType(qid)
      }
    | "(" ~ types ~ ")" ^^ {
        case _ => null // TODO also handle ()?
      }
    )

  lazy val typeArgs: Parser[List[Type]] =
    "[" ~> types <~ "]"

  lazy val types: Parser[List[Type]] =
    ( typeG ~ ("," ~> types) ^^ {
        case t ~ ts => t :: ts
      }
    | typeG ^^ {
        case t => List(t)
      }
    )

  lazy val typePat: Parser[Type] =
    typeG // TODO not currently used

  lazy val expr: Parser[Expr] =
    ( bindings ~ ("=>" ~> expr) ^^ {
        case args ~ body => AnonFuncExpr(args, body)
      }
    | id ~ ("=>" ~ expr) ^^ {
        case arg ~ body => null // TODO
      }
    | "_" ~> "=>" ~> expr ^^ {
        case body => null // TODO
      }
    | expr1
    )

  lazy val expr1: Parser[Expr] =
    ( "if" ~> ("(" ~> expr <~ ")") ~ expr ~ ((";" ?) ~ "else" ~> expr ?) ^^ {
        case test ~ trueClause ~ Some(falseClause) => IfThenElseExpr(test, trueClause, falseClause)
        case test ~ trueClause ~ None => IfThenExpr(test, trueClause)
      }
    | "while" ~> ("(" ~> expr <~ ")") ~ expr ^^ {
        case test ~ body => WhileExpr(test, body, false)
      }
    | "do" ~> expr ~ ((";" ?) ~ "while" ~ "(" ~> expr <~ ")") ^^ {
        case body ~ test => WhileExpr(test, body, true)
      }
    | "for" ~> ("(" ~> enumerators <~ ")") ~ expr ^^ {
        case _ => null // TODO
      }
    | "for" ~> ("{" ~> enumerators <~ "}") ~ ("yield" ~> expr) ^^ {
        case _ => null // TODO
      }
    | "throw" ~> expr ^^ {
        case _ => null // TODO
      }
    | "return" ~> expr ^^ {
        case _ => null // TODO
      }
    | "return" ^^ {
        case _ => null // TODO
      }
    | simpleExpr1 ~ assignop ~ expr ^^ {
        case p ~ a ~ e => AssignExpr(p, a, e)
      }
    | postfixExpr ~ ("match" ~ "{" ~> caseClauses <~ "}") ^^ {
        case _ => null // TODO
      }
    | postfixExpr
    )

  lazy val postfixExpr: Parser[Expr] =
	( infixExpr ~ id ^^ {
	    case _ => null
	  }
    | infixExpr
    )

  lazy val infixExpr: Parser[Expr] =
    ( prefixExpr ~ (id ~ prefixExpr *) ^^ {
        case eFirst ~ eRest => handlePrecedence(eFirst, eRest)
      }
    )
        
  lazy val prefixExpr: Parser[Expr] = null // TODO
  lazy val simpleExpr1: Parser[Expr] = null // TODO
  lazy val enumerators: Parser[Expr] = null // TODO
  lazy val paramType: Parser[Type] = null // TODO
  lazy val bindings: Parser[List[ParamDclStmt]] = null // TODO
  lazy val caseClauses: Parser[Expr] = null // TODO
  
  def handlePrecedence(eFirst: Expr, eRest: List[String ~ Expr]): Expr = null // TODO
  
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
          case c => buf.append(c); i += 1 // Includes \, ", and ', plus any other
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