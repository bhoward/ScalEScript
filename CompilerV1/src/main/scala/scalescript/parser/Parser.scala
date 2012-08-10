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
    ( "this" ^^ {
        case _ => List("this") // TODO return something different here than what qualId would have anyway?
      }
    | qualId
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
        
  lazy val prefixExpr: Parser[Expr] =
    ( "-" ~ simpleExpr ^^ {
        case op ~ expr => UnOpExpr(op, expr)
      }
    | "+" ~ simpleExpr ^^ {
        case op ~ expr => UnOpExpr(op, expr)
      }
    | "~" ~ simpleExpr ^^ {
        case op ~ expr => UnOpExpr(op, expr)
      }
    | "!" ~ simpleExpr ^^ {
        case op ~ expr => UnOpExpr(op, expr)
      }
    | simpleExpr
    )

    lazy val simpleExpr: Parser[Expr] =
    ( "new" ~> classTemplate ^^ { //TODO what if stms is not Nil? Then create an anonymous subclass, and do a classExpr on that
        case Tuple3(ClassInstance(name, args), types, stms) => ClassExpr(name, args)
      }
    | blockExpr
    | simpleExpr1 ~ "_" ^^ {
        case simpleExpr1 ~ "_" => simpleExpr1
      }
    | simpleExpr1
    )

    lazy val simpleExpr1: Parser[Expr] =
    ( literal
    | simpleExpr1 ~ argumentExprs ^^ {
        case funcName ~ args => FunExpr(funcName, args)
      }
    | path ^^ {
        case p => fieldSplit(VarExpr(p.head), p.tail)
      }
    | "(" ~> exprs <~ ")" ^^ {
        case es => if (es.length == 1) es.head else null // TODO handle tuples
      }
    | "(" ~ ")" ^^ {
        case _ => null // TODO
      }
    | simpleExpr ~ ("." ~> id) ^^ {
        case left ~ right => FieldSelectionExpr(left, right)
      } // TODO Make this reachable. - Parser gets caught up before reaching this. Possible workaround: just assign to a variable and call a field on that.
    | simpleExpr ~ typeArgs ^^ {
        case _ => null // TODO
      }
    )
// TODO continue here
    lazy val exprs: Parser[List[Expr]] =
    ( expr ~ "," ~ exprs ^^ {
        case expr ~ _ ~ exprs => expr :: exprs
      }
    | expr ^^ {
        case expr => List(expr)
      }
    )

    lazy val argumentExprs: Parser[List[Expr]] =
    ( "(" ~ exprs ~ ")" ^^ {
        case _ ~ exprs ~ _ => exprs
      }
    | "(" ~ ")" ^^ {
        case _ ~ _ => Nil
      }
    | blockExpr ^^ {
        case blockExpr => null
      }
    )

    lazy val blockExpr: Parser[Expr] =
    ( "{" ~ caseClauses ~ "}" ^^ {
        case _ => null
      }
    | "{" ~ block ~ "}" ^^ {
        case _ ~ stmt ~ _ => BlockExpr(stmt)
      }
    )

    //Filter out the nulls (empty statements)
    lazy val block: Parser[List[Stmt]] =
    ( (blockH *) ~ expr ^^ {
        case stmt ~ expr => stmt.filter((x => x != null)) ++ List[Expr](expr)
      }
    | (blockH *) ^^ {
        case stmt => stmt.filter((x => x != null))
      }
    )
    //Not defined in scala grammar. Used to match multiple blockStats separated by a semicolon
    lazy val blockH: Parser[Stmt] =
    ( blockStat ~ ";" ^^ {
        case stmt ~ _ => stmt
      }
    )

    lazy val blockStat: Parser[Stmt] =
    ( importG ^^ {
        case _ => null
      }
    | "implicit" ~ defG ^^ {
        case _ => null
      }
    | "lazy" ~ defG ^^ {
        case _ => null
      }
    | defG
    | (localModifier *) ~ tmplDef ^^ {
        case _ => null
      }
    | expr1
    | "" ^^ {
        case _ => null
      }
    )

    lazy val enumerators: Parser[Expr] =
    ( generator ~ enumeratorsH ^^ {
        case _ => null
      }
    )
    lazy val enumeratorsH: Parser[List[Expr]] =
    ( ";" ~ enumerator ~ enumeratorsH ^^ {
        case _ => null
      }
    | ";" ~ enumerator ^^ {
        case _ => null
      }
    )

    lazy val enumerator: Parser[Expr] =
    ( generator ^^ {
        case _ => null
      }
    | guard ^^ {
        case _ => null
      }
    | "val" ~ pattern1 ~ "=" ~ expr ^^ {
        case _ => null
      }
    )

    lazy val generator: Parser[Expr] =
    ( pattern1 ~ "<-" ~ expr ^^ {
        case _ => null
      }
    )

    lazy val caseClauses: Parser[List[Expr]] =
    ( caseClause ~ caseClauses ^^ {
        case _ => null
      }
    | caseClause ^^ {
        case _ => null
      }
    )

    lazy val caseClause: Parser[Expr] =
    ( "case" ~ pattern ~ guard ~ "=>" ~ block ^^ {
        case _ => null
      }
    | "case" ~ pattern ~ "=>" ~ block ^^ {
        case _ => null
      }
    )

    lazy val guard: Parser[Expr] =
    ( "if" ~ postfixExpr ^^ {
        case _ => null
      }
    )

    lazy val pattern: Parser[Expr] =
    ( pattern1 ~ "|" ~ pattern ^^ {
        case _ => null
      }
    | pattern1 ^^ {
        case _ => null
      }
    )

    lazy val pattern1: Parser[Expr] =
    ( pattern2 ^^ {
        case _ => null
      } //may add the following in the future:
            // varid ~ ":" ~ typePat
            // "_" ~ ":" ~ typePat
            )

    lazy val pattern2: Parser[String] =
    ( varid
    | pattern3
    )

    lazy val pattern3: Parser[String] =
    ( simplePattern ~ id ~ pattern3 ^^ {
        case _ => null
      }
    | simplePattern
    )

    lazy val simplePattern: Parser[String] =
    ( "_" ^^ {
        case _ => null
      }
    | varid ^^ {
        case _ => null
      }
    | literal ^^ {
        case _ => null
      }
    | qualId ^^ {
        case _ => null
      }
    | qualId ~ "(" ~ patterns ~ ")" ^^ {
        case _ => null
      }
    | qualId ~ "(" ~ ")" ^^ {
        case _ => null
      }
    | "(" ~ patterns ~ ")" ^^ {
        case _ => null
      }
    )

    lazy val patterns: Parser[String] =
    ( pattern ~ "," ~ pattern ^^ {
        case _ => null
      }
    | pattern ^^ {
        case _ => null
      }
    )

    lazy val paramClauses: Parser[List[List[ParamDclStmt]]] =
    ( paramClause *
    )

    lazy val paramClause: Parser[List[ParamDclStmt]] =
    ( "(" ~ params ~ ")" ^^ {
        case _ ~ params ~ _ => params
      }
    | "(" ~ ")" ^^ {
        case _ ~ _ => Nil
      }
    )

    lazy val params: Parser[List[ParamDclStmt]] =
    ( param ~ "," ~ params ^^ {
        case param ~ _ ~ params => param :: params
      }
    | param ^^ {
        case param => List(param)
      }
    )

    lazy val param: Parser[ParamDclStmt] =
    ( id ~ ":" ~ paramType ^^ {
        case id ~ _ ~ paramType => ParamDclStmt(id, paramType)
      }
    )

    lazy val paramType: Parser[Type] =
    ( typeG
    | "=>" ~ typeG ^^ {
        case _ => null
      }
    )

    lazy val bindings: Parser[List[ParamDclStmt]] =
    ( "(" ~ binding ~ bindingsH ~ ")" ^^ {
        case _ ~ arg ~ args ~ _ => arg :: args
      }
    | "(" ~ binding ~ ")" ^^ {
        case _ ~ arg ~ _ => List(arg)
      }
    )
    lazy val bindingsH: Parser[List[ParamDclStmt]] =
    ( "," ~ binding ~ bindingsH ^^ {
        case _ ~ arg ~ args => arg :: args
      }
    | "," ~ binding ^^ {
        case _ ~ arg => List(arg)
      }
    )

    lazy val binding: Parser[ParamDclStmt] =
    ( id ~ ":" ~ typeG ^^ {
        case arg ~ _ ~ typeG => ParamDclStmt(arg, typeG)
      }
    | "_" ~ ":" ~ typeG ^^ {
        case _ => null
      }
    | "_" ^^ {
        case _ => null
      }
    )

    lazy val modifier: Parser[Expr] =
    ( localModifier ^^ {
        case _ => null
      }
    | accessModifier ^^ {
        case _ => null
      }
    | "override" ^^ {
        case _ => null
      }
    )

    lazy val localModifier: Parser[Expr] =
    ( "sealed" ^^ {
        case _ => null
      }
    | "implicit" ^^ {
        case _ => null
      }
    | "lazy" ^^ {
        case _ => null
      }
    )

    lazy val accessModifier: Parser[Expr] =
    ( "private" ^^ {
        case _ => null
      }
    )

    lazy val templateBody: Parser[List[Stmt]] =
    ( "{" ~ selfType ~ templateStats ~ "}" ^^ {
        case _ => null
      }
    | "{" ~ templateStats ~ "}" ^^ //Strip out the nulls
            {
        case _ ~ stmts ~ _ => stmts.filter((x => x != null))
      }
    )

    lazy val templateStats: Parser[List[Stmt]] =
    ( templateStat ~ ";" ~ templateStats ^^ {
        case stat ~ _ ~ stats => stat :: stats
      }
    | templateStat ^^ {
        case stat => List(stat)
      }
    )
    lazy val templateStat: Parser[Stmt] =
    ( importG ^^ {
        case _ => null
      }
    | modifier ~ defG ^^ {
        case _ => null
      }
    | defG ^^ {
        case defG => defG
      }
    | modifier ~ dcl ^^ {
        case _ => null
      }
    | dcl ^^ {
        case _ => null
      }
    | expr
    | "" ^^ {
        case _ => null
      }
    )

    lazy val selfType: Parser[Expr] =
    ( id ~ "=>" ^^ {
        case _ => null
      }
    )

    lazy val importG: Parser[Expr] =
    ( "import" ~ importExpr ^^ {
        case _ => null
      }
    )

    lazy val importExpr: Parser[Expr] =
    ( qualId ~ "." ~ id ^^ {
        case _ => null
      }
    | qualId ~ "." ~ "_" ^^ {
        case _ => null
      }
    )

    lazy val dcl: Parser[Expr] =
    ( "val" ~ valDcl ^^ {
        case _ => null
      }
    | "var" ~ varDcl ^^ {
        case _ => null
      }
    | "def" ~ funDcl ^^ {
        case _ => null
      }
    )

    lazy val valDcl: Parser[Expr] =
    ( ids ~ ":" ~ typeG ^^ {
        case _ => null
      }
    )

    lazy val varDcl: Parser[Expr] =
    ( ids ~ ":" ~ typeG ^^ {
        case _ => null
      }
    )

    lazy val funDcl: Parser[Expr] =
    ( funSig ~ ":" ~ typeG ^^ {
        case _ => null
      }
    )

    lazy val funSig: Parser[(String, List[List[ParamDclStmt]])] =
    ( id ~ paramClauses ^^ {
        case id ~ paramClauses => (id, paramClauses)
      }
    )

    //Called def in the grammar (which is a reserved word, so I used defG)
    lazy val defG: Parser[Stmt] =
    ( "val" ~ patDef ^^ {
        case _ ~ DefWrapper(pats, typeG, expr) => ValDefStmt(pats, typeG, expr, "val")
      }
    | "var" ~ varDef ^^ {
        case _ ~ DefWrapper(pats, typeG, expr) => ValDefStmt(pats, typeG, expr, "var")
      }
    | "def" ~ funDef ^^ {
        case _ ~ FunWrapper(name, paramClauses, retType, body) => FunDefStmt(name, paramClauses, retType, body)
      }
    | tmplDef ^^ {
        case tmplDef => tmplDef
      }
    )

    lazy val patDef: Parser[DefWrapper] =
    ( pattern2 ~ patDefH ~ ":" ~ typeG ~ "=" ~ expr ^^ {
        case pat ~ pats ~ _ ~ typeG ~ _ ~ expr => DefWrapper(pat :: pats, typeG, expr)
      }
    | pattern2 ~ ":" ~ typeG ~ "=" ~ expr ^^ {
        case pat ~ _ ~ typeG ~ _ ~ expr => DefWrapper(List(pat), typeG, expr)
      }
    )
    //Not defined in scala grammar. Used to match multiple ids seperated by a comma
    lazy val patDefH: Parser[List[String]] =
    ( "," ~ pattern2 ~ patDefH ^^ {
        case _ ~ pat ~ pats => pat :: pats
      }
    | "," ~ pattern2 ^^ {
        case _ ~ pat => List(pat)
      }
    )

    lazy val varDef: Parser[DefWrapper] =
    ( patDef)

    lazy val funDef: Parser[FunWrapper] =
    ( funSig ~ ":" ~ typeG ~ "=" ~ expr ^^ {
        case (name, paramClauses) ~ _ ~ retType ~ _ ~ body => FunWrapper(name, paramClauses, retType, body)
      }
    )

    lazy val tmplDef: Parser[Stmt] =
        //Assemble the classDefStmt Here
    ( "case" ~ "class" ~ classDef ^^ {
                case _ ~ _ ~ Tuple3(name, args, Tuple3(whatExtends, extendsWith, body)) => ClassDefStmt("case class", name, args, whatExtends, extendsWith, body)
           
      }
    | "class" ~ classDef ^^ {
                case _ ~ Tuple3(name, args, Tuple3(whatExtends, extendsWith, body)) => ClassDefStmt("class", name, args, whatExtends, extendsWith, body)
           
      }
    | "case" ~ "object" ~ objectDef ^^ {
                case _ ~ Tuple3(name, args, Tuple3(whatExtends, extendsWith, body)) => ClassDefStmt("case object", name, args, whatExtends, extendsWith, body)
           
      }
    | "object" ~ objectDef ^^ {
                case _ ~ Tuple3(name, args, Tuple3(whatExtends, extendsWith, body)) => ClassDefStmt("object", name, args, whatExtends, extendsWith, body)
           
      }
    | "trait" ~ traitDef ^^ {
                case _ ~ Tuple2(name, Tuple3(whatExtends, extendsWith, body)) => ClassDefStmt("trait", name, Nil, whatExtends, extendsWith, body)
           
      }
    )

    lazy val classDef: Parser[(String, List[ParamDclStmt], (ClassInstance, List[Type], List[Stmt]))] =
    ( id ~ paramClause ~ classTemplateOpt ^^ {
        case name ~ params ~ extra => (name, params, extra)
      }
    | id ~ classTemplateOpt ^^ {
        case name ~ classTemplateOpt => (name, Nil, classTemplateOpt)
      }
    )

    lazy val traitDef: Parser[(String, (ClassInstance, List[Type], List[Stmt]))] =
    ( id ~ traitTemplateOpt ^^ {
        case name ~ traitTemplate => (name, traitTemplate)
      }
    )

    lazy val objectDef: Parser[(String, List[ParamDclStmt], (ClassInstance, List[Type], List[Stmt]))] =
    ( id ~ classTemplateOpt ^^ {
        case name ~ classTemplateOpt => (name, Nil, classTemplateOpt)
      }
    )

    lazy val classTemplateOpt: Parser[(ClassInstance, List[Type], List[Stmt])] =
    ( "extends" ~ classTemplate ^^ {
        case _ ~ cTemplate => cTemplate
      }
    | templateBody ^^ {
        case stmts => (ClassInstance(BaseType(List("AnyRef")), Nil), Nil, stmts)
      }
    | "" ^^ {
        case _ => (ClassInstance(BaseType(List("AnyRef")), Nil), Nil, Nil)
      }
    )

    lazy val traitTemplateOpt: Parser[(ClassInstance, List[Type], List[Stmt])] =
    ( "extends" ~ traitTemplate ^^ {
        case _ ~ tTemplate => tTemplate
      }
    | templateBody ^^ {
        case stms => (ClassInstance(BaseType(List("AnyRef")), Nil), Nil, stms)
      }
    | "" ^^ {
        case _ => (ClassInstance(BaseType(List("AnyRef")), Nil), Nil, Nil)
      }
    )

    lazy val classTemplate: Parser[(ClassInstance, List[Type], List[Stmt])] =
    ( classParents ~ templateBody ^^ {
        case (cInstance, withTypes) ~ body => (cInstance, withTypes, body)
      }
    | classParents ^^ {
        case (cInstance, withTypes) => (cInstance, withTypes, Nil)
      }
    )

    lazy val traitTemplate: Parser[(ClassInstance, List[Type], List[Stmt])] =
    ( traitParents ~ templateBody ^^ {
        case (cInstance, withTypes) ~ body => (cInstance, withTypes, body)
      }
    | traitParents ^^ {
        case (cInstance, withTypes) => (cInstance, withTypes, Nil)
      }
    )

    lazy val classParents: Parser[(ClassInstance, List[Type])] =
    ( constr ~ classParentsH ^^ {
        case (name, args) ~ types => (ClassInstance(name, args), types)
      }
    | constr ^^ {
        case (name, args) => (ClassInstance(name, args), Nil)
      }
    )

    lazy val classParentsH: Parser[List[Type]] =
    ( "with" ~ simpleType ~ classParentsH ^^ {
        case _ ~ name ~ names => name :: names
      }
    | "with" ~ simpleType ^^ {
        case _ ~ name => List(name)
      }
    )

    lazy val traitParents: Parser[(ClassInstance, List[Type])] =
    ( simpleType ~ traitParentsH ^^ {
        case name ~ types => (ClassInstance(name, Nil), types)
      }
    | simpleType ^^ {
        case name => (ClassInstance(name, Nil), Nil)
      }
    )
    lazy val traitParentsH: Parser[List[Type]] =
    ( "with" ~ simpleType ~ traitParentsH ^^ {
        case _ ~ name ~ names => name :: names
      }
    | "with" ~ simpleType ^^ {
        case _ ~ name => List(name)
      }
    )

    lazy val constr: Parser[(Type, List[Expr])] =
    ( simpleType ~ "(" ~ exprs ~ ")" ^^ {
        case simpleType ~ _ ~ args ~ _ => (simpleType, args)
      }
    | simpleType ~ "(" ~ ")" ^^ {
        case simpleType ~ _ ~ _ => (simpleType, Nil)
      }
    | simpleType ^^ {
        case simpleType => (simpleType, Nil)
      }
    )

    lazy val topStatSeq: Parser[Expr] =
    ( topStat ~ topStatSeqH ^^ {
        case _ => null
      }
    | topStat)
    lazy val topStatSeqH: Parser[Expr] =
    ( ";" ~ topStat ~ topStatSeqH ^^ {
        case _ => null
      }
    | ";" ~ topStat ^^ {
        case _ => null
      }
    )

    lazy val topStat: Parser[Expr] =
    ( modifier ~ tmplDef ^^ {
        case _ => null
      }
    | tmplDef ^^ {
        case _ => null
      }
    | importG ^^ {
        case _ => null
      }
    | "" ^^ {
        case _ => null
      }
    )

    lazy val compilationUnit: Parser[Expr] =
    ( "package" ~ qualId ~ ";" ~ topStatSeq ^^ {
        case _ => null
      }
    | topStatSeq ^^ {
        case _ => null
      }
    )
  
  def handlePrecedence(eFirst: Expr, eRest: List[String ~ Expr]): Expr = hpAux(Nil, eFirst, eRest)
  
  def hpAux(left: List[(Expr, String)], mid: Expr, right: List[String ~ Expr]): Expr = (left, right) match {
    case (Nil, Nil) => mid
    case ((lExpr, lOp) :: lRest, Nil) => hpAux(lRest, BinOpExpr(lOp, lExpr, mid), Nil)
    case (Nil, (rOp ~ rExpr) :: rRest) => hpAux((mid, rOp) :: Nil, rExpr, rRest)
    case ((lExpr, lOp) :: lRest, (rOp ~ rExpr) :: rRest) =>
      if (isHigherPrecedence(lOp, rOp)) {
        hpAux(lRest, BinOpExpr(lOp, lExpr, mid), right)
      } else {
        hpAux((mid, rOp) :: left, rExpr, rRest)
      }
  }
    
  def isHigherPrecedence(lOp: String, rOp: String): Boolean = {
    val lPrec = getPrecedence(lOp)
    val rPrec = getPrecedence(rOp)
    val lAssoc = isLeftAssociative(lOp)
    val rAssoc = isLeftAssociative(rOp)
    if (lPrec == rPrec && lAssoc != rAssoc) {
      throw new Exception("Mixture of left and right associative operators with same precedence")
    }
    lPrec > rPrec || (lPrec == rPrec && lAssoc)
  }
  
  val precedence = Map(
      '|' -> 1,
      '^' -> 2,
      '&' -> 3,
      '=' -> 4, '!' -> 4,
      '<' -> 5, '>' -> 5,
      ':' -> 6,
      '+' -> 7, '-' -> 7,
      '*' -> 8, '/' -> 8, '%' -> 8
    )
    
  def getPrecedence(operator: String): Int = {
    val c = operator.charAt(0)
    precedence.get(c) match {
      case Some(p) => p
      case None => if (c.isLetter) 0 else 9
    }
  }
  
  def isLeftAssociative(operator: String): Boolean = {
    val c = operator.charAt(operator.length - 1)
    c != ':'
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
  
  def fieldSplit(base: Expr, fields: List[String]): Expr = fields match {
    case Nil => base
    case f :: fs => fieldSplit(FieldSelectionExpr(base, f), fs)
  }
}
