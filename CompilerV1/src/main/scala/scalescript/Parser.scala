package scalescript

/**
 * @author Trevor Griswold
 * @author Mike Stees
 * @author Brian Howard
 */
object Parser extends TokenParsers {
  def apply(source: String): List[Stmt] = parseAll(top, new TokenStream(source)) match {
    case Success(result, _) => result
    case ns: Failure => throw new Exception(ns.msg)
  }
  
  // def top: Parser[List[Stmt]] = topStatSeq // corresponds to a Scala "script" -- no package spec
  def top: Parser[List[Stmt]] = templateStats // corresponds to REPL in :paste mode
  
  lazy val varid: Parser[String] =
    toktype(LID)
  
  lazy val plainid: Parser[String] =
    ( UID
    | varid
    | OP
    )
  
  lazy val id: Parser[String] =
    ( plainid
    | SID
    )
  
  lazy val integerLiteral: Parser[String] =
    toktype(INT)
  
  lazy val floatingPointLiteral: Parser[String] =
    toktype(DOUBLE)

  lazy val booleanLiteral: Parser[String] =
    ( TRUE
    | FALSE
    )

  lazy val characterLiteral: Parser[String] =
    toktype(CHAR)

  lazy val stringLiteral: Parser[String] =
    toktype(STRING)

  lazy val literal: Parser[Expr] =
    ( Token(OP, "-") ~> floatingPointLiteral ^^ {
        case numLit => NumExpr(NDouble(-numLit.toDouble))
      }
    | floatingPointLiteral ^^ {
        case numLit => NumExpr(NDouble(numLit.toDouble))
      }
    | Token(OP, "-") ~> integerLiteral ^^ {
        case numLit => NumExpr(NInt(-numLit.toInt))
      }
    | integerLiteral ^^ {
        case numLit => NumExpr(NInt(numLit.toInt))
      }
    | booleanLiteral ^^ {
        case boolLit => BoolExpr(boolLit.toBoolean)
      }
    | characterLiteral ^^ {
        case charLit => CharExpr(charLit.charAt(0))
      }
    | stringLiteral ^^ {
        case strLit => StringExpr(strLit)
      }
    | NULL ^^ {
        case _ => null // TODO
      }
    )
    
  lazy val qualId: Parser[List[String]] =
    ( id ~ (DOT ~> id *) ^^ {
        case i ~ qual => i :: qual
      }
    )
  
  lazy val ids: Parser[List[String]] =
    ( id ~ (COMMA ~> ids) ^^ {
        case i ~ is => i :: is
      }
    | id ^^ { 
        case i => List(i)
      }
    )

  lazy val path: Parser[List[String]] =
    ( THIS ^^ {
        case _ => List("this") // TODO return something different here than what qualId would have anyway?
      }
    | qualId
    )

  //Called type in the grammar (which is a reserved word, so I used typeG)
  lazy val typeG: Parser[Type] =
    ( functionArgTypes ~ (ARROW ~> typeG) ^^ {
        case argTypes ~ resultType => FuncType(resultType, argTypes)
      }
    | infixType
    )

  lazy val functionArgTypes: Parser[List[Type]] =
    ( LPAREN ~> RPAREN ^^ {
        case _ => Nil
      }
    | LPAREN ~> functionArgTypesH <~ RPAREN
    | infixType ^^ {
        case iType => List(iType)
      }
    )
    
  lazy val functionArgTypesH: Parser[List[Type]] =
    ( paramType ~ (COMMA ~> functionArgTypesH) ^^ {
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
    ( qualId ~ (simpleTypeRest *) ^^ {
        case qid ~ _ => BaseType(qid.mkString(".")) // TODO handle this differently?
      }
    | LPAREN ~ types ~ RPAREN ~ (simpleTypeRest *) ^^ {
        case _ => null // TODO also handle ()?
      }
    )
    
  lazy val simpleTypeRest: Parser[Null] = // TODO
    ( typeArgs ^^ {
        case _ => null
      }
    | HASH ~ id ^^ {
        case _ => null
      })

  lazy val typeArgs: Parser[List[Type]] =
    LBRACK ~> types <~ RBRACK

  lazy val types: Parser[List[Type]] =
    ( typeG ~ (COMMA ~> types) ^^ {
        case t ~ ts => t :: ts
      }
    | typeG ^^ {
        case t => List(t)
      }
    )

  lazy val typePat: Parser[Type] =
    typeG // TODO not currently used

  lazy val expr: Parser[Expr] =
    ( bindings ~ (ARROW ~> expr) ^^ {
        case args ~ body => AnonFuncExpr(args, body)
      }
    | id ~ (ARROW ~ expr) ^^ {
        case arg ~ body => null // TODO
      }
    | UNDER ~> ARROW ~> expr ^^ {
        case body => null // TODO
      }
    | expr1
    )

  lazy val expr1: Parser[Expr] =
    ( IF ~> (LPAREN ~> expr <~ RPAREN) ~ expr ~ ((SEMI ?) ~ ELSE ~> expr ?) ^^ {
        case test ~ trueClause ~ Some(falseClause) => IfThenElseExpr(test, trueClause, falseClause)
        case test ~ trueClause ~ None => IfThenExpr(test, trueClause)
      }
    | WHILE ~> (LPAREN ~> expr <~ RPAREN) ~ expr ^^ {
        case test ~ body => WhileExpr(test, body, false)
      }
    | DO ~> expr ~ ((SEMI ?) ~ WHILE ~ LPAREN ~> expr <~ RPAREN) ^^ {
        case body ~ test => WhileExpr(test, body, true)
      }
    | FOR ~> (LPAREN ~> enumerators <~ RPAREN) ~ expr ^^ {
        case _ => null // TODO
      }
    | FOR ~> (LBRACE ~> enumerators <~ RBRACE) ~ (YIELD ~> expr) ^^ {
        case _ => null // TODO
      }
    | THROW ~> expr ^^ {
        case _ => null // TODO
      }
    | RETURN ~> expr ^^ {
        case _ => null // TODO
      }
    | RETURN ^^ {
        case _ => null // TODO
      }
    | simpleExpr1 ~ EQUAL ~ expr ^^ {
        case p ~ a ~ e => AssignExpr(p, a, e)
      }
    | postfixExpr ~ (MATCH ~ LBRACE ~> caseClauses <~ RBRACE) ^^ {
        case _ => null // TODO
      }
    | postfixExpr
    )

  lazy val postfixExpr: Parser[Expr] =
	( infixExpr ~ (ASSIGN ~ infixExpr *) ~ (id ?) ^^ {
        case e ~ rest ~ Some(post) =>
          PostOpExpr(post, (e /: rest){ case (e1, op ~ e2) => BinOpExpr(op, e1, e2) })
        case e ~ rest ~ None =>
          (e /: rest){ case (e1, op ~ e2) => BinOpExpr(op, e1, e2) }
      }
    )

  lazy val infixExpr: Parser[Expr] =
    ( prefixExpr ~ (id ~ prefixExpr *) ^^ {
        case eFirst ~ eRest => handlePrecedence(eFirst, eRest)
      }
    )
        
  lazy val prefixExpr: Parser[Expr] =
    ( Token(OP, "-") ~ simpleExpr ^^ {
        case op ~ expr => UnOpExpr(op, expr)
      }
    | Token(OP, "+") ~ simpleExpr ^^ {
        case op ~ expr => UnOpExpr(op, expr)
      }
    | Token(OP, "~") ~ simpleExpr ^^ {
        case op ~ expr => UnOpExpr(op, expr)
      }
    | Token(OP, "!") ~ simpleExpr ^^ {
        case op ~ expr => UnOpExpr(op, expr)
      }
    | simpleExpr
    )

  lazy val simpleExpr: Parser[Expr] =
    ( NEW ~> classTemplate ^^ { //TODO what if stms is not Nil? Then create an anonymous subclass, and do a classExpr on that
        case Tuple3(ClassInstance(name, args), types, stms) => ClassExpr(name, args)
      }
    | blockExpr
    | simpleExpr1 <~ UNDER // TODO mark this somehow?
    | simpleExpr1
    )

  lazy val simpleExpr1: Parser[Expr] = 
    ( simpleExpr1First ~ (simpleExpr1Rest *) ^^ {
        case e ~ rest => (e /: rest)((acc, f) => f(acc))
      }
    )
    
  lazy val simpleExpr1First: Parser[Expr] =
    ( literal
    | path ^^ {
        case p => fieldSplit(VarExpr(p.head), p.tail)
      }
    | LPAREN ~> exprs <~ RPAREN ^^ {
        case es => if (es.length == 1) es.head else null // TODO handle tuples
      }
    | LPAREN ~ RPAREN ^^ {
        case _ => null // TODO
      }
    )
    
  lazy val simpleExpr1Rest: Parser[Expr => Expr] =
    ( argumentExprs ^^ {
        case args => (e: Expr) => FunExpr(e, args)
      }
    | DOT ~> id ^^ {
        case i => (e: Expr) => FieldSelectionExpr(e, i)
      }
    | typeArgs ^^ {
        case _ => null // TODO
      }
    )

  lazy val exprs: Parser[List[Expr]] =
    ( expr ~ (COMMA ~> exprs) ^^ {
        case e ~ es => e :: es
      }
    | expr ^^ {
        case e => List(e)
      }
    )

  lazy val argumentExprs: Parser[List[Expr]] =
    ( LPAREN ~> exprs <~ RPAREN ^^ {
        case es => es
      }
    | LPAREN ~ RPAREN ^^ {
        case _ ~ _ => Nil
      }
    | blockExpr ^^ {
        case be => List(be)
      }
    )

  lazy val blockExpr: Parser[Expr] =
    ( LBRACE ~> caseClauses <~ RBRACE ^^ {
        case _ => null // TODO
      }
    | LBRACE ~> block <~ RBRACE ^^ {
        case stmt => BlockExpr(stmt)
      }
    )

  lazy val block: Parser[List[Stmt]] =
    ( (blockStat <~ SEMI *) ~ (expr ?) ^^ {
        case stmts ~ Some(e) => stmts ++ List(e)
        case stmts ~ None => stmts
      }
    )
    
  lazy val blockStat: Parser[Stmt] =
    ( importG ^^ {
        case _ => null // TODO
      }
    | IMPLICIT ~ defG ^^ {
        case _ => null // TODO
      }
    | LAZY ~ defG ^^ {
        case _ => null // TODO
      }
    | defG
    | localModifier ~ (localModifier *) ~ tmplDef ^^ {
        case _ => null // TODO
      }
    | expr1
    | success("") ^^ {
        case _ => EmptyStmt
      }
    )

  lazy val enumerators: Parser[Expr] =
    ( generator ~ enumeratorsH ^^ {
        case _ => null // TODO
      }
    )
    
  lazy val enumeratorsH: Parser[List[Expr]] =
    ( SEMI ~> enumerator ~ enumeratorsH ^^ {
        case _ => null // TODO
      }
    | SEMI ~> enumerator ^^ {
        case _ => null // TODO
      }
    )

  lazy val enumerator: Parser[Expr] =
    ( generator ^^ {
        case _ => null // TODO
      }
    | guard ^^ {
        case _ => null // TODO
      }
    | VAL ~> pattern1 ~ (EQUAL ~> expr) ^^ {
        case _ => null // TODO
      }
    )

  lazy val generator: Parser[Expr] =
    ( pattern1 ~ (WORRA ~> expr) ^^ {
        case _ => null // TODO
      }
    )

  lazy val caseClauses: Parser[List[Expr]] =
    ( caseClause ~ caseClauses ^^ {
        case _ => null // TODO
      }
    | caseClause ^^ {
        case _ => null // TODO
      }
    )

  lazy val caseClause: Parser[Expr] =
    ( CASE ~> pattern ~ (guard ?) ~ (ARROW ~> block) ^^ {
        case _ => null // TODO
      }
    )

  lazy val guard: Parser[Expr] =
    ( IF ~> postfixExpr ^^ {
        case _ => null // TODO
      }
    )

  lazy val pattern: Parser[Expr] =
    ( pattern1 ~ (Token(OP, "|") ~> pattern1 *) ^^ {
        case _ => null // TODO
      }
    )

  lazy val pattern1: Parser[Expr] =
    ( pattern2 ^^ {
        case _ => null // TODO
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
        case _ => null // TODO
      }
    | simplePattern
    )

  lazy val simplePattern: Parser[String] =
    ( UNDER ^^ {
        case _ => null // TODO
      }
    | varid ^^ {
        case _ => null // TODO
      }
    | literal ^^ {
        case _ => null // TODO
      }
    | qualId ^^ {
        case _ => null // TODO
      }
    | qualId ~ (LPAREN ~> patterns <~ RPAREN) ^^ {
        case _ => null // TODO
      }
    | qualId <~ LPAREN ~ RPAREN ^^ {
        case _ => null // TODO
      }
    | LPAREN ~> patterns <~ RPAREN ^^ {
        case _ => null // TODO
      }
    )

  lazy val patterns: Parser[String] =
    ( pattern ~ (COMMA ~> pattern *) ^^ {
        case _ => null // TODO
      }
    )

  lazy val paramClauses: Parser[List[List[ParamDclStmt]]] =
    ( paramClause *
    )

  lazy val paramClause: Parser[List[ParamDclStmt]] =
    ( LPAREN ~> params <~ RPAREN
    | LPAREN ~ RPAREN ^^ {
        case _ => Nil
      }
    )

  lazy val params: Parser[List[ParamDclStmt]] =
    ( param ~ (COMMA ~> param *) ^^ {
        case p ~ ps => p :: ps
      }
    )

  lazy val param: Parser[ParamDclStmt] =
    ( id ~ (COLON ~> paramType) ^^ {
        case i ~ t => ParamDclStmt(i, t)
      }
    )

  lazy val paramType: Parser[Type] =
    ( typeG
    | ARROW ~ typeG ^^ {
        case _ => null
      }
    )

  lazy val bindings: Parser[List[ParamDclStmt]] =
    ( LPAREN ~> binding ~ bindingsH <~ RPAREN ^^ {
        case arg ~ args => arg :: args
      }
    | LPAREN ~> binding <~ RPAREN ^^ {
        case arg => List(arg)
      }
    )
    
  lazy val bindingsH: Parser[List[ParamDclStmt]] =
    ( COMMA ~> binding ~ bindingsH ^^ {
        case arg ~ args => arg :: args
      }
    | COMMA ~> binding ^^ {
        case arg => List(arg)
      }
    )

  lazy val binding: Parser[ParamDclStmt] =
    ( id ~ (COLON ~> typeG) ^^ {
        case arg ~ typeG => ParamDclStmt(arg, typeG)
      }
    | UNDER ~ (COLON ~> typeG) ^^ {
        case _ => null // TODO
      }
    | UNDER ^^ {
        case _ => null // TODO
      }
    )

  lazy val modifier: Parser[Expr] =
    ( localModifier ^^ {
        case _ => null // TODO
      }
    | accessModifier ^^ {
        case _ => null // TODO
      }
    | OVERRIDE ^^ {
        case _ => null // TODO
      }
    )

  lazy val localModifier: Parser[Expr] =
    ( SEALED ^^ {
        case _ => null // TODO
      }
    | IMPLICIT ^^ {
        case _ => null // TODO
      }
    | LAZY ^^ {
        case _ => null // TODO
      }
    )

  lazy val accessModifier: Parser[Expr] =
    ( PRIVATE ^^ {
        case _ => null // TODO
      }
    )

  lazy val templateBody: Parser[List[Stmt]] =
    ( LBRACE ~> selfType ~ templateStats <~ RBRACE ^^ {
        case _ => null // TODO
      }
    | LBRACE ~> templateStats <~ RBRACE
    )

  lazy val templateStats: Parser[List[Stmt]] =
    ( templateStat ~ (SEMI ~> templateStat *) ^^ {
        case stat ~ stats => stat :: stats
      }
    )

  lazy val templateStat: Parser[Stmt] =
    ( importG ^^ {
        case _ => null // TODO
      }
    | modifier ~ defG ^^ {
        case _ => null // TODO
      }
    | defG
    | modifier ~ dcl ^^ {
        case _ => null // TODO
      }
    | dcl ^^ {
        case _ => null // TODO
      }
    | expr
    | success("") ^^ {
        case _ => EmptyStmt
      }
    )

  lazy val selfType: Parser[Expr] =
    ( id <~ ARROW ^^ {
        case _ => null // TODO
      }
    )

  lazy val importG: Parser[Expr] =
    ( IMPORT ~> importExpr ^^ {
        case _ => null // TODO
      }
    )

  lazy val importExpr: Parser[Expr] =
    ( qualId ~ (DOT ~> id) ^^ {
        case _ => null // TODO
      }
    | qualId ~ (DOT ~> UNDER) ^^ {
        case _ => null // TODO
      }
    )

  lazy val dcl: Parser[Expr] =
    ( VAL ~ valDcl ^^ {
        case _ => null // TODO
      }
    | VAR ~ varDcl ^^ {
        case _ => null // TODO
      }
    | DEF ~ funDcl ^^ {
        case _ => null // TODO
      }
    )

  lazy val valDcl: Parser[Expr] =
    ( ids ~ (COLON ~> typeG) ^^ {
        case _ => null // TODO
      }
    )

  lazy val varDcl: Parser[Expr] =
    ( ids ~ (COLON ~ typeG) ^^ {
        case _ => null // TODO
      }
    )

  lazy val funDcl: Parser[Expr] =
    ( funSig ~ (COLON ~> typeG) ^^ {
        case _ => null // TODO
      }
    )

  lazy val funSig: Parser[(String, List[List[ParamDclStmt]])] =
    ( id ~ paramClauses ^^ {
        case i ~ ps => (i, ps)
      }
    )

    //Called def in the grammar (which is a reserved word, so I used defG)
  lazy val defG: Parser[Stmt] =
    ( VAL ~> patDef ^^ {
        case DefWrapper(pats, typeG, expr) => ValDefStmt(pats, typeG, expr, "val")
      }
    | VAR ~> varDef ^^ {
        case DefWrapper(pats, typeG, expr) => ValDefStmt(pats, typeG, expr, "var")
      }
    | DEF ~> funDef ^^ {
        case FunWrapper(name, paramClauses, retType, body) => FunDefStmt(name, paramClauses, retType, body)
      }
    | tmplDef
    )

  lazy val patDef: Parser[DefWrapper] =
    ( pattern2 ~ (COMMA ~> pattern2 *) ~ (COLON ~> typeG) ~ (EQUAL ~> expr) ^^ {
        case pat ~ pats ~ typeG ~ expr => DefWrapper(pat :: pats, typeG, expr)
      }
    )

  lazy val varDef: Parser[DefWrapper] =
    ( patDef
    )

  lazy val funDef: Parser[FunWrapper] =
    ( funSig ~ (COLON ~> typeG) ~ (EQUAL ~> expr) ^^ {
        case (name, paramClauses) ~ retType ~ body => FunWrapper(name, paramClauses, retType, body)
      }
    )

  lazy val tmplDef: Parser[Stmt] =
        //Assemble the classDefStmt Here
    ( CASE ~ CLASS ~> classDef ^^ {
        case Tuple3(name, args, Tuple3(whatExtends, extendsWith, body)) =>
          ClassDefStmt("case class", name, args, whatExtends, extendsWith, body)
      }
    | CLASS ~> classDef ^^ {
        case Tuple3(name, args, Tuple3(whatExtends, extendsWith, body)) =>
          ClassDefStmt("class", name, args, whatExtends, extendsWith, body)
      }
    | CASE ~ OBJECT ~> objectDef ^^ {
        case Tuple3(name, args, Tuple3(whatExtends, extendsWith, body)) =>
          ClassDefStmt("case object", name, args, whatExtends, extendsWith, body)
      }
    | OBJECT ~> objectDef ^^ {
        case Tuple3(name, args, Tuple3(whatExtends, extendsWith, body)) =>
          ClassDefStmt("object", name, args, whatExtends, extendsWith, body)
      }
    | TRAIT ~> traitDef ^^ {
        case Tuple2(name, Tuple3(whatExtends, extendsWith, body)) =>
          ClassDefStmt("trait", name, Nil, whatExtends, extendsWith, body)
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
    ( EXTENDS ~> classTemplate
    | templateBody ^^ {
        case stmts => (ClassInstance(BaseType("AnyRef"), Nil), Nil, stmts)
      }
    | success("") ^^ {
        case _ => (ClassInstance(BaseType("AnyRef"), Nil), Nil, Nil)
      }
    )

  lazy val traitTemplateOpt: Parser[(ClassInstance, List[Type], List[Stmt])] =
    ( EXTENDS ~> traitTemplate
    | templateBody ^^ {
        case stmts => (ClassInstance(BaseType("AnyRef"), Nil), Nil, stmts)
      }
    | success("") ^^ {
        case _ => (ClassInstance(BaseType("AnyRef"), Nil), Nil, Nil)
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
    ( constr ~ (WITH ~> simpleType *) ^^ {
        case (name, args) ~ types => (ClassInstance(name, args), types)
      }
    )

  lazy val traitParents: Parser[(ClassInstance, List[Type])] =
    ( simpleType ~ (WITH ~> simpleType *) ^^ {
        case name ~ types => (ClassInstance(name, Nil), types)
      }
    )

  lazy val constr: Parser[(Type, List[Expr])] =
    ( simpleType ~ (LPAREN ~> exprs <~ RPAREN) ^^ {
        case simpleType ~ args => (simpleType, args)
      }
    | simpleType <~ LPAREN ~ RPAREN ^^ {
        case simpleType => (simpleType, Nil)
      }
    | simpleType ^^ { // TODO do these need to be distinguished?
        case simpleType => (simpleType, Nil)
      }
    )

  lazy val topStatSeq: Parser[List[Stmt]] =
    ( topStat ~ (SEMI ~> topStat *) ^^ {
        case s ~ ss => s :: ss
      }
    )

  lazy val topStat: Parser[Stmt] =
    ( modifier ~ tmplDef ^^ {
        case _ => null // TODO
      }
    | tmplDef
    | importG ^^ {
        case _ => null // TODO
      }
    | success("") ^^ {
        case _ => EmptyStmt
      }
    )

  lazy val compilationUnit: Parser[Expr] =
    ( PACKAGE ~ qualId ~ SEMI ~ topStatSeq ^^ {
        case _ => null // TODO
      }
    | topStatSeq ^^ {
        case _ => null // TODO
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
    if (isAssignment(operator)) return -1 // Lowest precedence of all
    val c = operator(0)
    precedence.get(c) match {
      case Some(p) => p
      case None => if (c.isLetter) 0 else 9
    }
  }
  
  def isLeftAssociative(operator: String): Boolean = {
    val c = operator(operator.length - 1)
    c != ':'
  }
  
  def isAssignment(operator: String): Boolean = {
    operator(operator.length - 1) == '=' &&
      operator(0) != '=' &&
      operator != "<=" &&
      operator != ">=" &&
      operator != "!="
  }

  def fieldSplit(base: Expr, fields: List[String]): Expr = fields match {
    case Nil => base
    case f :: fs => fieldSplit(FieldSelectionExpr(base, f), fs)
  }
}
