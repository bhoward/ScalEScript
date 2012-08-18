package scalescript

sealed trait TokenType
case object EOF extends TokenType
case object LID extends TokenType // starts with lower
case object UID extends TokenType // starts with upper, $, or _
case object INT extends TokenType
case object DOUBLE extends TokenType
case object STRING extends TokenType
case object CHAR extends TokenType
case object OP extends TokenType
case object ASSIGN extends TokenType // op=
case object LPAREN extends TokenType // (
case object RPAREN extends TokenType // )
case object LBRACK extends TokenType // [
case object RBRACK extends TokenType // ]
case object LBRACE extends TokenType // {
case object RBRACE extends TokenType // }
case object DOT extends TokenType // .
case object COMMA extends TokenType // ,
case object SEMI extends TokenType // ;
case object COLON extends TokenType // :
case object EQUAL extends TokenType // =
case object ARROW extends TokenType // =>
case object WORRA extends TokenType // <-
case object SUBT extends TokenType // <:
case object SUPERT extends TokenType // >:
case object VIEWT extends TokenType // <%
case object HASH extends TokenType // #
case object AT extends TokenType // @
case object UNDER extends TokenType // _
case object ABSTRACT extends TokenType
case object CASE extends TokenType
case object CATCH extends TokenType
case object CLASS extends TokenType
case object DO extends TokenType
case object ELSE extends TokenType
case object EXTENDS extends TokenType
case object DEF extends TokenType
case object FALSE extends TokenType
case object FINAL extends TokenType
case object FINALLY extends TokenType
case object FOR extends TokenType
case object FORSOME extends TokenType
case object IF extends TokenType
case object IMPLICIT extends TokenType
case object IMPORT extends TokenType
case object LAZY extends TokenType
case object MATCH extends TokenType
case object NEW extends TokenType
case object NULL extends TokenType
case object OBJECT extends TokenType
case object OVERRIDE extends TokenType
case object PACKAGE extends TokenType
case object PRIVATE extends TokenType
case object PROTECTED extends TokenType
case object RETURN extends TokenType
case object SEALED extends TokenType
case object SUPER extends TokenType
case object THIS extends TokenType
case object THROW extends TokenType
case object TRAIT extends TokenType
case object TRY extends TokenType
case object TRUE extends TokenType
case object TYP extends TokenType // calling it TYPE crashes sbt...
case object VAL extends TokenType
case object VAR extends TokenType
case object WHILE extends TokenType
case object WITH extends TokenType
case object YIELD extends TokenType

case class Token(ttype: TokenType, lexeme: String)

object Token {
  val whitespace =
    """(\s|(//(?d:.)*\n)|(/\*(?s:.)*?\*/))+""".r

  val id =
    """[A-Za-z0-9$_]+(_[!#%&*+\-/:<=>?@\\^|~]+)?""".r

  val integerLiteral =
    """\d+""".r

  val floatingPointLiteral =
    """(\d+\.(\d*)?|\d*\.\d+)([eE][+-]?\d+)?|\d+([eE][+-]?\d+)""".r

  val stringLiteral =
    ("\"" + """([^"\p{Cntrl}\\]|\\[\\"'bfnrt])*""" + "\"").r

  val multilineStringLiteral =
    ("\"\"\"" + """("?"?[^"])*"*""" + "\"\"\"").r

  val characterLiteral =
    ("\'" + """([^\p{Cntrl}\\]|\\[\\"'bfnrt])""" + "\'").r

  val op =
    """[!#%&*+\-/:<=>?@\\^|~]+""".r

  val keyword = Map(
    "_" -> UNDER,
    "abstract" -> ABSTRACT,
    "case" -> CASE,
    "catch" -> CATCH,
    "class" -> CLASS,
    "do" -> DO,
    "else" -> ELSE,
    "extends" -> EXTENDS,
    "def" -> DEF,
    "false" -> FALSE,
    "final" -> FINAL,
    "finally" -> FINALLY,
    "for" -> FOR,
    "forSome" -> FORSOME,
    "if" -> IF,
    "implicit" -> IMPLICIT,
    "import" -> IMPORT,
    "lazy" -> LAZY,
    "match" -> MATCH,
    "new" -> NEW,
    "null" -> NULL,
    "object" -> OBJECT,
    "override" -> OVERRIDE,
    "package" -> PACKAGE,
    "private" -> PRIVATE,
    "protected" -> PROTECTED,
    "return" -> RETURN,
    "sealed" -> SEALED,
    "super" -> SUPER,
    "this" -> THIS,
    "throw" -> THROW,
    "trait" -> TRAIT,
    "try" -> TRY,
    "true" -> TRUE,
    "type" -> TYP,
    "val" -> VAL,
    "var" -> VAR,
    "while" -> WHILE,
    "with" -> WITH,
    "yield" -> YIELD)

  val special = Map(
    ":" -> COLON,
    "=" -> EQUAL,
    "=>" -> ARROW,
    "<-" -> WORRA,
    "<:" -> SUBT,
    ">:" -> SUPERT,
    "<%" -> VIEWT,
    "#" -> HASH,
    "@" -> AT,
    "<=" -> OP,
    ">=" -> OP,
    "!=" -> OP)

  def lex(s: String): (Token, String) = {
    if (s == "") {
      (Token(EOF, ""), "")
    } else {
      // Strip leading whitespace
      whitespace.findPrefixOf(s) match {
        case Some(ws) => return lex(s.substring(ws.length))
        case None => // do nothing
      }

      if (s(0).isLetter || s(0) == '$' || s(0) == '_') {
        getIdOrKeyword(s)
      } else if (s(0).isDigit || (s(0) == '.' && s(1).isDigit)) {
        getNumLiteral(s)
      } else s(0) match {
        case '"' => getStringLiteral(s)
        case ''' => getCharLiteral(s)
        case '(' => (Token(LPAREN, "("), s.substring(1))
        case ')' => (Token(RPAREN, ")"), s.substring(1))
        case '[' => (Token(LBRACK, "["), s.substring(1))
        case ']' => (Token(RBRACK, "]"), s.substring(1))
        case '{' => (Token(LBRACE, "{"), s.substring(1))
        case '}' => (Token(RBRACE, "}"), s.substring(1))
        case ',' => (Token(COMMA, ","), s.substring(1))
        case '.' => (Token(DOT, "."), s.substring(1))
        case ';' => (Token(SEMI, ";"), s.substring(1))
        case _ => getOp(s)
      }
    }
  }

  // pre: starts with letter, $, or _
  def getIdOrKeyword(s: String): (Token, String) = {
    id.findPrefixOf(s) match {
      case Some(lexeme) =>
        val toktype = keyword.getOrElse(lexeme, if (lexeme(0).isLower) LID else UID)
        (Token(toktype, lexeme), s.substring(lexeme.length))
      case None => throw new Exception("Failed to match identifier or keyword") // should not happen
    }
  }

  // pre: s starts with a digit
  def getNumLiteral(s: String): (Token, String) = {
    floatingPointLiteral.findPrefixOf(s) match {
      case Some(lexeme) => (Token(DOUBLE, lexeme), s.substring(lexeme.length))
      case None =>
        integerLiteral.findPrefixOf(s) match {
          case Some(lexeme) => (Token(INT, lexeme), s.substring(lexeme.length))
          case None => throw new Exception("Failed to match numeric literal") // should not happen
        }
    }
  }

  // pre: s starts with "
  def getStringLiteral(s: String): (Token, String) = {
    if (s.startsWith("\"\"\"")) {
      multilineStringLiteral.findPrefixOf(s) match {
        case Some(literal) => (Token(STRING, deTriquotify(literal)), s.substring(literal.length))
        case None => throw new Exception("Invalid multiline string literal")
      }
    } else {
      stringLiteral.findPrefixOf(s) match {
        case Some(literal) => (Token(STRING, deQuotify(literal)), s.substring(literal.length))
        case None => throw new Exception("Invalid string literal")
      }
    }
  }

  // pre: s starts with '
  def getCharLiteral(s: String): (Token, String) = {
    characterLiteral.findPrefixOf(s) match {
      case Some(literal) => (Token(CHAR, deQuotify(literal)), s.substring(literal.length))
      case None => throw new Exception("Invalid character literal")
    }

    // TODO also recognize symbols here
  }

  // pre: starts with misc. char
  def getOp(s: String): (Token, String) = {
    op.findPrefixOf(s) match {
      case Some(lexeme) =>
        val toktype = special.getOrElse(lexeme,
            if (lexeme(0) != '=' && lexeme(lexeme.length - 1) == '=') ASSIGN else OP)
        (Token(toktype, lexeme), s.substring(lexeme.length))
      case None => throw new Exception("Unrecognized token")
    }
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
}