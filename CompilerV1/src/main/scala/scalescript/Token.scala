package scalescript

sealed trait TokenType
case object EOF extends TokenType
case object ID extends TokenType
case object INT extends TokenType
case object DOUBLE extends TokenType
case object STRING extends TokenType
case object CHAR extends TokenType
case object OP extends TokenType
case object ASSIGN extends TokenType

case class Token(ttype: TokenType, lexeme: String)

object Token {
  val whitespace =
    """(\s|(//(?d:.)*\n)|(/\*(?s:.)*?\*/))*""".r

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

  def lex(s: String): (Token, String) = {
    if (s == "") {
      (Token(EOF, ""), "")
    } else {
      // Strip leading whitespace
      whitespace.findPrefixOf(s) match {
        case Some(ws) => return Token.lex(s.substring(ws.length))
        case None =>
      }

      if (s(0).isLetter) {
        getIdOrKeyword(s)
      } else if (s(0).isDigit || (s(0) == '.' && s(1).isDigit)) {
        getNumLiteral(s)
      } else if (s(0) == '"') {
        getStringLiteral(s)
      } else if (s(0) == ''') {
        getCharLiteral(s)
      } else {
        getOp(s)
      }
    }
  }

  def getIdOrKeyword(s: String): (Token, String) = null

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

  def getOp(s: String): (Token, String) = null

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