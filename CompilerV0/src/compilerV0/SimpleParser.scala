package compilerV0

import scala.util.parsing.combinator._
import java.io.Reader

trait SimpleParser[AST] extends RegexParsers with PackratParsers {
  type P[T] = PackratParser[T]
  
  /**
   * This is the starting non-terminal for the grammar
   */
  def top: Parser[AST]
    
  def apply(source: String): AST = parseAll(top, source) match {
    case Success(result, _) => result
    case ns: NoSuccess => throw new Exception(ns.msg)
  }

  def apply(source: Reader): AST = parseAll(top, source) match {
    case Success(result, _) => result
    case ns: NoSuccess => throw new Exception(ns.msg)
  }
}

trait JavaComments { this: RegexParsers =>
  override val whiteSpace = """(\s|(//.*\n)|(/\*(?s:.)*?\*/))*""".r
}

trait CommonLiterals { this: RegexParsers =>
  def ident: Parser[String] =
    """[a-zA-Z_]\w*""".r
    
  def wholeNumber: Parser[String] =
    """\d+""".r
    
  def floatingPointNumber: Parser[String] =
    """(\d+\.(\d*)?|\d*\.\d+)([eE][+-]?\d+)?""".r
  
  /**
   * A string literal may directly contain printable characters except " or \:
   * - to get a \, use \\
   * - to get a ", use \"
   * - to get backspace, use \b
   * - to get formfeed, use \f
   * - to get newline, use \n
   * - to get carriage return, use \r
   * - to get tab, use \t
   */
  def stringLiteral: Parser[String] =
    ("\"" + """([^"\p{Cntrl}\\]|\\[\\"bfnrt])*""" + "\"").r
    
  /**
   * Remove surrounding quotes, and replace escaped characters in a string literal.
   * Does no error checking.
   * TODO support octal and hex character codes
   * 
   * @param s
   */
  def unquote(s: String): String = {
    val buf = new StringBuilder
    var i = 1
    while (i < s.length - 1) {
      s.charAt(i) match {
        case '\\' => s.charAt(i+1) match {
          case '\\' => buf.append('\\'); i += 1
          case '"' => buf.append('"'); i += 1
          case 'b' => buf.append('\b'); i += 1
          case 'f' => buf.append('\f'); i += 1
          case 'n' => buf.append('\n'); i += 1
          case 'r' => buf.append('\r'); i += 1
          case 't' => buf.append('\t'); i += 1
          case c => buf.append(c); i += 1
        }
        
        case c => buf.append(c)
      }
      i += 1
    }
    buf.toString
  }
}
