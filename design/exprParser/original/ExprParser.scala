object ExprParser extends RegexParsers {
  val NUM = """[1-9]\d*|0""".r
  val ADDOP = """[-+]""".r
  val MULOP = """[*/]""".r
  
  def expr: Parser[Expression] =
    term ~ (ADDOP ~ term).* ^^ { case t ~ rest => rest.foldLeft(t){
      case (e, "+" ~ t) => Sum(e, t)
      case (e, "-" ~ t) => Difference(e, t)
    } }
  
  def term: Parser[Expression] =
    factor ~ (MULOP ~ factor).* ^^ { case f ~ rest => rest.foldLeft(f){
      case (e, "*" ~ t) => Product(e, t)
      case (e, "/" ~ t) => Quotient(e, t)
    } }
  
  def factor: Parser[Expression] =
    ( "(" ~> expr <~ ")"
    | NUM ^^ { case n => Constant(n.toInt) }
    )
}