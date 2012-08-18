package scalescript

class TokenStream(s: String) {
  lazy val headTail = Token.lex(s)
  lazy val head: Token = headTail._1
  lazy val tail: TokenStream = new TokenStream(headTail._2)
  def isEmpty: Boolean = head.ttype == EOF
}