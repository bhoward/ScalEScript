package scalescript

/**
 * Re-implementation of string-based combinator parser to have a separate lexing step.
 * 
 * @author Brian Howard
 */
trait TokenParsers {
  sealed trait Result[+A]
  case class Success[+A](value: A, rem: TokenStream) extends Result[A]
  case class Failure(msg: String) extends Result[Nothing]
  
  case class ~[+A, +B](a: A, b: B)
  
  trait Parser[+A] extends (TokenStream => Result[A]) { outer =>
    def ~[B](that: => Parser[B]): Parser[A ~ B] = new SequenceParser(this, that)
    
    def |[B >: A](that: => Parser[B]): Parser[B] = new DisParser(this, that)
    
    def ~>[B](that: => Parser[B]): Parser[B] =
      this ~ that ^^ { case a ~ b => b }
    
    def <~[B](that: => Parser[B]): Parser[A] =
      this ~ that ^^ { case a ~ b => a }
    
    def * : Parser[List[A]] =
      ( this ~ * ^^ { case a ~ b => a :: b }
      | success(Nil)
      )
      
    def ? : Parser[Option[A]] =
      ( this ^^ { case a => Some(a) }
      | success(None)
      )
    
    def ^^[B](f: A => B): Parser[B] = new Parser[B] {
      def apply(ts: TokenStream): Result[B] = outer(ts) match {
        case Success(v, rem) => Success(f(v), rem)
        case f: Failure => f
      }
    }
  }
  
  implicit def token(tt: TokenType): Parser[Token] = new Parser[Token] {
    def apply(ts: TokenStream): Result[Token] = {
      if (ts.head.ttype == tt) {
        Success(ts.head, ts.tail)
      } else {
        Failure("Expected '%s' got '%s'".format(tt, ts.head.lexeme))
      }
    }
  }

  class SequenceParser[+A, +B](l: => Parser[A], r: => Parser[B]) extends Parser[A ~ B] {
    lazy val left = l
    lazy val right = r
    
    def apply(ts: TokenStream): Result[A ~ B] = left(ts) match {
      case Success(a, rem) => right(rem) match {
        case Success(b, rem2) => Success(new ~(a, b), rem2)
        case f: Failure => f
      }
      case f: Failure => f
    }
  }
  
  class DisParser[+A](l: => Parser[A], r: => Parser[A]) extends Parser[A] {
    lazy val left = l
    lazy val right = r
    
    def apply(ts: TokenStream): Result[A] = {
      val res = left(ts)
      res match {
        case Success(_, _) => res
        case _ => right(ts)
      }
    }
  }
  
  def success[A](v: A) = new Parser[A] {
    def apply(ts: TokenStream): Result[A] = Success(v, ts)
  }
  
  def failure(msg: String) = new Parser[Nothing] {
    def apply(ts: TokenStream): Result[Nothing] = Failure(msg)
  }
  
  def phrase[A](p: Parser[A]) = new Parser[A] {
    def apply(ts: TokenStream): Result[A] = p.apply(ts) match {
      case Success(a, rem) =>
        if (rem.isEmpty) {
          Success(a, rem)
        } else {
          Failure("Expected end of input, got '%s...'".format(rem.head))
        }
      case f: Failure => f
    }
  }

  def parseAll[A](p: Parser[A], ts: TokenStream): Result[A] =
    phrase(p).apply(ts)
}
