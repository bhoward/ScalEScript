import scala.util.matching.Regex
trait Parsers {
  sealed trait Result[+A]
  case class Success[+A](value: A, rem: String) extends Result[A]
  case class Failure(msg: String) extends Result[Nothing]
  
  case class ~[+A, +B](a: A, b: B)
  
  trait Parser[+A] extends (String => Result[A]) { outer =>
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
    
    def ^^[B](f: A => B): Parser[B] = new Parser[B] {
      def apply(s: String): Result[B] = outer(s) match {
        case Success(v, rem) => Success(f(v), rem)
        case f: Failure => f
      }
    }
}
  
  class KeywordParser(str: String) extends Parser[String] {
    def apply(s: String): Result[String] = {
      if (s.startsWith(str)) {
        Success(str, s.substring(str.length))
      } else {
        Failure("Expected '%s' got '%s'".format(str, s.substring(0, str.length)))
      }
    }
  }
  
  class SequenceParser[+A, +B](l: => Parser[A], r: => Parser[B]) extends Parser[A ~ B] {
    lazy val left = l
    lazy val right = r
    
    def apply(s: String): Result[A ~ B] = left(s) match {
      case Success(a, rem) => right(rem) match {
        case Success(b, rem) => Success(new ~(a, b), rem)
        case f: Failure => f
      }
      case f: Failure => f
    }
  }
  
  class DisParser[+A](l: => Parser[A], r: => Parser[A]) extends Parser[A] {
    lazy val left = l
    lazy val right = r
    
    def apply(s: String): Result[A] = left(s) match {
      case res: Success[A] => res
      case _: Failure => right(s)
    }
  }
  
  def success[A](v: A) = new Parser[A] {
    def apply(s: String): Result[A] = Success(v, s)
  }
}

trait RegexParsers extends Parsers {
  implicit def keyword(str: String): Parser[String] = new KeywordParser(str)
  
  implicit def regex(r: Regex): Parser[String] = new Parser[String] {
    def apply(s: String): Result[String] = {
      r.findPrefixOf(s) match {
        case Some(str) => Success(str, s.substring(str.length))
        case None => Failure("Expected '%s' got '%s'".format(r, s))
      }
    }
  }
}
