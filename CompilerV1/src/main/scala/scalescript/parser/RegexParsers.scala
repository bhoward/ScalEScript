package scalescript.parser

import scala.util.matching.Regex

/**
 * Just enough of the standard combinator parsers for our use.
 */
trait RegexParsers {
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
      
    def ? : Parser[Option[A]] =
      ( this ^^ { case a => Some(a) }
      | success(None)
      )
    
    def ^^[B](f: A => B): Parser[B] = new Parser[B] {
      def apply(s: String): Result[B] = outer(s) match {
        case Success(v, rem) => Success(f(v), rem)
        case f: Failure => f
      }
    }
    
    /**
     * p - q is successful if p is, unless q also succeeds with the same value
     */
    def -[B >: A](that: => Parser[B]): Parser[B] = new Parser[B] {
      def apply(s: String): Result[B] = outer(s) match {
        case Success(v, rem) => that(s) match {
          case Success(v2, rem2) =>
            if (v == v2) {
              Failure("'%s' not allowed".format(v))
            } else {
              Success(v, rem)
            }
          case _ => Success(v, rem)
        }
        case f: Failure => f
      }
    }
  }
  
  val whitespace = """\s+""".r
  
  def skipWhitespace(s: String): String = {
    whitespace.findPrefixOf(s) match {
      case Some(pre) => s.substring(pre.length)
      case None => s
    }
  }

  implicit def keyword(str: String): Parser[String] = new Parser[String] {
    def apply(s0: String): Result[String] = {
      val s = skipWhitespace(s0)
      if (s.startsWith(str)) {
        Success(str, s.substring(str.length))
      } else {
        Failure("Expected '%s' got '%s'".format(str, s.substring(0, str.length)))
      }
    }
  }
  
  implicit def regex(r: Regex): Parser[String] = new Parser[String] {
    def apply(s0: String): Result[String] = {
      val s = skipWhitespace(s0)
      r.findPrefixOf(s) match {
        case Some(str) => Success(str, s.substring(str.length))
        case None => Failure("Expected '%s' got '%s'".format(r, s))
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
    
    def apply(s: String): Result[A] = {
      val res = left(s)
      res match {
        case Success(_, _) => res
        case _ => right(s)
      }
    }
  }
  
  def success[A](v: A) = new Parser[A] {
    def apply(s: String): Result[A] = Success(v, s)
  }
  
  def failure(msg: String) = new Parser[Nothing] {
    def apply(s: String): Result[Nothing] = Failure(msg)
  }
  
  def phrase[A](p: Parser[A]) = new Parser[A] {
    def apply(s: String): Result[A] = p.apply(s) match {
      case Success(a, rem) =>
        if (rem == "") {
          Success(a, "")
        } else {
          Failure("Expected end of input, got '%s...'".format(rem.substring(0, 10)))
        }
      case f: Failure => f
    }
  }

  def parseAll[A](p: Parser[A], s: String): Result[A] =
    phrase(p <~ """\z""".r).apply(s)
}
