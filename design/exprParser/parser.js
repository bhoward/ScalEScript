// By-Hand Translation of Scala Expression Parser/Evaluator
// Brian Howard, Summer 2012

// Original Scala source:
/*
// Option/Some/None come from the standard library, but they could be implemented like this:
sealed trait Option[+T] {
  def isEmpty: Boolean
  def get: T
  def map[U](f: T => U): Option[U]
}

case class Some[+T](value: T) extends Option[T] {
  def isEmpty: Boolean = false
  def get: T = value
  def map[U](f: T => U): Option[U] = Some(f(value))
}

case object None extends Option[Nothing] {
  def isEmpty: Boolean = true
  def get: Nothing = error("get from None")
  def map[U](f: Nothing => U): Option[U] = None
}

object Option {
  def apply[T](x: T): Option[T] =
    if (x == null) {
      None
    } else {
      Some(x)
    }
}

sealed trait Expression {
}

case class Constant(value: Double) extends Expression
case class Sum(left: Expression, right: Expression) extends Expression
case class Difference(left: Expression, right: Expression) extends Expression
case class Product(left: Expression, right: Expression) extends Expression
case class Quotient(left: Expression, right: Expression) extends Expression

object Expression {
  def eval(e: Expression): Double = e match {
    case Constant(value) => value
    case Sum(left, right) => eval(left) + eval(right)
    case Difference(left, right) => eval(left) - eval(right)
    case Product(left, right) => eval(left) * eval(right)
    case Quotient(left, right) => eval(left) / eval(right)
  }
}

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
  
  implicit def regex(r: scala.util.matching.Regex): Parser[String] = new Parser[String] {
    def apply(s: String): Result[String] = {
      r.findPrefixOf(s) match {
        case Some(str) => Success(str, s.substring(str.length))
        case None => Failure("Expected '%s' got '%s'".format(r, s))
      }
    }
  }
}
*/

var std = {};

std.Option = function(x) {
	if (this instanceof std.Option) {
		// constructor call with new
		// do nothing
	} else {
		// factory call without new
		if (x === null) { // and/or undefined?
			return std.None;
		} else {
			return std.Some(x);
		}
	}
}

std.Some = function(value) {
    if (this instanceof std.Some) {
        this.value = value;
    } else {
        return new std.Some(value);
    }
}
std.Some.prototype = new std.Option();
std.Some.prototype.constructor = std.Some;
// Can't put methods in prototype if they depend on constructor args
// (Alternately, save ctor args in hidden instance field for later use)
std.Some.prototype.toString = function() {
    return "Some(" + this.value + ")";
}
std.Some.prototype.isEmpty = function() {
    return false;
}
std.Some.prototype.get = function() {
    return this.value;
}
std.Some.prototype.map = function(f) {
    return std.Some(f(this.value));
}
std.Some.unapply = function(x) {
	return x; // special case: it already is Some(value) or None...
}

std.None = new std.Option();
std.None.toString = function() {
    return "None";
}
std.None.isEmpty = function() {
    return true;
}
std.None.get = function() {
    return undefined;
}
std.None.map = function(f) {
    return std.None;
}
std.None.unapply = function(x) {
	return x instanceof std.None;
}

std.List = function() {
	if (this instanceof std.List) {
		// constructor call with new
		// do nothing
	} else {
		// factory call without new
		var args = Array.prototype.slice.apply(arguments); // Special-case for varargs
		var result = std.Nil;
		while (args.length > 0) {
		    result = std["::"](args.pop(), result);
		}
		return result;
	}
}

std["::"] = function(head, tail) {
    if (this instanceof std["::"]) {
        this.head = head;
        this.tail = tail;
    } else {
        return new std["::"](head, tail);
    }
}
std["::"].prototype = new std.List();
std["::"].prototype.constructor = std["::"];
std["::"].prototype.toString = function() {
    return this.head + "::" + this.tail;
}
std["::"].prototype.isEmpty = function() {
    return false;
}
std["::"].prototype.map = function(f) {
    return std["::"](f(this.head), this.tail.map(f));
}
std["::"].unapply = function(x) {
	if (x isinstanceof std["::"]) {
	   return std.Some([head, tail]);
	} else {
	   return std.None;
	}
}

std.Nil = new std.List();
std.Nil.toString = function() {
    return "Nil";
}
std.Nil.isEmpty = function() {
    return true;
}
std.Nil.map = function(f) {
    return std.Nil;
}
std.Nil.unapply = function(x) {
	return x instanceof std.Nil;
}

var Expression = function() {
}

var Constant = function(value) {
	if (this instanceof Constant) {
	    // Use read-only properties for vals?
    	this.value = value;
    } else {
    	return new Constant(value);
    }
}
Constant.prototype = new Expression();
Constant.prototype.constructor = Constant;
Constant.prototype.toString = function() {
    return "Constant(" + this.value + ")";
}
Constant.unapply = function(x) {
    if (x instanceof Constant) {
        return std.Some(x.value);
    } else {
        return std.None;
    }
}

var Sum = function(left, right) {
	if (this instanceof Sum) {
    	this.left = left;
    	this.right = right;
    } else {
    	return new Sum(left, right);
    }
}
Sum.prototype = new Expression();
Sum.prototype.constructor = Sum;
Sum.prototype.toString = function() {
    return "Sum(" + this.left + ", " + this.right + ")";
}
Sum.unapply = function(x) {
    if (x instanceof Sum) {
        return std.Some([x.left, x.right]);
    } else {
        return std.None;
    }
}

var Difference = function(left, right) {
	if (this instanceof Difference) {
	    this.left = left;
    	this.right = right;
    } else {
    	return new Difference(left, right);
    }
}
Difference.prototype = new Expression();
Difference.prototype.constructor = Difference;
Difference.prototype.toString = function() {
    return "Difference(" + this.left + ", " + this.right + ")";
}
Difference.unapply = function(x) {
    if (x instanceof Difference) {
        return std.Some([x.left, x.right]);
    } else {
        return std.None;
    }
}

var Product = function(left, right) {
	if (this instanceof Product) {
	    this.left = left;
    	this.right = right;
    } else {
    	return new Product(left, right);
    }
}
Product.prototype = new Expression();
Product.prototype.constructor = Product;
Product.prototype.toString = function() {
    return "Product(" + this.left + ", " + this.right + ")";
}
Product.unapply = function(x) {
    if (x instanceof Product) {
        return std.Some([x.left, x.right]);
    } else {
        return std.None;
    }
}

var Quotient = function(left, right) {
	if (this instanceof Quotient) {
	    this.left = left;
    	this.right = right;
    } else {
    	return new Quotient(left, right);
    }
}
Quotient.prototype = new Expression();
Quotient.prototype.constructor = Quotient;
Quotient.prototype.toString = function() {
    return "Quotient(" + this.left + ", " + this.right + ")";
}
Quotient.unapply = function(x) {
    if (x instanceof Quotient) {
        return std.Some([x.left, x.right]);
    } else {
        return std.None;
    }
}

Function.prototype.orelse = function(f) {
    var that = this;
    return function(x) {
        var r = that.apply(null, [x]);
        if (r.isEmpty()) {
            return f(x);
        } else {
            return r;
        }
    }
}

Expression.eval = function(e) {
    return (function(e) {
        return Constant.unapply(e).map(function(v) {
        	return v;
        });
    }).orelse(function(e) {
        return Sum.unapply(e).map(function(a) {
        	return Expression.eval(a[0]) + Expression.eval(a[1]);
        });
    }).orelse(function(e) {
        return Difference.unapply(e).map(function(a) {
        	return Expression.eval(a[0]) - Expression.eval(a[1]);
        });
    }).orelse(function(e) {
        return Product.unapply(e).map(function(a) {
        	return Expression.eval(a[0]) * Expression.eval(a[1]);
        });
    }).orelse(function(e) {
        return Quotient.unapply(e).map(function(a) {
        	return Expression.eval(a[0]) / Expression.eval(a[1]);
        });
    })(e).get()
}

// call this as Parsers.apply(x) to mixin its methods to x
var Parsers = function() {
    var parsers = this;
    
    this.Result = function() {
    }
    
    this.Success = function(value, rem) {
        if (this instanceof Success) {
            this.value = value;
            this.rem = rem;
        } else {
            return new Success(value, rem);
        }
    }
    Success.prototype = new Result();
    Success.prototype.constructor = Success;
    Success.prototype.toString = function() {
        return "Success(" + this.value + ", " + this.rem + ")";
    }
    Success.unapply = function(x) {
        if (x instanceof Success) {
            return std.Some([x.value, x.rem]);
        } else {
            return std.None;
        }
    }
    
    this.Failure = function(msg) {
        if (this instanceof Failure) {
            this.msg = msg;
        } else {
            return new Failure(msg);
        }
    }
    Failure.prototype = new Result();
    Failure.prototype.constructor = Failure;
    Failure.prototype.toString = function() {
        return "Failure(" + this.msg + ")";
    }
    Failure.unapply = function(x) {
        if (x instanceof Failure) {
            return std.Some(msg);
        } else {
            return std.None;
        }
    }
    
    this["~"] = function(a, b) {
        if (this instanceof parsers["~"]) {
            this.a = a;
            this.b = b;
        } else {
            return new parsers["~"](a, b);
        }
    }
    this["~"].prototype.toString = function() {
        return this.a + "~" + this.b;
    }
    this["~"].unapply = function(x) {
        if (x instanceof parsers["~"]) {
            return std.Some([x.a, x.b]);
        } else {
            return std.None;
        }
    }
    
    this.Parser = function() {
        var outer = this;
        
        this["~"] = function(that) {
            return new SequenceParser(function() {return this}, that); // pass this as a thunk for cbn
        }
        
        this["|"] = function(that) {
            return new DisParser(function() {return this}, that);
        }
        
        this["~>"] = function(that) {
            return this["~"](that)["^^"](function(x) {
                return (function(x) {
                    return parsers["~"].unapply(x).map(function(a) {
                        return a[1];
                    });
                })(x).get();
            });
        }

        this["<~"] = function(that) {
            return this["~"](that)["^^"](function(x) {
                return (function(x) {
                    return parsers["~"].unapply(x).map(function(a) {
                        return a[0];
                    });
                })(x).get();
            });
        }
        
        this["*"] = function() {
            return this["~"](this["*"])["^^"](function(x) {
                return (function(x) {
                    return parsers["~"].unapply(x).map(function(a) {
                        return std["::"](a[0], a[1]);
                    });
                })(x).get();
            })["|"](function() {success(std.Nil)});
        }
        
        this["^^"] = function(f) {
        	var result = new Parser();
        	result.apply = function(s) {
        		return (function(x) {
        			return Success.unapply(x).map(function(a) {
        				return Success(f(a[0]), a[1]); // TODO will it need to be f.apply(a[0])? Do we need a separate apply for Scala functions?
        			});
        		}).orelse(function(x) {
        			return Failure.unapply(x).map(function(a) {
        				return x;
        			});
        		})(outer.apply(s)).get();
        	}
        }
    }
    
    this.KeywordParser = function(str) {
    	this.apply = function(s) {
    		if (s.substring(0, str.length) === str) {
    			return Success(str, s.substring(str.length));
    		} else {
    			return Failure("Expected '" + str +
    				"' got '" + s.substring(str.length) + "'");
    		}
    	}
    }
    KeywordParser.prototype = new Parser();
    KeywordParser.prototype.constructor = KeywordParser;
    
    this.SequenceParser = function(l, r) {
        var left = l();
        var right = r();
        
        this.apply = function(s) {
            return (function(x) {
            	return Success.unapply(x).map(function(a) {
            		return (function(x) {
            			return Success.unapply(x).map(function(b) {
            				return Success(new parsers["~"](a[0], b[0]), b[1]);
            			});
            		}).orelse(function(x) {
            			return Failure.unapply(x).map(function(b) {
            				return x;
            			});
            		})(right.apply(a[1])).get();
            	});
            }).orelse(function(x) {
            	return Failure.unapply(x).map(function(a) {
            		return x;
            	});
            })(left.apply(s)).get();
        }
    }
    SequenceParser.prototype = new Parser();
    SequenceParser.prototype.constructor = SequenceParser;
    
    this.DisParser = function(l, r) {
        var left = l();
        var right = r();
        
        this.apply = function(s) {
            return (function(x) {
            	return Success.unapply(x).map(function(a) {
            		return x;
            	});
            }).orelse(function(x) {
            	return Failure.unapply(x).map(function(a) {
            		return right.apply(s);
            	});
            })(left.apply(s)).get();
        }
    }
    DisParser.prototype = new Parser();
    DisParser.prototype.constructor = DisParser;
    
    this.success = function(v) {
    	var result = new Parser();
        result.apply = function(s) {
        	return Success(v, s);
        }
    }
}

var RegexParsers = function() {
	this.keyword = function(str) {
		return new KeywordParser(str); // will it find this through the prototype?
	}
	
	this.regex = function(r) {
		var result = new Parser();
		result.apply = function(s) {
			// TODO
		}
	}
}
RegexParsers.prototype = new Parsers();
RegexParsers.prototype.constructor = RegexParsers;