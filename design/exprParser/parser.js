var Option = function Option(x) {
	if (this instanceof Option) {
		// constructor call with new
		// do nothing
	} else {
		// factory call without new
		if (x === null) { // and/or undefined?
			return None;
		} else {
			return Some(x);
		}
	}
}

var Some = function Some(value) {
    if (this instanceof Some) {
	    // Option.apply(this, []); -- call the super class ctor (doesn't apply for a trait?)
        this.value = value;
    } else {
        return new Some(value);
    }
}
Some.prototype = new Option();
Some.prototype.constructor = Some;
// Can't put methods in prototype if they depend on constructor args
// (Alternately, save ctor args in hidden instance field for later use)
Some.prototype.toString = function() {
    return "Some(" + this.value + ")";
}
Some.prototype.isEmpty = function() {
    return false;
}
Some.prototype.get = function() {
    return this.value;
}
Some.prototype.map = function(f) {
    return Some(f(this.value));
}
Some.unapply = function(x) {
	return x; // special case: it already is Some(value) or None...
}

var None = new Option();
None.toString = function() {
    return "None";
}
None.isEmpty = function() {
    return true;
}
None.get = function() {
    return undefined;
}
None.map = function(f) {
    return None;
}
None.unapply = function(x) {
	return x instanceof None;
}

var Expression = function() {
}

var Constant = function Constant(value) {
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
        return Some(x.value);
    } else {
        return None;
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
        return Some([x.left, x.right]);
    } else {
        return None;
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
        return Some([x.left, x.right]);
    } else {
        return None;
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
        return Some([x.left, x.right]);
    } else {
        return None;
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
        return Some([x.left, x.right]);
    } else {
        return None;
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

var eval = function eval(e) {
    return (function(e) {
        return Constant.unapply(e).map(function(v) {
        	return v;
        });
    }).orelse(function(e) {
        return Sum.unapply(e).map(function(a) {
        	return eval(a[0]) + eval(a[1]);
        });
    }).orelse(function(e) {
        return Difference.unapply(e).map(function(a) {
        	return eval(a[0]) - eval(a[1]);
        });
    }).orelse(function(e) {
        return Product.unapply(e).map(function(a) {
        	return eval(a[0]) * eval(a[1]);
        });
    }).orelse(function(e) {
        return Quotient.unapply(e).map(function(a) {
        	return eval(a[0]) / eval(a[1]);
        });
    })(e).get()
}
