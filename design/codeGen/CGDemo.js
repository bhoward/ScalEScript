var top = {};

var Lazy = function(f) {
    var cached = undefined;
    return function() {
        if (cached === undefined) {
            cached = f();
        }
        return cached;
    };
};

top.AnyRef = function() {
    top.AnyRef._init(this);
};
top.AnyRef._initProto = function(p) {
    // define methods of AnyRef here
    p._supers = {};
    p._supers["AnyRef"] = true;
};
top.AnyRef._init = function(o) {
    // initialize AnyRef fields here, if any
};
top.AnyRef.prototype = {};
top.AnyRef.prototype.constructor = top.AnyRef;
top.AnyRef._initProto(top.AnyRef.prototype);

top.A = {}; // can't call new on a trait
top.A._initProto = function(p) {
    // add (non-abstract) methods contributed by A to prototype object p
    p.y = function() {
        var self = this; // could eliminate in simple cases like this
        return self._y;
    };
    p.y_ = function(y) { // called when updating the var y: "y = 42" => "y_(42)"
        var self = this;
        self._y = y;
    };
    p.f = function(x) {
        var self = this;
        return self.g(self.g(x));
    };
    p._supers["A"] = true;
};
top.A._init = function(o) {
    // initialize instance fields contributed by A
    o._y = 0;
};

top.B = {};
top.B._initProto = function(p) {
    p.g = function(x) {
        var self = this;
        return x + self.y();
    };
    p._supers["B"] = true;
};
top.B._init = function(o) {
    // nothing to do
};

top.C = function(a) {
    top.C._init(this, a);
};
top.C._initProto = function(p) {
    p.x = function() {
        var self = this;
        return self._x;
    };
    p.g = function() {
        var self = this;
        return self._a;
    };
    p._supers["C"] = true;
};
top.C._init = function(o, a) {
    o._a = a; // save the ctor param in case it is needed later
    top.AnyRef._init(o); // call the superclass ctor
    top.A._init(o); // call each trait _init in linearization between C and its immediate superclass
    o.y_(o._a);
    o._x = o.f(2);
};
top.C.prototype = {};
top.C.prototype.constructor = top.C;
// Call _initProto for each type in linearization down to C
top.AnyRef._initProto(top.C.prototype);
top.A._initProto(top.C.prototype);
top.C._initProto(top.C.prototype);

top._D = function() {
    top._D._init(this);
};
top._D._initProto = function(p) {
    p._supers["D"] = true;
};
top._D._init = function(o) {
    top.C._init(o, 20);
    top.B._init(o);
};
top._D.prototype = {};
top._D.prototype.constructor = top._D;
top.AnyRef._initProto(top._D.prototype);
top.A._initProto(top._D.prototype);
top.C._initProto(top._D.prototype);
top.B._initProto(top._D.prototype);
top._D._initProto(top._D.prototype);
top.D = Lazy(function() { return new top._D(); });

print(top.D().x()); // supposed to print 42