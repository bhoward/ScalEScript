var ju = jsUnity.assertions;

var testSuite = {
	suiteName: "Eval tests",
	// setUp: function() {},
	// tearDown: function() {},
	
	testEval1: function() {
		var e = Sum(Product(Constant(4), Constant(8)), Difference(Constant(12), Constant(2)));
		ju.assertIdentical(42, eval(e));
	}
}

