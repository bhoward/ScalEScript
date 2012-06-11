var ju = jsUnity.assertions;

var testSuite = {
	suiteName: "Eval tests",
	// setUp: function() {},
	// tearDown: function() {},
	
	testEval1: function() {
		var e = Sum(Product(Constant(4), Constant(8)), Difference(Constant(12), Constant(2)));
		ju.assertIdentical(42, Expression.eval(e));
	},
	
	// The following are just to test the test runner...
	testEval2: function() {
	    ju.assertIdentical(2+2, 4);
	},
	
	testFail: function() {
		ju.fail();
	}
}

