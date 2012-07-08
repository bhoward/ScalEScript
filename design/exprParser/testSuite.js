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
	},
	
	testKeywordParser: function() {
	    var r = new RegexParsers();
	    var p = r.keyword("test");
	    ju.assertIdentical("Success(test, ing)", p.app("testing").toString());
	},
	
	testRegexParser: function() {
	    var r = new RegexParsers();
	    var p = r.regex(/[1-9]\d*|0/);
	    ju.assertIdentical("Success(23, skidoo)", p.app("23skidoo").toString());
	},
	
	testRegexParser2: function() {
	    var r = new RegexParsers();
	    var p = r.regex(/[1-9]\d*|0/);
	    ju.assertIdentical("Success(0, 123skidoo)", p.app("0123skidoo").toString());
	},
	
	testRegexParser3: function() {
	    var r = new RegexParsers();
	    var p = r.regex(/[1-9]\d*|0/);
	    ju.assertIdentical("Failure(Expected '/[1-9]\\d*|0/' got 'testing')", p.app("testing").toString());
	},
	
	testSeqParser: function() {
	    var r = new RegexParsers();
	    var p = r.keyword("test")["~"](function() {return r.regex(/[1-9]\d*|0/);});
	    ju.assertIdentical("Success(test~123, )", p.app("test123").toString());
	}
}

