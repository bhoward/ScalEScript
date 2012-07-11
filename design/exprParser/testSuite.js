var ju = jsUnity.assertions;

var checkParse = function(parser, src, val, rem) {
    var result = parser.app(src);
    ju.assertTrue(result.successful());
    ju.assertTrue(std.equals(val, result.value));
    ju.assertIdentical(rem, result.rem);
};

var testSuite = {
    suiteName: "Eval and Parser tests",
    // setUp: function() {},
    // tearDown: function() {},
    
    testEval1: function() {
        var e = Sum(Product(Constant(4), Constant(8)), Difference(Constant(12), Constant(2)));
        ju.assertIdentical(42, Expression.eval(e));
    },
    
    testKeywordParser: function() {
        var r = new RegexParsers();
        checkParse(r.keyword("test"), "testing", "test", "ing");
    },
    
    testRegexParser: function() {
        var r = new RegexParsers();
        checkParse(r.regex(/[1-9]\d*|0/), "23skidoo", "23", "skidoo");
    },
    
    testRegexParser2: function() {
        var r = new RegexParsers();
        checkParse(r.regex(/[1-9]\d*|0/), "0123skidoo", "0", "123skidoo");
    },
    
    testRegexParser3: function() {
        var r = new RegexParsers();
        var result = r.regex(/[1-9]\d*|0/).app("testing");
        ju.assertFalse(result.successful());
        ju.assertIdentical("Expected '/[1-9]\\d*|0/' got 'testing'", result.msg);
    },
    
    testSeqParser: function() {
        var r = new RegexParsers();
        checkParse(r.keyword("test")["~"](function() {return r.regex(/[1-9]\d*|0/);}), "test123", r["~"]("test", "123"), "");
    },
    
    testExprParser: function() {
        checkParse(ExprParser.expr(), "2 + 2", Sum(Constant(2), Constant(2)), "");
    },
    
    testExprParser2: function() {
        //checkParse(ExprParser.expr(), "4*8 + (12 - 2)", Sum(Product(Constant(4), Constant(8)), Difference(Constant(12), Constant(2))), "");
        var result = ExprParser.expr().app("4*8+ ( 12-2)");
        ju.assertTrue(result.successful());
        ju.assertTrue(std.equals(Sum(Product(Constant(4), Constant(8)), Difference(Constant(12), Constant(2))), result.value));
        ju.assertIdentical("", result.rem);
    },
    
    testExprParser3: function() {
        checkParse(ExprParser.expr(), "12 - 2 + 4*8", Sum(Difference(Constant(12), Constant(2)), Product(Constant(4), Constant(8))), "");
    },
    
    testExprParser4: function() {
        var p = ExprParser.expr();
        ju.assertIdentical(42, Expression.eval(p.app("12 - 2 + 4*8").value));
    },
    
    testLazy: function() {
        var x = 0;
        var z = Lazy(function() {
            x = x + 1;
            return x;
        });
        ju.assertIdentical(0, x);
        ju.assertIdentical(2, z() + z());
        ju.assertIdentical(1, x);
    }
};
