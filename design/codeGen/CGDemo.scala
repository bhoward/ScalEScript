trait A {
    def x: Int;
    var y: Int = 0;
    def f(x: Int): Int = g(g(x));
    def g(x: Int): Int;
};

trait B extends A {
    override def g(x: Int): Int = x + y;
};

class C(a: Int) extends A {
    y = a;
    val x: Int = f(2);
    def g(x: Int): Int = a;
};

object D extends C(20) with B;

// D.x is 42
