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
