package com.github.mdr.lambdacalculus

object ChurchNumeral {

  def apply(s: String): Expression = this(Integer.parseInt(s))

  def apply(n: Int): Expression = {
    require(n >= 0)
    val x = Variable("x")
    val f = Variable("f")
    var body: Expression = x
    for (i <- 1 to n) { body = Application(f, body) }
    Abstraction(f, Abstraction(x, body))
  }

  def unapply(expression: Expression): Option[Int] = expression match {
    case Abstraction(var1, Abstraction(var2, subExpression)) => iteratedApplication(var1, var2, subExpression)
    case _ => None
  }

  def iteratedApplication(functionVar: Variable, zeroVar: Variable, expression: Expression): Option[Int] = expression match {
    case `zeroVar` => Some(0)
    case Application(`functionVar`, subExpression) => iteratedApplication(functionVar, zeroVar, subExpression) map { _ + 1 }
    case _ => None
  }

}

