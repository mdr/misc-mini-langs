package com.github.mdr.lambdacalculus

import Expression._

object ChurchNumeral {

  def apply(s: String): Expression = this(Integer.parseInt(s))

  def apply(n: Int): Expression = {
    require(n >= 0)
    val x = Variable("x")
    val f = Variable("f")
    def iterate(n: Int): Expression = if (n == 0) x else f(iterate(n - 1))
    λ(f, λ(x, iterate(n)))
  }

  def unapply(expression: Expression): Option[Int] = expression match {
    case λ(var1, λ(var2, subExpression)) => iteratedApplication(var1, var2, subExpression)
    case _ => None
  }

  def iteratedApplication(functionVar: Variable, zeroVar: Variable, expression: Expression): Option[Int] =
    expression match {
      case `zeroVar` => Some(0)
      case `functionVar` * subExpression => iteratedApplication(functionVar, zeroVar, subExpression) map { _ + 1 }
      case _ => None
    }

}
