package com.github.mdr.lambdacalculus

import Term._

object ChurchNumeral {

  def apply(s: String): Term = this(Integer.parseInt(s))

  def apply(n: Int): Term = {
    require(n >= 0)
    val x = Variable("x")
    val f = Variable("f")
    def iterate(n: Int): Term = if (n == 0) x else f(iterate(n - 1))
    λ(f, λ(x, iterate(n)))
  }

  def unapply(term: Term): Option[Int] = term match {
    case λ(var1, λ(var2, subTerm)) ⇒ iteratedApplication(var1, var2, subTerm)
    case _ ⇒ None
  }

  def iteratedApplication(functionVar: Variable, zeroVar: Variable, term: Term): Option[Int] =
    term match {
      case `zeroVar` ⇒ Some(0)
      case `functionVar` * subTerm ⇒ iteratedApplication(functionVar, zeroVar, subTerm) map { _ + 1 }
      case _ ⇒ None
    }

}
