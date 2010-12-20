package com.github.mdr.combinatorylogic

sealed trait Expression {

  import com.github.mdr.combinatorylogic

  def r = reduce
  def reduce: Expression = this match {
    case Application(I, x) => x
    case Application(Application(K, x), y) => x
    case Application(Application(Application(S, x), y), z) => x(z)(y(z))
    case Application(left, right) =>
      left.r match {
        case `left` => Application(left, right.r)
        case other => Application(other, right)
      }
    case _ => this
  }

  def S = this(combinatorylogic.S)
  def K = this(combinatorylogic.K)
  def I = this(combinatorylogic.I)

  def apply(other: Expression) = Application(this, other)

  import Expression._
  override def toString = PrettyPrinter(abbreviateConstants, omitParentheses).print(this)

}

case class Variable(name: String) extends Expression {

}

case class Application(left: Expression, right: Expression) extends Expression {

}

case object S extends Expression
case object K extends Expression
case object I extends Expression

object Expression extends Parser {

  var abbreviateConstants = true
  var omitParentheses = true

  def apply(input: String): Expression = parseAll(expression, input).get

  implicit def string2Expression(s: String): Expression = Expression(s)

  implicit def string2Expressionable(s: String): Expressionable = new Expressionable(s)
  class Expressionable(s: String) {
    def e = Expression(s)
  }

}
