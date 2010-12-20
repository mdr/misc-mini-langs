package com.github.mdr.combinatorylogic

import com.github.mdr.combinatorylogic
import com.github.mdr.lambdacalculus

sealed trait Expression {

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

  def variables: Set[Variable] = Set()

}

case class Variable(name: String) extends Expression {

  override def variables: Set[Variable] = Set(this)

}

case class Application(left: Expression, right: Expression) extends Expression {

  override def variables: Set[Variable] = left.variables ++ right.variables

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

  def apply(lambdaTerm: lambdacalculus.Expression): Expression = lambdaTerm match {
    case lambdacalculus.Variable(name) => Variable(name)
    case lambdacalculus.Application(left, right) => Application(Expression(left), Expression(right))
    case lambdacalculus.Abstraction(parameter, body) => abstraction(Variable(parameter.name), Expression(body))
  }

  private def abstraction(variable: Variable, expression: Expression): Expression = expression match {
    case _ if !expression.variables.contains(variable) => K(expression)
    case `variable` => I
    case Application(left, `variable`) if !left.variables.contains(variable) => left
    case Application(left, right) => S(abstraction(variable, left))(abstraction(variable, right))
  }

}
