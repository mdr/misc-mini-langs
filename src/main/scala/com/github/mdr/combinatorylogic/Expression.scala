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

  def apply(other: Expression) = Application(this, other)

  import Expression._
  override def toString = PrettyPrinter(abbreviateConstants, omitParentheses).print(this)

  def variables: Set[Variable] = Set()

  def abstraction(variable: String): Expression = abstraction(Variable(variable))
  
  def abstraction(variable: Variable): Expression = this match {
    case _ if !(variables contains variable) => K(this)
    case `variable` => I
    case Application(left, `variable`) if !(left.variables contains variable) => left
    case Application(left, right) => S(left abstraction variable)(right abstraction variable)
  }

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
    case lambdacalculus.Abstraction(parameter, body) => Expression(body) abstraction Variable(parameter.name)
  }

}
