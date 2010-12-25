package com.github.mdr.combinatorylogic

import com.github.mdr.combinatorylogic
import com.github.mdr.lambdacalculus
import PartialFunction._

import Expression._

sealed trait Expression {

  def r = reduce
  def reduce: Expression = this match {
    case IRegex(x) => x
    case KRegex(x, y) => x
    case SRegex(x, y, z) => x(z)(y(z))
    case left * right =>
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
    case left * `variable` if !(left.variables contains variable) => left
    case left * right => S(left abstraction variable)(right abstraction variable)
  }

  def redexes: List[Redex] = Nil

  def isInWeakNormalForm = redexes.isEmpty

  def contract(redex: Redex): Expression = contract(redex.position)

  def contract(position: Position): Expression =
    throw new IllegalArgumentException("Invalid contraction of redex: " + this)

}

case class Variable(name: String) extends Expression {

  override def variables: Set[Variable] = Set(this)

}

case class Application(left: Expression, right: Expression) extends Expression {

  override def variables: Set[Variable] = left.variables ++ right.variables

  override def redexes =
    (left.redexes map { _.prependChoice(false) }) ++ (right.redexes map { _.prependChoice(true) }) ++
      (condOpt(this) {
        case IRegex(_) | KRegex(_, _) | SRegex(_, _, _) => Redex(this, Position(Vector()))
      })

  override def contract(position: Position): Expression =
    if (position.choices.isEmpty)
      this match {
        case IRegex(x) => x
        case KRegex(x, y) => x
        case SRegex(x, y, z) => x(z)(y(z))
        case _ => throw new IllegalArgumentException("Invalid contraction of redex: " + this)
      }
    else if (position.choices.head == false)
      copy(left = left contract position.tail)
    else
      copy(right = right contract position.tail)

}

case object S extends Expression
case object K extends Expression
case object I extends Expression

object Expression extends Parser {

  var abbreviateConstants = true
  var omitParentheses = true

  object * {
    def unapply(e: Expression) = condOpt(e) { case Application(left, right) => (left, right) }
  }

  object SRegex {
    def unapply(e: Expression) = condOpt(e) { case S * x * y * z => (x, y, z) }
  }

  object KRegex {
    def unapply(e: Expression) = condOpt(e) { case K * x * y => (x, y) }
  }

  object IRegex {
    def unapply(e: Expression) = condOpt(e) { case I * x => x }
  }

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
