package com.github.mdr.combinatorylogic

import com.github.mdr.combinatorylogic
import com.github.mdr.lambdacalculus
import PartialFunction._

import Term._

sealed trait Term {

  def r = reduce
  def reduce: Term = this match {
    case IRedex(x) ⇒ x
    case KRedex(x, y) ⇒ x
    case SRedex(x, y, z) ⇒ x(z)(y(z))
    case left * right ⇒
      left.r match {
        case `left` ⇒ left(right.r)
        case other ⇒ other(right)
      }
    case _ ⇒ this
  }

  def apply(other: Term) = Application(this, other)

  import PrettyPrintingConstants._
  override def toString = PrettyPrinter(abbreviateChurchNumerals, abbreviateConstants, omitParentheses).print(this)

  def variables: Set[Variable] = Set()

  def abstraction(variable: String): Term = abstraction(Variable(variable))

  def abstraction(variable: Variable): Term = this match {
    case _ if !(variables contains variable) ⇒ K(this)
    case `variable` ⇒ I
    case left * `variable` if !(left.variables contains variable) ⇒ left
    case left * right ⇒ S(left abstraction variable)(right abstraction variable)
  }

  def redexes: List[Redex] = Nil

  def isInWeakNormalForm = redexes.isEmpty

  def contract(redex: Redex): Term = contract(redex.position)

  def contract(position: Position): Term =
    throw new IllegalArgumentException("Invalid contraction of redex: " + this)

}

case class Variable(name: String) extends Term {

  override def variables: Set[Variable] = Set(this)

}

case class Application(left: Term, right: Term) extends Term {

  override def variables: Set[Variable] = left.variables ++ right.variables

  override def redexes =
    (left.redexes map { _.prependChoice(false) }) ++ (right.redexes map { _.prependChoice(true) }) ++
      (condOpt(this) {
        case IRedex(_) | KRedex(_, _) | SRedex(_, _, _) ⇒ Redex(this, Position(Vector()))
      })

  override def contract(position: Position): Term =
    if (position.choices.isEmpty)
      this match {
        case IRedex(x) ⇒ x
        case KRedex(x, y) ⇒ x
        case SRedex(x, y, z) ⇒ x(z)(y(z))
        case _ ⇒ throw new IllegalArgumentException("Invalid contraction of redex: " + this)
      }
    else if (position.choices.head == false)
      copy(left = left contract position.tail)
    else
      copy(right = right contract position.tail)

}

case object S extends Term
case object K extends Term
case object I extends Term

object Term extends Parser {

  val * = Application
  val ** = Application

  object SRedex {
    def unapply(t: Term) = condOpt(t) { case S * x * y * z ⇒ (x, y, z) }
  }

  object KRedex {
    def unapply(t: Term) = condOpt(t) { case K * x * y ⇒ (x, y) }
  }

  object IRedex {
    def unapply(t: Term) = condOpt(t) { case I * x ⇒ x }
  }

  def apply(input: String): Term = parseAll(term, input).get

  implicit def string2Term(s: String): Term = Term(s)

  implicit def string2Termable(s: String): Termable = new Termable(s)
  class Termable(s: String) {
    def t = Term(s)
  }

  def apply(lambdaTerm: lambdacalculus.Term): Term = lambdaTerm match {
    case lambdacalculus.Variable(name) ⇒ Variable(name)
    case lambdacalculus.Application(left, right) ⇒ Term(left)(Term(right))
    case lambdacalculus.Abstraction(parameter, body) ⇒ Term(body) abstraction Variable(parameter.name)
  }

}
