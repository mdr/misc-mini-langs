package com.github.mdr.lambdacalculus

import PartialFunction._

import Term._

sealed abstract trait Term {

  def substitute(variable: Variable, replacement: Term): Term

  def freeVariables: Set[Variable]

  def boundVariables: Set[Variable] = getBoundVariables(Set())

  private[lambdacalculus] def getBoundVariables(bindingVariables: Set[Variable]): Set[Variable]

  def betaRedexes: List[Redex]

  def isInBetaNormalForm = betaRedexes.isEmpty

  def contract(redex: Redex): Term = contract(redex.position)

  def contract(position: Position): Term

  def α_==(other: Term) = alphaEquivalent(other)

  def alphaEquivalent(other: Term): Boolean

  def contains(other: Term): Boolean

  def b: Term = betaReduction getOrElse this

  def b(reductions: Int): Term = if (reductions == 0) this else b.b(reductions - 1)

  def betaReduction: Option[Term] = this match {
    case BetaRedex(parameter, body, argument) ⇒ Some(body.substitute(parameter, argument))
    case left * right ⇒ (left.betaReduction map { _(right) }) orElse (right.betaReduction map { left(_) })
    case λ(parameter, body) ⇒ body.betaReduction map { λ(parameter, _) }
    case _ ⇒ None
  }

  def etaReduction: Option[Term] = this match {
    case EtaRedex(f, _) ⇒ Some(f)
    case left * right ⇒ (left.etaReduction map { _(right) }) orElse (right.etaReduction map { left(_) })
    case λ(parameter, body) ⇒ body.etaReduction map { λ(parameter, _) }
    case _ ⇒ None
  }

  def evaluateWithTrace(maxReductions: Int = 1000): List[Term] =
    if (maxReductions == 0)
      throw new RuntimeException("Too many reductions without reaching normal form: " + this)
    else {
      val rest =
        { betaReduction map (_.evaluateWithTrace(maxReductions - 1)) } orElse
          { etaReduction map (_.evaluateWithTrace(maxReductions - 1)) } getOrElse
          Nil
      this :: rest
    }

  def evaluate(maxReductions: Int = 1000): Term = evaluateWithTrace(maxReductions).last

  def apply(other: Term) = Application(this, other)

  import PrettyPrintingConstants._
  override def toString = PrettyPrinter(abbreviateChurchNumerals, abbreviateConstants, omitParentheses).print(this)

}

case class Variable(name: String) extends Term {

  def substitute(variable: Variable, replacement: Term): Term =
    if (variable == this) replacement else this

  def freeVariables: Set[Variable] = Set(this)

  private[lambdacalculus] def getBoundVariables(bindingVariables: Set[Variable]): Set[Variable] =
    if (bindingVariables contains this) Set(this) else Set()

  def alphaEquivalent(other: Term): Boolean = cond(other) { case Variable(name2) ⇒ name == name2 }

  def contains(other: Term) = this == other

  def betaRedexes = List()

  def contract(position: Position): Term =
    throw new IllegalArgumentException("Invalid contraction of redex: " + this)

}

case class Abstraction(parameter: Variable, body: Term) extends Term {

  def substitute(variable: Variable, replacement: Term): Term =
    if (variable == parameter)
      this
    else if (!body.freeVariables.contains(variable))
      this
    else if (replacement.freeVariables contains parameter) {
      val freeVariables = body.freeVariables ++ replacement.freeVariables
      val freshVar = Variable(VariableNames.getFirstNameNotIn(freeVariables map { _.name }))
      λ(freshVar, body.substitute(parameter, freshVar).substitute(variable, replacement))
    } else
      copy(body = body.substitute(variable, replacement))

  def freeVariables: Set[Variable] = body.freeVariables - parameter

  private[lambdacalculus] def getBoundVariables(bindingVariables: Set[Variable]) =
    body.getBoundVariables(bindingVariables + parameter) + parameter

  def alphaEquivalent(other: Term): Boolean = cond(other) {
    case λ(otherArgument, otherBody) ⇒ body α_== otherBody.substitute(otherArgument, parameter)
  }

  def contains(other: Term) = parameter == other || (body contains other)

  def betaRedexes = body.betaRedexes map { _.prependChoice(true) }

  def contract(position: Position): Term =
    if (position.choices.isEmpty)
      throw new IllegalArgumentException("Invalid contraction of redex: " + this)
    else if (position.choices.head == false)
      throw new IllegalArgumentException("Invalid contraction of redex: " + this)
    else
      copy(body = body contract position.tail)

}

case class Application(function: Term, argument: Term) extends Term {

  def substitute(variable: Variable, replacement: Term): Term =
    function.substitute(variable, replacement)(argument.substitute(variable, replacement))

  def freeVariables: Set[Variable] = function.freeVariables ++ argument.freeVariables

  private[lambdacalculus] def getBoundVariables(bindingVariables: Set[Variable]): Set[Variable] =
    function.getBoundVariables(bindingVariables) ++ argument.getBoundVariables(bindingVariables)

  def alphaEquivalent(other: Term): Boolean = cond(other) {
    case otherFunction * otherArgument ⇒ (function α_== otherFunction) && (argument α_== otherArgument)
  }

  def contains(other: Term) = (function contains other) || (argument contains other)

  def betaRedexes = (function.betaRedexes map { _.prependChoice(false) }) ++
    (argument.betaRedexes map { _.prependChoice(true) }) ++
    condOpt(function) { case λ(_, _) ⇒ Redex(this, Position(Vector())) }

  def contract(position: Position): Term =
    if (position.choices.isEmpty)
      function match {
        case λ(variable, body) ⇒ body.substitute(variable, argument)
        case _ ⇒ throw new IllegalArgumentException("Invalid contraction of redex: " + this)
      }
    else if (position.choices.head == false)
      copy(function = function contract position.tail)
    else
      copy(argument = argument contract position.tail)

}

object Term extends Parser {

  val λ = Abstraction
  val * = Application
  val ** = Application

  object BetaRedex {
    def unapply(t: Term) = condOpt(t) { case λ(parameter, body) * argument ⇒ (parameter, body, argument) }
  }

  object EtaRedex {
    def unapply(t: Term) = condOpt(t) { case λ(x, f ** y) if x == y && !(f.freeVariables contains x) ⇒ (f, x) }
  }

  def apply(n: Int): Term = ChurchNumeral(n)

  def apply(input: String): Term = parseAll(term, input).get

  implicit def string2Term(s: String): Term = Term(s)
  implicit def int2Term(n: Int): Term = Term(n)

  implicit def string2Termable(s: String): Termable = new Termable(s)
  class Termable(s: String) {
    def t = Term(s)
  }

}
