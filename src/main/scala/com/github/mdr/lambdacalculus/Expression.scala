package com.github.mdr.lambdacalculus

import PartialFunction._

import Expression._

sealed abstract trait Expression {

  def substitute(variable: Variable, replacement: Expression): Expression

  def freeVariables: Set[Variable]

  def boundVariables: Set[Variable] = getBoundVariables(Set())

  private[lambdacalculus] def getBoundVariables(bindingVariables: Set[Variable]): Set[Variable]

  /** Find beta redexes */
  def redexes: List[Redex]

  def isInBetaNormalForm = redexes.isEmpty

  def contract(redex: Redex): Expression = contract(redex.position)

  def contract(position: Position): Expression

  def α_==(other: Expression) = alphaEquivalent(other)

  def alphaEquivalent(other: Expression): Boolean

  def contains(other: Expression): Boolean

  def b = betaReduction

  def betaReduction: Expression = this match {
    case BetaRedex(parameter, body, argument) => body.substitute(parameter, argument)
    case a * b =>
      val left = a.betaReduction(b)
      if (left != this)
        left
      else
        a(b.betaReduction)
    case λ(parameter, body) => λ(parameter, body.betaReduction)
    case _ => this
  }

  def etaConversion: Expression = condOpt(this) { case EtaRedex(f, x) => f } getOrElse this

  def evaluate(callback: Expression => Unit): Expression = {
    callback(this)
    val beta = this.betaReduction
    if (beta != this)
      beta.evaluate(callback)
    else {
      val eta = this.etaConversion
      if (eta != this) callback(eta)
      eta
    }
  }

  def evaluate: Expression = evaluate { e => () }

  def apply(other: Expression) = Application(this, other)

  import Expression._
  override def toString = PrettyPrinter(abbreviateChurchNumerals, abbreviateConstants, omitParentheses).print(this)

}

case class Variable(name: String) extends Expression {

  def substitute(variable: Variable, replacement: Expression): Expression =
    if (variable == this) replacement else this

  def freeVariables: Set[Variable] = Set(this)

  private[lambdacalculus] def getBoundVariables(bindingVariables: Set[Variable]): Set[Variable] =
    if (bindingVariables contains this) Set(this) else Set()

  def alphaEquivalent(other: Expression): Boolean = cond(other) { case Variable(name2) => name == name2 }

  def contains(other: Expression) = this == other

  def redexes = List()

  def contract(position: Position): Expression =
    throw new IllegalArgumentException("Invalid contraction of redex: " + this)

}

case class Abstraction(parameter: Variable, body: Expression) extends Expression {

  def substitute(variable: Variable, replacement: Expression): Expression =
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

  def alphaEquivalent(other: Expression): Boolean = cond(other) {
    case λ(otherArgument, otherBody) => body α_== otherBody.substitute(otherArgument, parameter)
  }

  def contains(other: Expression) = parameter == other || (body contains other)

  def redexes = body.redexes map { _.prependChoice(true) }

  def contract(position: Position): Expression =
    if (position.choices.isEmpty)
      throw new IllegalArgumentException("Invalid contraction of redex: " + this)
    else if (position.choices.head == false)
      throw new IllegalArgumentException("Invalid contraction of redex: " + this)
    else
      copy(body = body contract position.tail)

}

case class Application(function: Expression, argument: Expression) extends Expression {

  def substitute(variable: Variable, replacement: Expression): Expression =
    function.substitute(variable, replacement)(argument.substitute(variable, replacement))

  def freeVariables: Set[Variable] = function.freeVariables ++ argument.freeVariables

  private[lambdacalculus] def getBoundVariables(bindingVariables: Set[Variable]): Set[Variable] =
    function.getBoundVariables(bindingVariables) ++ argument.getBoundVariables(bindingVariables)

  def alphaEquivalent(other: Expression): Boolean = cond(other) {
    case otherFunction * otherArgument => (function α_== otherFunction) && (argument α_== otherArgument)
  }

  def contains(other: Expression) = (function contains other) || (argument contains other)

  def redexes = (function.redexes map { _.prependChoice(false) }) ++
    (argument.redexes map { _.prependChoice(true) }) ++
    condOpt(function) { case λ(_, _) => Redex(this, Position(Vector())) }

  def contract(position: Position): Expression =
    if (position.choices.isEmpty)
      function match {
        case λ(variable, body) => body.substitute(variable, argument)
        case _ => throw new IllegalArgumentException("Invalid contraction of redex: " + this)
      }
    else if (position.choices.head == false)
      copy(function = function contract position.tail)
    else
      copy(argument = argument contract position.tail)

}

object Expression extends Parser {

  var abbreviateChurchNumerals = true
  var abbreviateConstants = true
  var omitParentheses = true

  val λ = Abstraction
  val * = Application
  val ** = Application

  object BetaRedex {
    def unapply(e: Expression) = condOpt(e) { case λ(parameter, body) * argument => (parameter, body, argument) }
  }

  object EtaRedex {
    def unapply(e: Expression) = condOpt(e) { case λ(x, f ** y) if x == y && !(f.freeVariables contains x) => (f, x) }
  }

  def apply(n: Int): Expression = ChurchNumeral(n)

  def apply(input: String): Expression = parseAll(expression, input).get

  implicit def string2Expression(s: String): Expression = Expression(s)
  implicit def int2Expression(n: Int): Expression = Expression(n)

  implicit def string2Expressionable(s: String): Expressionable = new Expressionable(s)
  class Expressionable(s: String) {
    def e = Expression(s)
  }

}
