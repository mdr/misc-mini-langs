package com.github.mdr.lambdacalculus

import scala.util.parsing.combinator._
import PartialFunction._

case class Redex(application: Application, position: Position) {

  def prependChoice(choice: Boolean) = copy(position = position.prependChoice(choice))

}

case class Position(choices: Vector[Boolean]) {

  def prependChoice(choice: Boolean) = Position(choice +: choices)

  def tail = Position(choices.tail)

}

object VariableNames {

  private val letters = "abcdefghijklmnopqrstuvwyz".toList map { _.toString }

  def getFirstNameNotIn(names: Set[String]): String =
    (letters filterNot names headOption) getOrElse { throw new UnsupportedOperationException("TODO: Add primes") }

}

trait ReductionStrategy {

  def findRedex(expression: Expression): Option[Redex]

}


