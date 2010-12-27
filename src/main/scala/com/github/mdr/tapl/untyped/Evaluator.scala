package com.github.mdr.tapl.untyped

import PartialFunction._
import scala.annotation.tailrec

object Evaluator {

  private def isVal(t: Term) = cond(t) { case Abstraction(_, _) => true }

  private def eval1(t: Term): Option[Term] = {
    implicit def autoPromoteToOption(t: Term): Option[Term] = Some(t)
    t match {
      case Application(Abstraction(parameter, body), right) if isVal(right) =>
        substitute(body, parameter, right)
      case Application(left, right) if isVal(left) =>
        eval1(right) map { Application(left, _) }
      case Application(left, right) =>
        eval1(left) map { Application(_, right) }
      case _ ⇒ None
    }
  }

  def substitute(term: Term, variableToReplace: String, replacement: Term): Term = term match {
    case Variable(name) => if (name == variableToReplace) replacement else term
    case Application(left, right) =>
      Application(substitute(left, variableToReplace, replacement), substitute(right, variableToReplace, replacement))
    case Abstraction(parameter, body) =>
      if (variableToReplace == parameter)
        term
      else if (!freeVariables(body).contains(variableToReplace))
        term
      else if (freeVariables(replacement) contains parameter) {
        val freeVars = freeVariables(body) ++ freeVariables(replacement)
        val freshVar = getFirstNameNotIn(freeVars)
        Abstraction(freshVar, substitute(substitute(body, parameter, Variable(freshVar)), variableToReplace, replacement))
      } else
        Abstraction(parameter, substitute(body, variableToReplace, replacement))
  }

  private val letters = "abcdefghijklmnopqrstuvwyz".toList map { _.toString }

  private def getFirstNameNotIn(variables: Set[String]): String =
    (letters filterNot variables headOption) getOrElse { throw new UnsupportedOperationException("TODO: Add primes") }

  private def freeVariables(term: Term): Set[String] = term match {
    case Variable(name) => Set(name)
    case Abstraction(parameter, body) => freeVariables(body) - parameter
    case Application(left, right) => freeVariables(left) ++ freeVariables(right)
  }

  @tailrec
  def eval(t: Term): Term = eval1(t) match {
    case None ⇒ t
    case Some(nt) ⇒ eval(nt)
  }

}
