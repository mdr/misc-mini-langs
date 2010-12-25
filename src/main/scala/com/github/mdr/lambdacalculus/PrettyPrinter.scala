package com.github.mdr.lambdacalculus

import Term._

case class PrettyPrinter(
  abbreviateChurchNumerals: Boolean = true,
  abbreviateConstants: Boolean = true,
  omitParens: Boolean = true) {

  def print(t: Term): String = if (omitParens) printWithoutParens(t) else printWithParens(t)

  object Constant {
    def unapply(term: Term): Option[String] =
      (for ((s, t) ← Constants.constants if t α_== term) yield s).headOption
  }

  private def printWithParens(term: Term): String = term match {
    case Constant(s) if abbreviateConstants ⇒ s
    case ChurchNumeral(n) if abbreviateChurchNumerals ⇒ n.toString
    case left * right ⇒ "(" + printWithParens(left) + " " + printWithParens(right) + ")"
    case λ(variable, body) ⇒ "(" + "λ" + variable + "·" + printWithParens(body) + ")"
    case Variable(name) ⇒ name
  }

  private def printWithoutParens(term: Term): String = term match {
    case Constant(s) if abbreviateConstants ⇒ s
    case ChurchNumeral(n) if abbreviateChurchNumerals ⇒ n.toString
    case left * right ⇒
      val leftStr = left match {
        case Constant(s) if abbreviateConstants ⇒ s
        case ChurchNumeral(n) if abbreviateChurchNumerals ⇒ n.toString
        case λ(_, _) ⇒ "(" + printWithoutParens(left) + ")"
        case _ ⇒ printWithoutParens(left)
      }
      val rightStr = right match {
        case Constant(s) if abbreviateConstants ⇒ s
        case ChurchNumeral(n) if abbreviateChurchNumerals ⇒ n.toString
        case λ(_, _) | _* _ ⇒ "(" + printWithoutParens(right) + ")"
        case _ ⇒ printWithoutParens(right)
      }
      leftStr + " " + rightStr
    case abstraction@λ(_, _) ⇒
      def getVarsAndBody(t: Term): (List[Variable], Term) = t match {
        case Constant(_) if abbreviateConstants ⇒ (Nil, t)
        case ChurchNumeral(_) if abbreviateChurchNumerals ⇒ (Nil, t)
        case λ(argument, body) ⇒
          val (subVars, subExpr) = getVarsAndBody(body)
          ((argument :: subVars), subExpr)
        case _ ⇒ (Nil, t)
      }
      val (vars, subExpr) = getVarsAndBody(abstraction)
      "λ" + (vars map { _.name } mkString) + "·" + printWithoutParens(subExpr)
    case Variable(name) ⇒ name
  }

}

object PrettyPrintingConstants {

  var abbreviateChurchNumerals = true
  var abbreviateConstants = true
  var omitParentheses = true

}
