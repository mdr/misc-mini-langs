package com.github.mdr.combinatorylogic

case class PrettyPrinter(
  abbreviateChurchNumerals: Boolean = true,
  abbreviateConstants: Boolean = true,
  omitParens: Boolean = true) {

  def print(term: Term): String =
    if (omitParens) printWithoutParens(term)
    else printWithParens(term)

  object Constant {
    def unapply(term: Term): Option[String] =
      (for ((s, `term`) ← Constants.constants) yield s).headOption
  }

  private def printWithParens(term: Term): String = term match {
    case Constant(s) if abbreviateConstants ⇒ s
    case ChurchNumeral(n) if abbreviateChurchNumerals ⇒ n.toString
    case Application(left, right) ⇒ "(" + printWithParens(left) + " " + printWithParens(right) + ")"
    case Variable(name) ⇒ name
    case S ⇒ "S"
    case K ⇒ "K"
    case I ⇒ "I"
  }

  private def printWithoutParens(term: Term): String = term match {
    case Constant(s) if abbreviateConstants ⇒ s
    case ChurchNumeral(n) if abbreviateChurchNumerals ⇒ n.toString
    case Application(left, right) ⇒
      val leftStr = left match {
        case Constant(s) if abbreviateConstants ⇒ s
        case ChurchNumeral(n) if abbreviateChurchNumerals ⇒ n.toString
        case _ ⇒ printWithoutParens(left)
      }
      val rightStr = right match {
        case Constant(s) if abbreviateConstants ⇒ s
        case ChurchNumeral(n) if abbreviateChurchNumerals ⇒ n.toString
        case Application(_, _) ⇒ "(" + printWithoutParens(right) + ")"
        case _ ⇒ printWithoutParens(right)
      }
      leftStr + " " + rightStr
    case Variable(name) ⇒ name
    case S ⇒ "S"
    case K ⇒ "K"
    case I ⇒ "I"
  }

}

object PrettyPrintingConstants {

  var abbreviateChurchNumerals = true
  var abbreviateConstants = true
  var omitParentheses = true

}
