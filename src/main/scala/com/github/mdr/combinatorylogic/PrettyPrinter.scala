package com.github.mdr.combinatorylogic

case class PrettyPrinter(abbreviateConstants: Boolean = true, omitParens: Boolean = true) {

  def print(expression: Expression): String =
    if (omitParens) printWithoutParens(expression)
    else printWithParens(expression)

  object Constant {
    def unapply(expression: Expression): Option[String] =
      (for ((s, `expression`) <- Constants.constants) yield s).headOption
  }

  private def printWithParens(expression: Expression): String = expression match {
    case Constant(s) if abbreviateConstants => s
    case Application(left, right) => "(" + printWithParens(left) + " " + printWithParens(right) + ")"
    case Variable(name) => name
    case S => "S"
    case K => "K"
    case I => "I"
  }

  private def printWithoutParens(expression: Expression): String = expression match {
    case Constant(s) if abbreviateConstants => s
    case Application(left, right) =>
      val leftStr = left match {
        case Constant(s) if abbreviateConstants => s
        case _ => printWithoutParens(left)
      }
      val rightStr = right match {
        case Constant(s) if abbreviateConstants => s
        case Application(_, _) => "(" + printWithoutParens(right) + ")"
        case _ => printWithoutParens(right)
      }
      leftStr + " " + rightStr
    case Variable(name) => name
    case S => "S"
    case K => "K"
    case I => "I"
  }

}

