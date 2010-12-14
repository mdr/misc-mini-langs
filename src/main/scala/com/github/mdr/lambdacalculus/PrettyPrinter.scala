package com.github.mdr.lambdacalculus

case class PrettyPrinter(
  abbreviateChurchNumerals: Boolean = true,
  abbreviateConstants: Boolean = true,
  omitParens: Boolean = true) {

  def print(expression: Expression): String =
    if (omitParens) printWithoutParens(expression)
    else printWithParens(expression)

  object Constant {
    def unapply(expression: Expression): Option[String] =
      (for ((s, e) <- Constants.constants if e alphaEquivalent expression) yield s).headOption
  }

  private def printWithParens(expression: Expression): String = expression match {
    case Constant(s) if abbreviateConstants => s
    case ChurchNumeral(n) if abbreviateChurchNumerals => n.toString
    case Application(left, right) => "(" + printWithParens(left) + " " + printWithParens(right) + ")"
    case Abstraction(variable, body) => "(" + "λ" + variable + "·" + printWithParens(body) + ")"
    case Variable(name) => name
  }

  private def printWithoutParens(expression: Expression): String = expression match {
    case Constant(s) if abbreviateConstants => s
    case ChurchNumeral(n) if abbreviateChurchNumerals => n.toString
    case Application(left, right) =>
      val leftStr = left match {
        case Constant(s) if abbreviateConstants => s
        case ChurchNumeral(n) if abbreviateChurchNumerals => n.toString
        case Abstraction(_, _) => "(" + printWithoutParens(left) + ")"
        case _ => printWithoutParens(left)
      }
      val rightStr = right match {
        case Constant(s) if abbreviateConstants => s
        case ChurchNumeral(n) if abbreviateChurchNumerals => n.toString
        case Abstraction(_, _) | Application(_, _) => "(" + printWithoutParens(right) + ")"
        case _ => printWithoutParens(right)
      }
      leftStr + " " + rightStr
    case abstraction@Abstraction(_, _) =>
      def getVarsAndBody(e: Expression): (List[Variable], Expression) = e match {
        case Constant(_) if abbreviateConstants => (Nil, e)
        case ChurchNumeral(_) if abbreviateChurchNumerals => (Nil, e)
        case Abstraction(argument, body) =>
          val (subVars, subExpr) = getVarsAndBody(body)
          ((argument :: subVars), subExpr)
        case _ => (Nil, e)
      }
      val (vars, subExpr) = getVarsAndBody(abstraction)
      "λ" + (vars map { _.name } mkString) + "·" + printWithoutParens(subExpr)
    case Variable(name) => name
  }

}

