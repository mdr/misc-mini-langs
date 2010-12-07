package com.github.mdr.lambdacalculus

import scala.util.parsing.combinator._
import PartialFunction.cond

sealed abstract class Expression {

  def substitute(variable: Var, replacement: Expression): Expression

  def freeVars: Set[Var]

  override def toString: String = PrettyPrinter().print(this)

  def α_==(other: Expression) = alphaEquivalent(other)

  def alphaEquivalent(other: Expression): Boolean

  def betaReduction: Expression = this match {
    case Application(Abstraction(arg, body), b) => body.substitute(arg, b)
    case Application(a, b) =>
      val left = Application(a.betaReduction, b)
      if (left != this)
        left
      else
        Application(a, b.betaReduction)
    case Abstraction(arg, body) => Abstraction(arg, body.betaReduction)
    case _ => this
  }

  def etaConversion: Expression = this match {
    case Abstraction(x, Application(f, y)) if x == y =>
      if (f.freeVars contains x) this else f
    case _ => this
  }

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
}

case class Var(name: String) extends Expression {

  def substitute(variable: Var, replacement: Expression): Expression =
    if (variable == this) replacement else this

  def freeVars: Set[Var] = Set(this)

  def alphaEquivalent(other: Expression): Boolean = cond(other) { case Var(name2) => name == name2 }

}

object VariableNames {

  private val letters = "abcdefghijklmnopqrstuvwyz".toList map { _.toString }

  def getFirstNameNotIn(names: Set[String]): String =
    (letters filterNot names headOption) getOrElse { throw new UnsupportedOperationException("TODO: Add primes") }

}

case class Abstraction(argument: Var, body: Expression) extends Expression {

  def substitute(variable: Var, replacement: Expression): Expression =
    if (variable == argument)
      this
    else if (!body.freeVars.contains(variable))
      this
    else if (replacement.freeVars contains argument) {
      val freeVars = body.freeVars ++ replacement.freeVars
      val freshVar = Var(VariableNames.getFirstNameNotIn(freeVars map { _.name }))
      Abstraction(freshVar, body.substitute(argument, freshVar).substitute(variable, replacement))
    } else
      copy(body = body.substitute(variable, replacement))

  def freeVars: Set[Var] = body.freeVars - argument

  def alphaEquivalent(other: Expression): Boolean = cond(other) {
    case Abstraction(otherArgument, otherBody) => body alphaEquivalent otherBody.substitute(otherArgument, argument)
  }

}

case class Application(function: Expression, argument: Expression) extends Expression {

  def substitute(variable: Var, replacement: Expression): Expression =
    Application(function.substitute(variable, replacement), argument.substitute(variable, replacement))

  def freeVars: Set[Var] = function.freeVars ++ argument.freeVars

  def alphaEquivalent(other: Expression): Boolean = cond(other) {
    case Application(otherFunction, otherArgument) => (function alphaEquivalent otherFunction) && (argument alphaEquivalent otherArgument)
  }

}

class LambdaParsers extends RegexParsers {
  def expression: Parser[Expression] = (application
    | simpleExpression)

  def simpleExpression: Parser[Expression] = (function
    | variable
    | num
    | constant
    | "(" ~> expression <~ ")")

  def function: Parser[Expression] =
    (lambda ~> arguments <~ dot) ~ expression ^^ {
      case args ~ exp => (args :\ exp) { Abstraction(_, _) }
    }

  def application: Parser[Expression] =
    simpleExpression ~ rep1(simpleExpression) ^^ {
      case exp ~ exps => (exp /: exps) { (app, e) => Application(app, e) }
    }

  def arguments: Parser[List[Var]] = rep1(variable)

  def lambda: Parser[String] = """\\|λ""".r

  def dot: Parser[String] = ".|·".r

  def variable: Parser[Var] = """[a-z]'*""".r ^^ { Var(_) }

  def num: Parser[Expression] = """[0-9]+""".r ^^ { s => ChurchNumeral(Integer.parseInt(s)) }

  def constant: Parser[Expression] = """[^a-z\\λ\(\)\s\.·']+""".r ^^ {
    case name => Expression(Expression.sources(name))
  }
}

object ChurchNumeral {

  def apply(n: Int): Expression = {
    require(n >= 0)
    val x = Var("x")
    val f = Var("f")
    var body: Expression = x
    for (i <- 1 to n) { body = Application(f, body) }
    Abstraction(f, Abstraction(x, body))
  }

  def unapply(expression: Expression): Option[Int] = expression match {
    case Abstraction(var1, Abstraction(var2, subExpression)) => iteratedApplication(var1, var2, subExpression)
    case _ => None
  }

  def iteratedApplication(functionVar: Var, zeroVar: Var, expression: Expression): Option[Int] = expression match {
    case `zeroVar` => Some(0)
    case Application(`functionVar`, subExpression) => iteratedApplication(functionVar, zeroVar, subExpression) map { _ + 1 }
    case _ => None
  }

}

case class PrettyPrinter(abbreviateChurchNumerals: Boolean = true) {

  def print(expression: Expression): String = expression match {
    case ChurchNumeral(n) if abbreviateChurchNumerals => n.toString
    case Application(left, right) =>
      val leftStr = left match {
        case Abstraction(_, _) => "(" + print(left) + ")"
        case _ => print(left)
      }
      val rightStr = right match {
        case Var(_) => print(right)
        case _ => "(" + print(right) + ")"
      }
      leftStr + " " + rightStr
    case abstraction@Abstraction(_, _) =>
      def getVarsAndBody(e: Expression): (List[Var], Expression) = e match {
        case Abstraction(argument, body) =>
          val (subVars, subExpr) = getVarsAndBody(body)
          ((argument :: subVars), subExpr)
        case other => (Nil, other)
      }
      val (vars, subExpr) = getVarsAndBody(abstraction)
      "λ" + (vars map { _.name } mkString) + "·" + print(subExpr)
    case Var(name) => name
  }

}

object Expression extends LambdaParsers {

  def main(args: Array[String]) {
    for (arg <- args) {
      val exp = Expression(arg)
      exp evaluate { println(_) }
    }
  }

  def apply(n: Int): Expression = ChurchNumeral(n)

  def apply(input: String): Expression = {
    (parseAll(expression, input): @unchecked) match {
      case Success(e, _) => e
    }
  }

  lazy val constants = sources transform { (name, src) => Expression(src) }

  implicit def string2Expression(s: String): Expression = Expression(s)
  implicit def int2Expression(n: Int): Expression = Expression(n)

  private[lambdacalculus] val sources = Map(
    "0" -> "λfx.x",
    "1" -> "λfx.f x",
    "2" -> "λfx.f (f x)",
    "3" -> "λfx.f (f (f x))",
    "SUCC" -> "λnfx.f (n f x)",
    "+" -> "λmnfx.m f (n f x)",
    "*" -> "λmn.m (+ n) 0",
    "^" -> "λbe.e b", // exponentiation
    "PRED" -> "λnfx.n (λgh.h (g f)) (λu.x) (λu.u)",
    "-" -> "λmn.n PRED m",
    "TRUE" -> "λxy.x",
    "FALSE" -> "λxy.y",
    "&&" -> "λpq.p q p",
    "||" -> "λpq.p p q",
    "!" -> "λpab.p b a", /* negation */
    "ISZERO" -> "λn.n (λx.FALSE) TRUE",
    "AND" -> "λpq.p q p",
    "OR" -> "λpq.p p q",
    "NOT" -> "λpab.p b a",
    "IFTHENELSE" -> "λpab.p a b",
    "LEQ" -> "λmn.ISZERO (- m n)",
    "==" -> "λmn. AND (LEQ m n) (LEQ n m)",
    "Y" -> "λf·(λx·f (x x)) (λx·f (x x))",
    "Z" -> "λf. (λx. f (λy. x x y)) (λx. f (λy. x x y))")
}

object Constants {

  val SUCC = Expression("λnfx.f (n f x)")
  val + = Expression("λmnfx.m f (n f x)")
  val * = Expression("λmn.m (+ n) 0")
  val ^ = Expression("λbe.e b")
  val PRED = Expression("λnfx.n (λgh.h (g f)) (λu.x) (λu.u)")
  val - = Expression("λmn.n PRED m")
  val TRUE = Expression("λxy.x")
  val FALSE = Expression("λxy.y")
  val && = Expression("λpq.p q p")
  val || = Expression("λpq.p p q")
  val ! = Expression("λpab.p b a")
  val ISZERO = Expression("λn.n (λx.FALSE) TRUE")
  val AND = Expression("λpq.p q p")
  val OR = Expression("λpq.p p q")
  val NOT = Expression("λpab.p b a")
  val IFTHENELSE = Expression("λpab.p a b")
  val LEQ = Expression("λmn.ISZERO (- m n)")
  val == = Expression("λmn. AND (LEQ m n) (LEQ n m)")
  val Y = Expression("λf·(λx·f (x x)) (λx·f (x x))")
  val Z = Expression("λf. (λx. f (λy. x x y)) (λx. f (λy. x x y))")

}
