package com.github.mdr.lambdacalculus

import scala.util.parsing.combinator._
import PartialFunction._

sealed abstract trait Expression {

  def substitute(variable: Variable, replacement: Expression): Expression

  def freeVariables: Set[Variable]

  def boundVariables: Set[Variable] = getBoundVariables(Set())

  private[lambdacalculus] def getBoundVariables(bindingVariables: Set[Variable]): Set[Variable]

  def redexes: Set[Redex]

  def isABetaNormalForm = redexes.isEmpty

  def contract(redex: Redex): Expression = contract(redex.position)

  def contract(position: Position): Expression

  def α_==(other: Expression) = alphaEquivalent(other)

  def alphaEquivalent(other: Expression): Boolean

  def contains(other: Expression): Boolean

  def b = betaReduction

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
      if (f.freeVariables contains x) this else f
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

  import Expression._
  override def toString: String =
    PrettyPrinter(abbreviateChurchNumerals, abbreviateConstants, omitParentheses).print(this)

}

object Expression extends LambdaParsers {

  var abbreviateChurchNumerals = true
  var abbreviateConstants = true
  var omitParentheses = true

  def apply(n: Int): Expression = ChurchNumeral(n)

  def apply(input: String): Expression = {
    (parseAll(expression, input): @unchecked) match {
      case Success(e, _) => e
    }
  }

  lazy val constants = sources transform { (name, src) => Expression(src) }
  lazy val constantsToString = constants map { _.swap }
  implicit def string2Expression(s: String): Expression = Expression(s)
  implicit def int2Expression(n: Int): Expression = Expression(n)

  implicit def string2Expressionable(s: String): Expressionable = new Expressionable(s)
  class Expressionable(s: String) {
    def e = Expression(s)
  }

  private[lambdacalculus] val sources = Map(
    "PAIR" -> "λabf. f a b",
    "FST" -> "λp.p λab. a",
    "SND" -> "λp.p λab. b",
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
    "Z" -> "λf. (λx. f (λy. x x y)) (λx. f (λy. x x y))",
    "U" -> "λux.x(uux)",
    "V" -> "λy.x(yy)",
    "S" -> "λabc.ac(bc)",
    "K" -> "λab.a",
    "I" -> "λa.a",
    "FACT" -> "λfn. (ISZERO n) 1 (* n (f (PRED n)))")
 
}

case class Variable(name: String) extends Expression {

  def substitute(variable: Variable, replacement: Expression): Expression =
    if (variable == this) replacement else this

  def freeVariables: Set[Variable] = Set(this)

  private[lambdacalculus] def getBoundVariables(bindingVariables: Set[Variable]): Set[Variable] =
    if (bindingVariables contains this) Set(this) else Set()

  def alphaEquivalent(other: Expression): Boolean = cond(other) { case Variable(name2) => name == name2 }

  def contains(other: Expression) = this == other

  def redexes = Set()

  def contract(position: Position): Expression = throw new IllegalArgumentException("Invalid contraction of redex: " + this)

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
      Abstraction(freshVar, body.substitute(parameter, freshVar).substitute(variable, replacement))
    } else
      copy(body = body.substitute(variable, replacement))

  def freeVariables: Set[Variable] = body.freeVariables - parameter

  private[lambdacalculus] def getBoundVariables(bindingVariables: Set[Variable]) =
    body.getBoundVariables(bindingVariables + parameter) + parameter

  def alphaEquivalent(other: Expression): Boolean = cond(other) {
    case Abstraction(otherArgument, otherBody) => body alphaEquivalent otherBody.substitute(otherArgument, parameter)
  }

  def contains(other: Expression) = parameter == other || (body contains other)

  def redexes = body.redexes map { redex => redex.prependChoice(true) }

  def contract(position: Position): Expression = {
    val Position(choices) = position
    if (choices.isEmpty)
      throw new IllegalArgumentException("Invalid contraction of redex: " + this)
    else if (choices.head == false)
      throw new IllegalArgumentException("Invalid contraction of redex: " + this)
    else
      copy(body = body contract position.tail)
  }

}

case class Application(function: Expression, argument: Expression) extends Expression {

  def substitute(variable: Variable, replacement: Expression): Expression =
    Application(function.substitute(variable, replacement), argument.substitute(variable, replacement))

  def freeVariables: Set[Variable] = function.freeVariables ++ argument.freeVariables

  private[lambdacalculus] def getBoundVariables(bindingVariables: Set[Variable]): Set[Variable] =
    function.getBoundVariables(bindingVariables) ++ argument.getBoundVariables(bindingVariables)

  def alphaEquivalent(other: Expression): Boolean = cond(other) {
    case Application(otherFunction, otherArgument) => (function alphaEquivalent otherFunction) && (argument alphaEquivalent otherArgument)
  }

  def contains(other: Expression) = (function contains other) || (argument contains other)

  def redexes = (function.redexes map { _.prependChoice(false) }) ++ (argument.redexes map { _.prependChoice(true) }) ++ condOpt(function) {
    case Abstraction(argument, body) => Redex(this, Position(Vector()))
  }

  def contract(position: Position): Expression = {
    val Position(choices) = position
    if (choices.isEmpty) {
      function match {
        case Abstraction(variable, body) => body.substitute(variable, argument)
        case _ => throw new IllegalArgumentException("Invalid contraction of redex: " + this)
      }
    } else if (choices.head == false)
      copy(function = function contract position.tail)
    else
      copy(argument = argument contract position.tail)
  }

}

case class Redex(application: Application, position: Position) {

  def prependChoice(choice: Boolean) = copy(position = position.prependChoice(choice))

}

case class Position(choices: Vector[Boolean]) {

  def prependChoice(choice: Boolean) = Position(choice +: choices)

  def tail = Position(choices.tail)

}

class LambdaParsers extends RegexParsers {
  def expression: Parser[Expression] = application | simpleExpression

  def simpleExpression: Parser[Expression] = abstraction | variable | num | constant | "(" ~> expression <~ ")"

  def abstraction: Parser[Expression] =
    (lambda ~> parameters <~ dot) ~ expression ^^ {
      case args ~ exp => (args :\ exp) { Abstraction(_, _) }
    }

  def application: Parser[Expression] =
    simpleExpression ~ rep1(simpleExpression) ^^ {
      case exp ~ exps => (exp /: exps) { (app, e) => Application(app, e) }
    }

  def parameters: Parser[List[Variable]] = rep1(variable)

  def lambda: Parser[String] = """\\|λ""".r

  def dot: Parser[String] = ".|·".r

  def variable: Parser[Variable] = """[a-z]'*""".r ^^ { Variable(_) }

  def num: Parser[Expression] = """[0-9]+""".r ^^ { s => ChurchNumeral(Integer.parseInt(s)) }

  def constant: Parser[Expression] = """[^a-z\\λ\(\)\s\.·']+""".r ^^ {
    case name => Expression(Expression.sources(name))
  }
}

object VariableNames {

  private val letters = "abcdefghijklmnopqrstuvwyz".toList map { _.toString }

  def getFirstNameNotIn(names: Set[String]): String =
    (letters filterNot names headOption) getOrElse { throw new UnsupportedOperationException("TODO: Add primes") }

}

trait ReductionStrategy {

  def findRedex(expression: Expression): Option[Redex]

}

object ChurchNumeral {

  def apply(n: Int): Expression = {
    require(n >= 0)
    val x = Variable("x")
    val f = Variable("f")
    var body: Expression = x
    for (i <- 1 to n) { body = Application(f, body) }
    Abstraction(f, Abstraction(x, body))
  }

  def unapply(expression: Expression): Option[Int] = expression match {
    case Abstraction(var1, Abstraction(var2, subExpression)) => iteratedApplication(var1, var2, subExpression)
    case _ => None
  }

  def iteratedApplication(functionVar: Variable, zeroVar: Variable, expression: Expression): Option[Int] = expression match {
    case `zeroVar` => Some(0)
    case Application(`functionVar`, subExpression) => iteratedApplication(functionVar, zeroVar, subExpression) map { _ + 1 }
    case _ => None
  }

}

case class PrettyPrinter(
  abbreviateChurchNumerals: Boolean = true,
  abbreviateConstants: Boolean = true,
  omitParens: Boolean = true) {

  def print(expression: Expression): String =
    if (omitParens) printWithoutParens(expression)
    else printWithParens(expression)

  object Constant {
    def unapply(expression: Expression): Option[String] =
      (for ((s, e) <- Expression.constants if expression alphaEquivalent e) yield s).headOption

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

object Constants {
  import Expression._

  val PAIR = Expression("λabf. f a b")
  val FST = Expression("λp.p λab. a")
  val SND = Expression("λp.p λab. b")
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
  val U = Expression("U")
  val V = Expression("V")
  val S = "S".e
  val K = "K".e
  val I = "I".e
  val FACT = Expression("FACT")
  val a = "a".e
  val b = "b".e
  val c = "c".e
  val d = "d".e
  val e = "e".e
  val f = "f".e
  val g = "g".e
  val h = "h".e
  val i = "i".e
  val j = "j".e
  val k = "k".e
  val l = "l".e
  val m = "m".e
  val n = "n".e
  val o = "o".e
  val p = "p".e
  val q = "q".e
  val r = "r".e
  val s = "s".e
  val t = "t".e
  val u = "u".e
  val v = "v".e
  val w = "w".e
  val x = "x".e
  val y = "y".e
  val z = "z".e
}

object DelMe {

  var e = Expression("""(\xyz.xz(yz))((\xy.yx)u)((\xy.yx)v)w""")
  var continue = true
  while (continue) {
    println(e)
    e.redexes.headOption match {
      case Some(redex) =>
        println("  -> contracting " + redex)
        e = e contract redex
      case None =>
        continue = false
    }
  }

}
