package com.github.mdr.lambdacalculus

import scala.util.parsing.combinator._

class Parser extends RegexParsers {

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
    case name => Expression(Constants.sources(name))
  }

}
