package com.github.mdr.lambdacalculus

import scala.util.parsing.combinator._

class Parser extends RegexParsers {

  def expression: Parser[Expression] = application | simpleExpression

  def simpleExpression = abstraction | num | variable | constant | "(" ~> expression <~ ")"

  def abstraction = (lambda ~> parameters <~ dot) ~ expression ^^ {
    case params ~ body => params.foldRight(body)(Abstraction)
  }

  def lambda = """\\|λ""".r

  def dot = ".|·".r

  def application = chainl1(simpleExpression, success(Application))

  def parameters = rep1(variable)

  def variable = """[a-z]'*""".r ^^ Variable

  def num = """[0-9]+""".r ^^ { ChurchNumeral(_) }

  def constant = """[^a-z\\λ\(\)\s\.·']+""".r ^^ {
    case name => Expression(Constants.sources(name))
  }

}
