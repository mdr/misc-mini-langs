package com.github.mdr.lambdacalculus

import scala.util.parsing.combinator._

class Parser extends RegexParsers {

  def term: Parser[Term] = application | simpleTerm

  def simpleTerm = abstraction | num | variable | constant | "(" ~> term <~ ")"

  def abstraction = (lambda ~> parameters <~ dot) ~ term ^^ {
    case params ~ body ⇒ params.foldRight(body)(Abstraction)
  }

  def lambda = """\\|λ""".r

  def dot = ".|·".r

  def application = chainl1(simpleTerm, success(Application))

  def parameters = rep1(variable)

  def variable = """[a-z]'*""".r ^^ Variable

  def num = """[0-9]+""".r ^^ { ChurchNumeral(_) }

  def constant = """[^a-z\\λ\(\)\s\.·']+""".r ^^ {
    case name ⇒ Term(Constants.sources(name))
  }

}
