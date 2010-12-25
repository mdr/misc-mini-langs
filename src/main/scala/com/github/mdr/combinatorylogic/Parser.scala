package com.github.mdr.combinatorylogic

import scala.util.parsing.combinator._

class Parser extends RegexParsers {

  def expression: Parser[Expression] = application | simpleExpression

  def simpleExpression = combinator | num | constant | variable | "(" ~> expression <~ ")"

  def combinator = "S" ^^^ S | "K" ^^^ K | "I" ^^^ I

  def application = chainl1(simpleExpression, success(Application))

  def variable = """[a-z]'*""".r ^^ Variable

  def num = """[0-9]+""".r ^^ { ChurchNumeral(_) }

  def constant = """[^a-z\\λ\(\)\s\.·']+'*""".r ^^ {
    case name => Expression(Constants.sources(name))
  }

}
