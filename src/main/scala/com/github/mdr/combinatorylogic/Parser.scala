package com.github.mdr.combinatorylogic

import scala.util.parsing.combinator._

class Parser extends RegexParsers {

  def expression: Parser[Expression] = application | simpleExpression

  def simpleExpression = combinator | variable | constant | "(" ~> expression <~ ")"

  def combinator = "S" ^^^ S | "K" ^^^ K | "I" ^^^ I

  def application = chainl1(simpleExpression, success(Application))

  def variable = """[a-zA-Z]'*""".r ^^ { Variable(_) }

  def constant = """[^a-zA-Z\\λ\(\)\s\.·']+""".r ^^ {
    case name => S //Expression(Constants.sources(name))
  }

}
