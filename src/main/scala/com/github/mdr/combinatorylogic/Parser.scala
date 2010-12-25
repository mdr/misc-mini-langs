package com.github.mdr.combinatorylogic

import scala.util.parsing.combinator._

class Parser extends RegexParsers {

  def expression: Parser[Expression] = application | simpleExpression

  def simpleExpression = combinator | constant | variable | "(" ~> expression <~ ")"

  def combinator = "S" ^^^ S | "K" ^^^ K | "I" ^^^ I

  def application = chainl1(simpleExpression, success(Application))

  def variable = """[a-z]'*""".r ^^ { Variable(_) }

  def constant = """[^a-z\\λ\(\)\s\.·']+'*""".r ^^ {
    case name => Expression(Constants.sources(name))
  }

}
