package com.github.mdr.tapl.untyped

import scala.util.parsing.combinator._

object Parser extends Parsers {

  def apply(input: String): List[Command] = parseAll(topLevel, input).get

}

class Parsers extends RegexParsers {

  override val whiteSpace = """(\s|/\*([^/]|[^*]/)*\*/)+""".r

  def lowerCaseId = """[a-z]+""".r

  def topLevel: Parser[List[Command]] = rep(command <~ ";")

  def command = lowerCaseId <~ binder ^^ Binder | term ^^ Eval

  def binder = "/"

  def term: Parser[Term] = lambdaTerm | appTerm

  def lambdaTerm = ("lambda" ~> lowerCaseId) ~ ("." ~> term) ^^ {
    case variableName ~ body => Abstraction(variableName, body)
  }

  private def mkApplication(left: Term, right: Term) = Application(left, right)

  def appTerm = chainl1(aterm, success(Application))

  def aterm = "(" ~> term <~ ")" | lowerCaseId ^^ Variable

}

