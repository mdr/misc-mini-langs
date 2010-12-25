package com.github.mdr.combinatorylogic

import scala.util.parsing.combinator._

class Parser extends RegexParsers {

  def term: Parser[Term] = application | simpleTerm

  def simpleTerm = combinator | num | constant | variable | "(" ~> term <~ ")"

  def combinator = "S" ^^^ S | "K" ^^^ K | "I" ^^^ I

  def application = chainl1(simpleTerm, success(Application))

  def variable = """[a-z]'*""".r ^^ Variable

  def num = """[0-9]+""".r ^^ { ChurchNumeral(_) }

  def constant = """[^a-z\\λ\(\)\s\.·']+'*""".r ^^ {
    case name ⇒ Term(Constants.sources(name))
  }

}
