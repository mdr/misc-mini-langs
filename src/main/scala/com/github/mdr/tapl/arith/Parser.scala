package com.github.mdr.tapl.arith

import scala.util.parsing.combinator._

object Parser extends Parsers {

  def apply(input: String): List[Command] = parseAll(topLevel, input).get

}

class Parsers extends RegexParsers {

  override val whiteSpace = """(\s|/\*([^/]|[^*]/)*\*/)+""".r

  def topLevel: Parser[List[Command]] = rep(command <~ ";")

  def command = term ^^ Eval

  def term: Parser[Term] = appTerm | ifTerm

  def ifTerm = ("if" ~> term) ~ ("then" ~> term) ~ ("else" ~> term) ^^ {
    case condition ~ thenClause ~ elseClause ⇒ If(condition, thenClause, elseClause)
  }

  def appTerm = aterm | "succ" ~> aterm ^^ Succ | "pred" ~> aterm ^^ Pred | "iszero" ~> aterm ^^ IsZero

  def aterm = "(" ~> term <~ ")" | "true" ^^^ True | "false" ^^^ False | "[0-9]+".r ^^ Number.apply

}

