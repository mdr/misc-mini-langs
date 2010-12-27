package com.github.mdr.whilesemantics

import scala.util.parsing.combinator._

class WhileParsers extends RegexParsers {

  def variable = "[a-zA-Z]".r ^^ Var
  def num = """-?\d+""".r ^^ { s => Num(Integer.parseInt(s)) }
  def plus = (aexp2 <~ "+") ~ aexp2 ^^ { case a1 ~ a2 => a1 + a2 }
  def minus = (aexp2 <~ "-") ~ aexp2 ^^ { case a1 ~ a2 => a1 - a2 }
  def times = (aexp3 <~ "*") ~ aexp3 ^^ { case a1 ~ a2 => a1 * a2 }
  def aexp3 = num | variable | "(" ~> aexp <~ ")"
  def aexp2 = times | aexp3
  def aexp: Parser[Aexp] = plus | minus | aexp2

  def equals = (aexp <~ "=") ~ aexp ^^ { case a1 ~ a2 => Equals(a1, a2) }
  def lessThanEquals = (aexp <~ "<=") ~ aexp ^^ { case a1 ~ a2 => a1 <= a2 }
  def bexp4 = "true" ^^^ True | "false" ^^^ False | "(" ~> bexp <~ ")"
  def bexp3 = not | bexp4
  def bexp2 = equals | lessThanEquals | bexp3
  def not = ("!" | "¬") ~> bexp4 ^^ { _.! }
  def and = (bexp2 <~ ("&&" | "∧")) ~ bexp2 ^^ { case b1 ~ b2 => b1 && b2 }
  def bexp: Parser[Bexp] = and | bexp2

  def assign = (variable <~ ":=") ~ aexp ^^ { case v ~ a => Assign(v, a) }
  def skip = "skip" ^^^ Skip
  def ifThenElse = ("if" ~> bexp) ~ ("then" ~> stm) ~ ("else" ~> stm) ^^ {
    case cond ~ thenClause ~ elseClause => IfThenElse(cond, thenClause, elseClause)
  }
  def whileStmt = ("while" ~> bexp) ~ ("do" ~> stm) ^^ { case cond ~ body => WhileStm(cond, body) }
  def stm1 = assign | skip | ifThenElse | whileStmt | "(" ~> stm <~ ")"
  def sequence = (stm1 <~ ";") ~ stm ^^ { case s1 ~ s2 => Seq(s1, s2) }
  def stm: Parser[Stm] = sequence | stm1

}

object WhileParser extends WhileParsers {

  def stm(input: String): Stm = parseAll(super.stm, input).get
  def aexp(input: String): Aexp = parseAll(super.aexp, input).get
  def bexp(input: String): Bexp = parseAll(super.bexp, input).get

}
