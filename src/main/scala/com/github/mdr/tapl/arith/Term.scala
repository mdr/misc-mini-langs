package com.github.mdr.tapl.arith

sealed trait Term {

  override def toString = Printer.print(this)

}

case class If(condition: Term, thenClause: Term, elseClause: Term) extends Term
case class Succ(term: Term) extends Term
case class Pred(term: Term) extends Term
case class IsZero(term: Term) extends Term
case object True extends Term
case object False extends Term
case object Zero extends Term
