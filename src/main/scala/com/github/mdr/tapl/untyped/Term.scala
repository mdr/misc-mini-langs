package com.github.mdr.tapl.untyped

sealed trait Term {

  override def toString = Printer.print(this)

}

case class Variable(name: String) extends Term
case class Abstraction(parameter: String, body: Term) extends Term
case class Application(left: Term, right: Term) extends Term
