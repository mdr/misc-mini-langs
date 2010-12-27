package com.github.mdr.tapl.untyped

sealed trait Command {
  def execute()
}

case class Eval(term: Term) extends Command {
  def execute() {
    println(Evaluator.eval(term))
  }
}

case class Binder(variable: String) extends Command {
  def execute() {}
}
