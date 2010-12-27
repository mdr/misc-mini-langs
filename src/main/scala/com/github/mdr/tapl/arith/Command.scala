package com.github.mdr.tapl.arith

sealed trait Command {
  def execute()
}

case class Eval(term: Term) extends Command {
  def execute() {
    println(Evaluator.eval(term))
  }
}
