package com.github.mdr.tapl.arith

import PartialFunction._

object Printer {

  def print(t: Term): String = t match {
    case Zero ⇒ "0"
    case True ⇒ "true"
    case False ⇒ "false"
    case Succ(Number(n)) ⇒ (n + 1).toString
    case Succ(t2) ⇒ "(succ " + print(t2) + ")"
    case Pred(n) ⇒ "(pred " + print(n) + ")"
    case IsZero(n) ⇒ "(iszero " + print(n) + ")"
    case If(condition, thenClause, elseClause) ⇒
      "(if " + print(condition) + " then " + print(thenClause) + " else " + print(elseClause) + ")"
  }

}
