package com.github.mdr.tapl.arith

import PartialFunction._
import scala.annotation.tailrec

object Evaluator {

  private def isNumericVal(t: Term): Boolean = cond(t) {
    case Zero ⇒ true
    case Succ(t) ⇒ isNumericVal(t)
  }

  private def isVal(t: Term) = cond(t) {
    case True | False | _ if isNumericVal(t) ⇒ true
  }

  private def eval1(t: Term): Option[Term] = {
    implicit def autoPromoteToOption(t: Term): Option[Term] = Some(t)
    t match {
      case If(True, thenClause, _) ⇒ thenClause
      case If(False, _, elseClause) ⇒ elseClause
      case ifTerm@If(condition, _, _) ⇒
        for (reducedCondition ← eval1(condition))
          yield ifTerm.copy(condition = reducedCondition)
      case Succ(n) ⇒ eval1(n) map Succ
      case Pred(Zero) ⇒ Zero
      case Pred(Succ(n)) if isNumericVal(n) ⇒ n
      case IsZero(Zero) ⇒ True
      case IsZero(Succ(n)) if isNumericVal(n) ⇒ False
      case IsZero(n) ⇒ eval1(n) map IsZero
      case _ ⇒ None
    }
  }

  @tailrec
  def eval(t: Term): Term = eval1(t) match {
    case None ⇒ t
    case Some(nt) ⇒ eval(nt)
  }

}
