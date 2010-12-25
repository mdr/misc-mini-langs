package com.github.mdr.combinatorylogic

import Constants._
import Term._
import PartialFunction._

object ChurchNumeral {

  def apply(s: String): Term = this(Integer.parseInt(s))

  def apply(n: Int): Term = n match {
    case 0 ⇒ (K I)
    case _ ⇒ (S B)(ChurchNumeral(n - 1))
  }

  def unapply(term: Term): Option[Int] = condOpt(term) {
    case K * I ⇒ 0
    case S * B * ChurchNumeral(n) ⇒ n + 1
  }

}

