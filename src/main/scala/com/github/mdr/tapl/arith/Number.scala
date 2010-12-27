package com.github.mdr.tapl.arith

import PartialFunction._

object Number {

  def unapply(t: Term): Option[Int] = condOpt(t) {
    case Zero ⇒ 0
    case Succ(Number(n)) ⇒ n + 1
  }

  def apply(n: Int): Term = if (n == 0) Zero else Succ(apply(n - 1))

  def apply(s: String): Term = Number(Integer.parseInt(s))

}
