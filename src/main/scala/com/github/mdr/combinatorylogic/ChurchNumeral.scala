package com.github.mdr.combinatorylogic

import Constants._
import Expression._
import PartialFunction._

object ChurchNumeral {

  def apply(s: String): Expression = this(Integer.parseInt(s))

  def apply(n: Int): Expression = n match {
    case 0 => (K I)
    case _ => (S B)(ChurchNumeral(n - 1))
  }

  def unapply(expression: Expression): Option[Int] = condOpt(expression) {
    case K * I => 0
    case S * B * ChurchNumeral(n) => n + 1
  }

}

