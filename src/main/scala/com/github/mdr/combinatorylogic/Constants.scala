package com.github.mdr.combinatorylogic

import Expression._

object Constants {

  lazy val constants = sources transform { case (_, e) => Expression(e) }

  private[combinatorylogic] val sources = Map(
    "B" -> "S(KS)K",
    "C" -> "S(B B S)(KK)")

  val B = "B".e
  val C = "C".e

  val x = "x".e
  val y = "y".e
  val z = "z".e
}
