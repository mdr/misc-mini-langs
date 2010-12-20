package com.github.mdr.combinatorylogic

import Expression._

object Constants {

  lazy val constants = sources transform { case (_, e) => Expression(e) }

  private[combinatorylogic] val sources = Map(
    "B" -> "S(KS)K")

  val B = "B".e

  val X = "X".e
  val Y = "Y".e
  val Z = "Z".e
}
