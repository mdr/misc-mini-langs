package com.github.mdr.combinatorylogic

import Expression._

object Constants {

  lazy val constants = sources transform { case (_, e) => Expression(e) }

  private[combinatorylogic] val sources = Map(
    "B" -> "S(KS)K",
    "B'" -> "S(K(SB))K",
    "C" -> "S(B B S)(KK)",
    "W" -> "SS(KI)")

  val B = "B".e
  val C = "C".e
  val W = "W".e

  val a = "a".e
  val b = "b".e
  val c = "c".e
  val d = "d".e
  val e = "e".e
  val f = "f".e
  val g = "g".e
  val h = "h".e
  val i = "i".e
  val j = "j".e
  val k = "k".e
  val l = "l".e
  val m = "m".e
  val n = "n".e
  val o = "o".e
  val p = "p".e
  val q = "q".e
  val r = "r".e
  val s = "s".e
  val t = "t".e
  val u = "u".e
  val v = "v".e
  val w = "w".e
  val x = "x".e
  val y = "y".e
  val z = "z".e

  class HasConstantMethods(exp: Expression) {

    def a = exp("a")
    def b = exp("b")
    def c = exp("c")
    def d = exp("d")
    def e = exp("e")
    def f = exp("f")
    def g = exp("g")
    def h = exp("h")
    def i = exp("i")
    def j = exp("j")
    def k = exp("k")
    def l = exp("l")
    def m = exp("m")
    def n = exp("n")
    def o = exp("o")
    def p = exp("p")
    def q = exp("q")
    def r = exp("r")
    def s = exp("s")
    def t = exp("t")
    def u = exp("u")
    def v = exp("v")
    def w = exp("w")
    def x = exp("x")
    def y = exp("y")
    def z = exp("z")

    def S = exp("S")
    def K = exp("K")
    def I = exp("I")
    def B = exp("B")
    def C = exp("C")
    def W = exp("W")

  }

  implicit def expressionToHasConstantMethods(e: Expression): HasConstantMethods = new HasConstantMethods(e)

}
