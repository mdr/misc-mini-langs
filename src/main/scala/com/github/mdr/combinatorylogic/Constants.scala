package com.github.mdr.combinatorylogic

import Term._
import PartialFunction._

object Constants {

  lazy val constants = sources transform { case (_, t) ⇒ Term(t) }

  private[combinatorylogic] val sources = Map(
    "B" -> "S(KS)K",
    "B'" -> "S(K(SB))K",
    "C" -> "S(B B S)(KK)",
    "W" -> "SS(KI)")

  val B = "B".t
  val C = "C".t
  val W = "W".t

  val a = "a".t
  val b = "b".t
  val c = "c".t
  val d = "d".t
  val e = "e".t
  val f = "f".t
  val g = "g".t
  val h = "h".t
  val i = "i".t
  val j = "j".t
  val k = "k".t
  val l = "l".t
  val m = "m".t
  val n = "n".t
  val o = "o".t
  val p = "p".t
  val q = "q".t
  val r = "r".t
  val s = "s".t
  val t = "t".t
  val u = "u".t
  val v = "v".t
  val w = "w".t
  val x = "x".t
  val y = "y".t
  val z = "z".t

  class HasConstantMethods(term: Term) {

    def a = term("a")
    def b = term("b")
    def c = term("c")
    def d = term("d")
    def e = term("e")
    def f = term("f")
    def g = term("g")
    def h = term("h")
    def i = term("i")
    def j = term("j")
    def k = term("k")
    def l = term("l")
    def m = term("m")
    def n = term("n")
    def o = term("o")
    def p = term("p")
    def q = term("q")
    def r = term("r")
    def s = term("s")
    def t = term("t")
    def u = term("u")
    def v = term("v")
    def w = term("w")
    def x = term("x")
    def y = term("y")
    def z = term("z")

    def S = term("S")
    def K = term("K")
    def I = term("I")
    def B = term("B")
    def C = term("C")
    def W = term("W")

  }

  implicit def termToHasConstantMethods(t: Term): HasConstantMethods = new HasConstantMethods(t)

}
