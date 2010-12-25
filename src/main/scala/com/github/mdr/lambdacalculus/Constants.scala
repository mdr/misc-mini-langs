package com.github.mdr.lambdacalculus

import Term._

object Constants {

  lazy val constants = sources transform { case (_, e) ⇒ Term(e) }

  private[lambdacalculus] val sources = Map(
    "PAIR" -> "λabf. f a b",
    "FST" -> "λp.p λab. a",
    "SND" -> "λp.p λab. b",
    "SUCC" -> "λnfx.f (n f x)",
    "+" -> "λmnfx.m f (n f x)",
    "*" -> "λmn.m (+ n) 0",
    "^" -> "λbe.e b", // exponentiation
    "PRED" -> "λnfx.n (λgh.h (g f)) (λu.x) (λu.u)",
    "-" -> "λmn.n PRED m",
    "TRUE" -> "λxy.x",
    "FALSE" -> "λxy.y",
    "&&" -> "λpq.p q p",
    "||" -> "λpq.p p q",
    "!" -> "λpab.p b a", /* negation */
    "ISZERO" -> "λn.n (λx.FALSE) TRUE",
    "AND" -> "λpq.p q p",
    "OR" -> "λpq.p p q",
    "NOT" -> "λpab.p b a",
    "IFTHENELSE" -> "λpab.p a b",
    "LEQ" -> "λmn.ISZERO (- m n)",
    "==" -> "λmn. AND (LEQ m n) (LEQ n m)",
    "Y" -> "λf·(λx·f (x x)) (λx·f (x x))",
    "Z" -> "λf. (λx. f (λy. x x y)) (λx. f (λy. x x y))",
    "U" -> "λux.x(uux)",
    "V" -> "λy.x(yy)",
    "S" -> "λabc.ac(bc)",
    "K" -> "λab.a",
    "I" -> "λa.a",
    "FACT" -> "λfn. (ISZERO n) 1 (* n (f (PRED n)))")

  val PAIR = "λabf. f a b".t
  val FST = "λp.p λab. a".t
  val SND = "λp.p λab. b".t
  val SUCC = "λnfx.f (n f x)".t
  val + = "λmnfx.m f (n f x)".t
  val * = "λmn.m (+ n) 0".t
  val ^ = "λbe.e b".t
  val PRED = "λnfx.n (λgh.h (g f)) (λu.x) (λu.u)".t
  val - = "λmn.n PRED m".t
  val TRUE = "λxy.x".t
  val FALSE = "λxy.y".t
  val && = "λpq.p q p".t
  val || = "λpq.p p q".t
  val ! = "λpab.p b a".t
  val ISZERO = "λn.n (λx.FALSE) TRUE".t
  val AND = "λpq.p q p".t
  val OR = "λpq.p p q".t
  val NOT = "λpab.p b a".t
  val IFTHENELSE = "λpab.p a b".t
  val LEQ = "λmn.ISZERO (- m n)".t
  val == = "λmn. AND (LEQ m n) (LEQ n m)".t
  val Y = "λf·(λx·f (x x)) (λx·f (x x))".t
  val Z = "λf. (λx. f (λy. x x y)) (λx. f (λy. x x y))".t
  val U = "U".t
  val V = "V".t
  val S = "S".t
  val K = "K".t
  val I = "I".t
  val FACT = "FACT".t

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

  }

  implicit def termToHasConstantMethods(t: Term): HasConstantMethods = new HasConstantMethods(t)

}
