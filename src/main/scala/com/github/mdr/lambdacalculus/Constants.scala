package com.github.mdr.lambdacalculus

import Expression._

object Constants {

  lazy val constants = sources transform { case (_, e) => Expression(e) }

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

  val PAIR = "λabf. f a b".e
  val FST = "λp.p λab. a".e
  val SND = "λp.p λab. b".e
  val SUCC = "λnfx.f (n f x)".e
  val + = "λmnfx.m f (n f x)".e
  val * = "λmn.m (+ n) 0".e
  val ^ = "λbe.e b".e
  val PRED = "λnfx.n (λgh.h (g f)) (λu.x) (λu.u)".e
  val - = "λmn.n PRED m".e
  val TRUE = "λxy.x".e
  val FALSE = "λxy.y".e
  val && = "λpq.p q p".e
  val || = "λpq.p p q".e
  val ! = "λpab.p b a".e
  val ISZERO = "λn.n (λx.FALSE) TRUE".e
  val AND = "λpq.p q p".e
  val OR = "λpq.p p q".e
  val NOT = "λpab.p b a".e
  val IFTHENELSE = "λpab.p a b".e
  val LEQ = "λmn.ISZERO (- m n)".e
  val == = "λmn. AND (LEQ m n) (LEQ n m)".e
  val Y = "λf·(λx·f (x x)) (λx·f (x x))".e
  val Z = "λf. (λx. f (λy. x x y)) (λx. f (λy. x x y))".e
  val U = "U".e
  val V = "V".e
  val S = "S".e
  val K = "K".e
  val I = "I".e
  val FACT = "FACT".e

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
}
