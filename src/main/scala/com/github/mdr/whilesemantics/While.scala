package com.github.mdr.whilesemantics

sealed trait Aexp {

  def +(a2: Aexp) = Plus(this, a2)
  def -(a2: Aexp) = Minus(this, a2)
  def *(a2: Aexp) = Times(this, a2)
  def equals(a2: Aexp) = Equals(this, a2)
  def <=(a2: Aexp) = LessThanEquals(this, a2)

}

case class Var(name: String) extends Aexp {
  override def toString = name
}
case class Num(n: Int) extends Aexp {
  override def toString = n.toString
}
case class Plus(a1: Aexp, a2: Aexp) extends Aexp {
  override def toString = "(" + a1 + " + " + a2 + ")"
}
case class Minus(a1: Aexp, a2: Aexp) extends Aexp {
  override def toString = "(" + a1 + " - " + a2 + ")"
}
case class Times(a1: Aexp, a2: Aexp) extends Aexp {
  override def toString = a1 + " * " + a2
}

sealed trait Bexp {

  def not = Not(this)
  def ! = not

  def &&(b2: Bexp) = And(this, b2)
  def ∧(b2: Bexp) = this && b2

}

case object True extends Bexp {
  override def toString = "true"
}
case object False extends Bexp {
  override def toString = "false"
}
case class Equals(a1: Aexp, a2: Aexp) extends Bexp {
  override def toString = "(" + a1 + " = " + a2 + ")"
}
case class LessThanEquals(a1: Aexp, a2: Aexp) extends Bexp {
  override def toString = "(" + a1 + " <= " + a2 + ")"
}
case class Not(b: Bexp) extends Bexp {
  override def toString = "¬" + b
}
case class And(b1: Bexp, b2: Bexp) extends Bexp {
  override def toString = "(" + b1 + " ∧ " + b2 + ")"
}

sealed trait Stm
case class Assign(v: Var, a: Aexp) extends Stm {
  override def toString = v + " := " + a
}
case object Skip extends Stm {
  override def toString = "skip"
}
case class Seq(s1: Stm, s2: Stm) extends Stm {
  override def toString = "(" + s1 + "; " + s2 + ")"
}
case class IfThenElse(cond: Bexp, thenClause: Stm, elseClause: Stm) extends Stm {
  override def toString = "if " + cond + " then " + thenClause + " else " + elseClause
}
case class WhileStm(cond: Bexp, body: Stm) extends Stm {
  override def toString = "while " + cond + " do " + body
}
