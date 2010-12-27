package com.github.mdr.whilesemantics

class State(bindings: Map[String, Int]) extends (String => Int) {

  def apply(v: String): Int = bindings.get(v).getOrElse(0)

  def apply(v: Var): Int = apply(v.name)

  override def toString = "State(" + bindings + ")"

  def subst(v: String, n: Int): State = new State(bindings + (v -> n))

  def subst(v: Var, n: Int): State = subst(v.name, n)

}

object State {

  def apply(bindings: (String, Int)*) = new State(Map(bindings: _*))

}

object A {

  def apply(a: Aexp)(implicit s: State): Int = a match {
    case Num(n) => n
    case Var(x) => s(x)
    case Plus(a1, a2) => A(a1) + A(a2)
    case Minus(a1, a2) => A(a1) - A(a2)
    case Times(a1, a2) => A(a1) * A(a2)
  }

}

object B {
  def apply(b: Bexp)(implicit s: State): Boolean = b match {
    case True => true
    case False => false
    case Equals(a1, a2) => A(a1) == A(a2)
    case LessThanEquals(a1, a2) => A(a1) <= A(a2)
    case Not(b1) => !B(b1)
    case And(b1, b2) => B(b1) && B(b2)
  }
}

object NaturalSemantics {
  case class Conclusion(program: Stm, preState: State, postState: State)
  case class DerivationTree(rule: String, conclusion: Conclusion, premises: List[DerivationTree] = Nil)

  def findDerivationTrees(program: Stm)(implicit initialState: State): List[DerivationTree] = {
    def conclude(newState: State) = Conclusion(program, initialState, newState)
    program match {
      case Assign(x, a) => DerivationTree("ass", conclude(initialState.subst(x, A(a)))) :: Nil
      case Skip => DerivationTree("skip", conclude(initialState)) :: Nil
      case Seq(stat1, stat2) =>
        for {
          dt1 <- findDerivationTrees(stat1)(initialState)
          dt2 <- findDerivationTrees(stat2)(dt1.conclusion.postState)
        } yield DerivationTree("comp", conclude(dt2.conclusion.postState), List(dt1, dt2))
      case IfThenElse(cond, thenClause, elseClause) if B(cond) =>
        for {
          dt <- findDerivationTrees(thenClause)
        } yield DerivationTree("if_tt", conclude(dt.conclusion.postState), List(dt))
      case IfThenElse(cond, thenClause, elseClause) if !B(cond) =>
        for {
          dt <- findDerivationTrees(elseClause)
        } yield DerivationTree("if_tt", conclude(dt.conclusion.postState), List(dt))
      case WhileStm(cond, body) if B(cond) =>
	for {
	  dt1 <- findDerivationTrees(body)(initialState)
	  dt2 <- findDerivationTrees(program)(dt1.conclusion.postState)
	} yield DerivationTree("while_tt", conclude(dt2.conclusion.postState), List(dt1, dt2))
      case WhileStm(cond, _) if !B(cond) => DerivationTree("while_ff", conclude(initialState)) :: Nil
    }
  }
  
  def printDerivationTree(derivationTree: DerivationTree) {
    def print(dt: DerivationTree, depth: Int) {
      println((" " * (depth * 2)) + dt.rule + ": " + dt.conclusion)
      for (premise <- dt.premises)
	print(premise, depth + 1)
    }
    print(derivationTree, 0)

  }

}
