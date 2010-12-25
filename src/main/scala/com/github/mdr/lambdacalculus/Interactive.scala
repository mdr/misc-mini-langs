package com.github.mdr.lambdacalculus

object Interactive {

  def start(initialTerm: Term) {
    var currentTerm = initialTerm
    while (true) {
      println(currentTerm)
      val redexes = currentTerm.betaRedexes
      if (redexes.isEmpty)
        return
      for ((redex, index) ← redexes.zipWithIndex)
        println(index + ": " + redex)
      val line = readLine()
      if (line.toLowerCase.startsWith("q"))
        return
      else {
        try {
          val index = Integer.parseInt(line.trim)
          currentTerm = currentTerm.contract(redexes(index))
        } catch {
          case e: NumberFormatException ⇒
        }
      }
    }

  }
}
