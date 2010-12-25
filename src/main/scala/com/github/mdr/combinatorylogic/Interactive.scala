package com.github.mdr.combinatorylogic

object Interactive {

  def start(initialExpr: Expression) {
    var currentExpr = initialExpr
    while (true) {
      println(currentExpr)
      val redexes = currentExpr.redexes
      if (redexes.isEmpty)
        return
      for ((redex, index) <- redexes.zipWithIndex)
        println(index + ": " + redex)
      val line = readLine()
      if (line.toLowerCase.startsWith("q"))
        return
      else {
        try {
          val index = Integer.parseInt(line.trim)
          currentExpr = currentExpr.contract(redexes(index))
        } catch {
          case e: NumberFormatException =>
        }
      }
    }

  }
}
