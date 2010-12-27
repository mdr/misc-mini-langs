package com.github.mdr.tapl.untyped

import PartialFunction._

object Printer {

  def print(t: Term): String = t match {
    case Variable(name) => name
    case Abstraction(parameter, body) => "(lambda " + parameter + " . " + body + ")"
    case Application(left, right) => "(" + print(left) + " " + print(right) + ")"
  }

}
