package com.github.mdr.tapl.untyped

import scala.io.Source

object Main {

  def main(args: Array[String]) {
    val text = Source.fromFile(args(0)).mkString
    val commands = Parser(text)
    commands foreach { _.execute() }
  }

}
