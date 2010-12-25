package com.github.mdr.combinatorylogic

case class Redex(application: Application, position: Position) {

  def prependChoice(choice: Boolean) = copy(position = position.prependChoice(choice))

}

case class Position(choices: Vector[Boolean]) {

  def prependChoice(choice: Boolean) = Position(choice +: choices)

  def tail = Position(choices.tail)

}
