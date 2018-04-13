package com.nathanielmay.quarto.quarto

sealed case class Board(squares: Map[Square, Piece]){
  def contains(p: Piece): Boolean = squares.valuesIterator.contains(p)
  def isFull: Boolean = squares.size >= 16
  def isValid: Boolean = squares.foldLeft(true)({case (valid, (_, piece)) => valid && 1 >= squares.valuesIterator.count(_ == piece)})
}

case object Board { val newBoard = Board(Map()) }

sealed case class Square(h: Index, v: Index)

sealed abstract class Index(i: Int)
case object I0 extends Index(0)
case object I1 extends Index(1)
case object I2 extends Index(2)
case object I3 extends Index(3)