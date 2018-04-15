package com.nathanielmay.quarto.quarto

sealed case class Board(squares: Map[Square, Piece]){
  require(!duplicateValues(squares), "map has values that appear more than once")

  private def duplicateValues[K, V](m: Map[K, V]): Boolean = m.foldLeft(false)({case (dupsExist, (_, v)) => dupsExist || 1 < m.valuesIterator.count(_ == v)})
  def isFull: Boolean               = squares.size >= 16
  def contains(sq: Square): Boolean = squares.contains(sq)
  def contains(p: Piece): Boolean   = squares.valuesIterator.contains(p)
}

object Board{
  def apply(): Board = new Board(Map())
}

sealed case class Square(h: Index, v: Index)

sealed abstract class Index(i: Int)
case object I0 extends Index(0)
case object I1 extends Index(1)
case object I2 extends Index(2)
case object I3 extends Index(3)