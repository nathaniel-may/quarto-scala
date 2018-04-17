package com.nathanielmay.quarto.quarto

sealed case class Board(squares: Map[Square, Piece]){
  require(!duplicateValues(squares), "map has values that appear more than once")

  private def duplicateValues[K, V](m: Map[K, V]): Boolean = m.foldLeft(false)({case (dupsExist, (_, v)) => dupsExist || 1 < m.valuesIterator.count(_ == v)})
  def isFull: Boolean               = squares.size >= 16
  def contains(sq: Square): Boolean = squares.contains(sq)
  def contains(p: Piece): Boolean   = squares.valuesIterator.contains(p)
  override def toString: String = "\n|" + squares.foldLeft(Array.fill(16)("    "))({
    case (arr, (sq, piece)) => arr.updated(sq.v.i + (sq.h.i * 4), piece.toString)
  }).zipWithIndex.map({
    case (str, i) if (i+1)%4 == 0 => str + "|\n"
    case (str, _)                   => str
  }).mkString("|")
}

object Board{
  def apply(): Board = new Board(Map())
}

sealed case class Square(h: Index, v: Index)

sealed abstract class Index(val i: Int)
case object I0 extends Index(0)
case object I1 extends Index(1)
case object I2 extends Index(2)
case object I3 extends Index(3)