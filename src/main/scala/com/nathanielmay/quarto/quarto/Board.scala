package com.nathanielmay.quarto.quarto

sealed class Board private (val squares: Map[Square, Piece] = Map()) {

  def isFull: Boolean = squares.size >= 16

  def contains(sq: Square): Boolean = squares.contains(sq)

  def contains(p: Piece): Boolean = squares.valuesIterator.contains(p)

  override def toString: String = {
    val indices = List(I0, I1, I2, I3)
    indices.map(h =>
      indices.map(v =>
        squares.get(Square(h, v)).fold("    ")(_.toString)
      ).mkString("|","|","|")
    ).mkString("\n", "\n", "\n")
  }

  override def equals(obj: scala.Any): Boolean = obj match {
    case obj: Board => squares == obj.squares
    case _          => false
  }
}

object Board{

  def apply(squares: Map[Square, Piece]): Option[Board] = {
    if (duplicateValues(squares)) None else Some(new Board(squares))
  }

  def apply(): Board = new Board(Map())

  private def duplicateValues[K, V](m: Map[K, V]): Boolean = m.map({case (_, v) => (v, Unit)}).size < m.size
}

sealed case class Square(h: Index, v: Index)

sealed abstract class Index(val i: Int)
case object I0 extends Index(0)
case object I1 extends Index(1)
case object I2 extends Index(2)
case object I3 extends Index(3)