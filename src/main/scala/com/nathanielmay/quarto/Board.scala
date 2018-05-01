package com.nathanielmay.quarto

import com.nathanielmay.quarto.piece.Piece

object Board{

  private def noDuplicatePieces(m: Map[Square, Piece]): Boolean = {
    m.map({case (_, v) => (v, Unit)}).size == m.size
  }

}

sealed case class Board (squares: Map[Square, Piece] = Map()) {
  require(Board.noDuplicatePieces(squares) , "the given map has two different squares with identical pieces")

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

sealed case class Square(h: Index, v: Index)

sealed abstract class Index(val i: Int)
case object I0 extends Index(0)
case object I1 extends Index(1)
case object I2 extends Index(2)
case object I3 extends Index(3)