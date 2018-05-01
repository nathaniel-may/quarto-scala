package com.nathanielmay.quarto.board

import com.nathanielmay.quarto.piece.Piece

object Board{

  val indexes: List[Index] = List(I0, I1, I2, I3)

  private def noDuplicatePieces(m: Map[Square, Piece]): Boolean = {
    m.map({case (_, v) => (v, Unit)}).size == m.size
  }

}

sealed case class Board(squares: Map[Square, Piece] = Map()) {
  require(Board.noDuplicatePieces(squares) , "the given map has two different squares with identical pieces")

  def isFull: Boolean = squares.size >= 16

  def contains(sq: Square): Boolean = squares.contains(sq)

  def contains(p: Piece): Boolean = squares.valuesIterator.contains(p)

  override def toString: String = {
    Board.indexes.map(h =>
      Board.indexes.map(v =>
        squares.get(Square(h, v)).fold("    ")(_.toString)
      ).mkString("|","|","|")
    ).mkString("\n", "\n", "\n")
  }
  
}

/** Square on the board
  *
  * @param h horizontal index
  * @param v vertical index
  */
sealed case class Square(h: Index, v: Index)

/** Singleton types for board indexes make
  * illegal board locations unrepresentable
  *
  * @param i value of index
  */
sealed abstract class Index(val i: Int)
case object I0 extends Index(0)
case object I1 extends Index(1)
case object I2 extends Index(2)
case object I3 extends Index(3)