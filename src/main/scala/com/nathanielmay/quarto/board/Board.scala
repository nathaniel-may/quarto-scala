package com.nathanielmay.quarto.board

import com.nathanielmay.quarto.piece.Piece

object Board{

  val indexes: List[Index] = List(I0, I1, I2, I3)

  //returns some board iff squares is a valid game state
  def apply(squares: Map[Square, Piece] = Map()): Option[Board] = {
    if (Board.noDuplicatePieces(squares))
      Some(new Board(squares))
    else
      None //TODO: Make custom sum type where this branch contains error info?
  }

  def apply(): Board = new Board(Map())

  private def noDuplicatePieces(m: Map[Square, Piece]): Boolean = {
    m.map({case (_, v) => (v, Unit)}).size == m.size
  }

}

//private constructor forces creation from apply methods which contains error handling
sealed case class Board private (squares: Map[Square, Piece]) {

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