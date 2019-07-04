package com.nathanielmay.quarto

import com.nathanielmay.quarto.Exceptions.{DuplicatePieceError, InvalidPlacementError}
import scala.util.{Failure, Success, Try}

object Board {
  val indexes: List[Index] = List(I0, I1, I2, I3)

  def apply(): Board = new Board(Map())

  /**
    * smart constructor
    *
    * @param tiles map identifying the spaces with pieces on them
    * @return board iff map contains a valid game state
    */
  private[quarto] def apply(tiles: Map[Tile, Piece] = Map()): Try[Board] =
    if (noDuplicatePieces(tiles))
      Success(new Board(tiles))
    else
      Failure(DuplicatePieceError)

  private def noDuplicatePieces(m: Map[Tile, Piece]): Boolean =
    m.map{case (_, v) => (v, Unit)}.size == m.size
}

/** Board for a Quarto game
  *
  * @param tiles map identifying the spaces with pieces on them
  *
  * @constructor only called by apply functions
  */
final case class Board private (tiles: Map[Tile, Piece]) {
  def isFull: Boolean = tiles.size >= 16
  def size: Int = tiles.size
  def contains(t: Tile): Boolean = tiles.contains(t)
  def contains(p: Piece): Boolean = tiles.valuesIterator.contains(p)
  def get(t: Tile): Option[Piece] = tiles.get(t)

  def place(p: Piece, t: Tile): Try[Board] =
    if (contains(p))
      Failure(DuplicatePieceError)
    else if (contains(t))
      Failure(InvalidPlacementError)
    else
      Board(tiles + (t -> p))


  override def toString: String = {
    Board.indexes.map(h =>
      Board.indexes.map(v =>
        tiles.get(Tile(h, v)).fold("    ")(_.toString)
      ).mkString("|","|","|\n")
    ).mkString
  }
}

/**
  * Space on the board
  *
  * @param h horizontal index
  * @param v vertical index
  */
final case class Tile(h: Index, v: Index)

/** Singleton types for board indexes make
  * illegal board locations unrepresentable
  *
  * i - value of index
  */
sealed trait Index { val i: Int }
case object I0 extends Index { val i = 0 }
case object I1 extends Index { val i = 1 }
case object I2 extends Index { val i = 2 }
case object I3 extends Index { val i = 3 }