package com.nathanielmay.quarto.quarto

import scala.util.Try
import scalaz._
import Scalaz._

case class Quarto(board: Board, active: Option[Piece]){

  def takeTurn(piece: Piece, square: Square, forOpponent: Option[Piece]): Try[Quarto] = {
    Try({
      if (!this.isValid)      throw InvalidGameError("not a valid game")
      if (Quarto.isWon(this)) throw InvalidGameError("cannot take a turn on a completed game")

      active match {
        case Some(p) if p != piece          => throw BadTurnError(s"must place the active piece: $active. actual piece placed: $piece")
        case None if this != Quarto.newGame => throw BadTurnError(s"no active piece set for in progress game")
        case _                              =>
      }

      if (board.squares.contains(square))          throw BadTurnError(s"square $square is already occupied")
      if (board.squares.values.exists(_ == piece)) throw BadTurnError(s"piece $piece has already been placed") //TODO should never trigger if forOpponent is checked for

      forOpponent match {
        case Some(p) if board.squares.values.exists(_ == p) &&
                        !Quarto.willWin(this, piece, square)    => throw BadTurnError(s"piece for opponent $p has already been placed")
        case Some(p) if p == piece                              => throw BadTurnError(s"piece being placed and piece for opponent are the same: $p")
        case None    if !Quarto.willWin(this, piece, square) &&
                        !isLastTurn                             => throw BadTurnError(s"no piece chosen for opponent and game still has more turns")
        case _                                                  =>
      }

      forOpponent match {
        case Some(p) if board.squares.values.exists(_ == p) &&
                     Quarto.willWin(this, piece, square)    => Quarto(Board(board.squares + (square -> piece)), None)
        case _                                              => Quarto(Board(board.squares + (square -> piece)), forOpponent)
      }
    })
  }

  //TODO won't need if Boards have unique maps instead
  def isValid: Boolean = {
    board.isValid && (active match {
      case Some(p) => !board.contains(p)
      case None => board == Board.newBoard || Quarto.isWon(this) || board.isFull
    })
  }

  def isLastTurn: Boolean = board.squares.size == 15
}

case object Quarto{
  val newGame = Quarto(Board.newBoard, None)

  private val indexes = List(I0, I1, I2, I3)
  private val hLines = indexes.map(h => indexes.map(v => Square(h, v)))
  private val vLines = indexes.map(v => indexes.map(h => Square(h, v)))
  private val dLines = List(indexes zip indexes map {case (h, v) => Square(h, v)},
                                        indexes zip indexes.reverse map {case (h, v) => Square(h, v)})
  //TODO can add squares for variant
  val allLines: List[List[Square]] = hLines ++ vLines ++ dLines

  def takeTurns(q0: => Quarto)(turns: List[(Piece, Square, Option[Piece])]) : Try[Quarto] =
    turns.foldLeft(Try(q0)) { case (game, (piece, square, active)) =>
      game flatMap (_.takeTurn(piece, square, active))
    }

  def isWon(game: Quarto): Boolean = {
    allLines.exists(winningLine(game, _))
  }

  def winningLine(game: Quarto, line: List[Square]): Boolean = {
    val pieces = line flatMap {piece => game.board.squares get piece}
    val attrCounts = pieces.foldLeft(Map[Attribute, Int]())((counts, piece) =>
      piece.attrs.foldLeft(counts)((m, attr) => m |+| Map(attr -> 1)))
    attrCounts.exists(4 <= _._2)
  }

  def willWin(game: Quarto, piece: Piece, square: Square): Boolean = {
    Quarto.isWon(Quarto(Board(game.board.squares + (square -> piece)), None))
  }

}

sealed case class Board(squares: Map[Square, Piece]){
  def contains(p: Piece): Boolean = squares.valuesIterator.contains(p)
  def isFull: Boolean = squares.size >= 16
  def isValid: Boolean = squares.foldLeft(true)({case (valid, (_, piece)) => valid && 1 >= squares.valuesIterator.count(_ == piece)})
}

case object Board { val newBoard = Board(Map()) }

sealed case class Piece(color: Color, size: Size, shape: Shape, top: Top) {
  val attrs = List(color, size, shape, top)
  override def toString: String = "" + color + size + shape + top
}

sealed case class Square(h: Index, v: Index)

sealed abstract class Index(i: Int)
case object I0 extends Index(0)
case object I1 extends Index(1)
case object I2 extends Index(2)
case object I3 extends Index(3)

sealed trait Attribute

trait Color extends Attribute
case object White extends Color { override def toString = "W" }
case object Black extends Color { override def toString = "B" }

trait Size extends Attribute
case object Large extends Size { override def toString = "L" }
case object Small extends Size { override def toString = "S" }

trait Shape extends Attribute
case object Round extends Shape { override def toString = "R" }
case object Square extends Shape { override def toString = "Q" }

trait Top extends Attribute
case object Flat extends Top { override def toString = "F" }
case object Hole extends Top { override def toString = "H" }

abstract class QuartoError(msg: String) extends Exception {
  override def toString: String = super.toString + s"\n$msg"
}
case class BadTurnError(msg: String) extends QuartoError(msg)
case class InvalidGameError(msg: String) extends QuartoError(msg)