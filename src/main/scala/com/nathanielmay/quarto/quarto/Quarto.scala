package com.nathanielmay.quarto.quarto

import scala.util.Try
import scalaz._
import Scalaz._

case class Quarto(board: Board)

case object Quarto{

  def takeTurn(game: Quarto, toPlace: Piece, square: Square, active: Option[Piece]): Try[Quarto] = Try {
    if(!validTurn(toPlace, square, active)) throw new BadTurnError

    val newActive = if(willWin(toPlace, square)) None else active

    new Quarto(
      squares + (square -> toPlace),
      newActive,
      pieces + toPlace,
      Quarto.updateLines(lines, square, toPlace)
    )
  }

  private def validTurn(game: Quarto, toPlace: Piece, square: (Int, Int), active:Option[Piece]): Boolean = {
    val winningMove = willWin(toPlace, square)

    !squares.contains(square) &&
    (toPlace != active) || ((toPlace == active) && winningMove) &&
    //TODO active is option. It needs to be matched for Some Piece
    (game.pieces.contains(active) !activeIsPlaced(active) || (activeIsPlaced(active) && winningMove))
  }

  def takeTurns(q0: => Quarto)(turns: List[(Piece, (Int,Int), Option[Piece])]) : Try[Quarto] =
    turns.foldLeft(Try(q0)) { case (game, (piece, square, active)) =>
      game flatMap (_.takeTurn(piece, square, active)) }

  def isWon(game: Quarto): Boolean = {
    game.pieces.foldRight(Map())((ps, counts) =>
      counts |+| (ps match { case (piece, state) => linePairs(piece, state) } )
    ).valuesIterator.max >= 4
  }

  protected def linePairs(piece: Piece, state: State): Map[LinePair, Int] = {
    val attrs = List(piece.color, piece.size, piece.shape, piece.top)
    (state match {
      case Placed(square) =>
        (attrs.map(LinePair(Horizontal(square.h), _) -> 1) ++
         attrs.map(LinePair(Vertical(square.v),   _) -> 1)).toMap

        Map(LinePair(Horizontal(square.h), piece.color) -> 1,
                                 LinePair(Horizontal(square.h), piece.size)  -> 1,
                                 LinePair(Horizontal(square.h), piece.shape) -> 1,
                                 LinePair(Horizontal(square.h), piece.top)   -> 1,
                                 LinePair(Vertical(square.v),   piece.color) -> 1,
                                 LinePair(Vertical(square.v),   piece.size)  -> 1,
                                 LinePair(Vertical(square.v),   piece.shape) -> 1,
                                 LinePair(Vertical(square.v),   piece.top)   -> 1)
      case Active => Map()
    }) ++ diagonalLinePairs(piece, state)
  }

  protected def diagonalLinePairs(piece: Piece, state: State): Map[LinePair, Int] = {
    (state match {
      case Placed(square) => getDiagonal(square)
        //TODO what if it's not.
    }) match {
      case Some(line) => Map(LinePair(line, piece.color) -> 1,
                             LinePair(line, piece.size)  -> 1,
                             LinePair(line, piece.shape) -> 1,
                             LinePair(line, piece.top)   -> 1)
      case None => Map()
    }
  }

  protected def getDiagonal(square: Square): Option[Diagonal] = {
    if (square.h == square.v) Some(Diagonal(Backward))
    else if (3 == square.h.i + square.v.i) Some(Diagonal (Forward))
    else None
  }

}

sealed case class Board(pieces: Map[Square, Piece], Active: Option[Piece])

sealed case class Piece(color: Color, size: Size, shape: Shape, top: Top) {
  override def toString: String = "" + color + size + shape + top
}

sealed case class Square(h: Index, v: Index)

sealed case class Index(i: Int)
case object I0 extends Index(0)
case object I1 extends Index(1)
case object I2 extends Index(2)
case object I3 extends Index(3)

sealed trait State
case object Active extends State
case class Placed(square: Square) extends State

sealed case class LinePair(line: Line, attribute: Attribute)

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

sealed trait Angle
case object Forward  extends Angle { override def toString:String = "D1" }
case object Backward extends Angle { override def toString:String = "D0" }

sealed trait Line
case class Horizontal(i:Index)   extends Line { override def toString:String = "H" + i }
case class Vertical(i:Index)     extends Line { override def toString:String = "V" + i }
case class Diagonal(angle:Angle) extends Line { override def toString:String = "D" + angle }

class QuartoError extends Exception
class BadTurnError extends QuartoError
class InvalidBoardError extends QuartoError