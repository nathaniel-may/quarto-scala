package com.nathanielmay.quarto.quarto

import scala.util.Try
import scalaz._
import Scalaz._

case class Quarto(board: Board)

case object Quarto{

  val allLines = hLines ++ vLines ++ dLines
  val hLines = indexes.foldRight(List(): List[List[Square]])(
    (dex, squares) => squares ++ List(for{ i <- indexes } yield Square(dex, i))
  )
  val vLines = indexes.foldRight(List(): List[List[Square]])(
    (dex, squares) => squares ++ List(for{ i <- indexes } yield Square(i, dex))
  )
  val dLines: List[List[Square]] = List(for( (x, y) <- indexes zip indexes) yield Square(x,y)) ++
                                   List(for( (x, y) <- indexes zip indexes.reverse) yield Square(x,y))
  //TODO can add squares for variant
  val indexes = List(I0, I1, I2, I3)

  def takeTurns(q0: => Quarto)(turns: List[(Piece, (Int,Int), Option[Piece])]) : Try[Quarto] =
    turns.foldLeft(Try(q0)) { case (game, (piece, square, active)) =>
      game flatMap (_.takeTurn(piece, square, active)) }

  def isWon(game: Quarto): Boolean = {
    if (winningLines(game).isEmpty) false else true
  }

  def winningLines(game: Quarto): List[List[Square]] = {
    allLines.filter(winningLine(game, _))
  }

  def winningLine(game: Quarto, line: List[Square]): Boolean = {
    val pieces = line flatMap {piece => game.board.squares get piece}
    val attrCounts = pieces.foldRight(Map())((piece, counts) =>
      for(attr <- piece.attrs) {counts |+| attr -> 1})
    if (4 >= attrCounts.valuesIterator.max) true else false
  }

}

sealed case class Board(squares: Map[Square, Piece], Active: Option[Piece])

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