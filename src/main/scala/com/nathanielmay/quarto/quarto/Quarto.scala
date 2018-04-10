package com.nathanielmay.quarto.quarto

import scala.util.{Try, Success, Failure}
import scalaz._
import Scalaz._


case class Quarto(board: Board, active: Option[Piece]){
  def takeTurn(piece: Piece, square: Square, forOpponent: Option[Piece]): Try[Quarto] = {
    if (!this.isValid) Failure(InvalidGameError())
    active match {
      case Some(p) if p != piece => Failure(BadTurnError(s"must place the active piece: $active actual piece placed: $piece"))
      case None if this != Quarto.newGame => Failure(BadTurnError(s"no active piece set for in progress game"))
    }
    if (board.squares.contains(square)) Failure(BadTurnError(s"square $square is already occupied"))
    if (board.squares.values.exists(_ == piece)) Failure(BadTurnError(s"piece $piece has already been placed")) //TODO should never reach here if active is checked for
    forOpponent match {
      case Some(p) if board.squares.values.exists(_ == p) => Failure(BadTurnError(s"active piece $active has already been placed"))
      case None if !Quarto.isWon(Quarto(Board(board.squares + (square -> piece)), None)) &&
        !Board(board.squares + (square -> piece)).isFull =>
        Failure(BadTurnError(s"no piece chosen for opponent and game still has more turns"))
    }

    Success(Quarto(board(board.squares + (square -> piece)), active))
  }

  def isValid: Boolean = {
    active match {
      case _ if !board.isValid => false
      case Some(p) if board.contains(p) => false
      case None if !Quarto.isWon(this) || !board.isFull => false
      case _ => true
    }
  }
}

case object Quarto{
  val newGame = Quarto(Board.newBoard, None)

  private val indexes = List(I0, I1, I2, I3)
  private val hLines = indexes.foldRight(List(): List[List[Square]])(
    (dex, squares) => squares ++ List(for{ i <- indexes } yield Square(dex, i))
  )
  private val vLines = indexes.foldRight(List(): List[List[Square]])(
    (dex, squares) => squares ++ List(for{ i <- indexes } yield Square(i, dex))
  )
  private val dLines: List[List[Square]] = List(for( (x, y) <- indexes zip indexes) yield Square(x,y)) ++
                                   List(for( (x, y) <- indexes zip indexes.reverse) yield Square(x,y))
  //TODO can add squares for variant
  val allLines: List[List[Square]] = hLines ++ vLines ++ dLines

  def takeTurns(q0: => Quarto)(turns: List[(Piece, Square, Option[Piece])]) : Try[Quarto] =
    turns.foldLeft(Try(q0)) { case (game, (piece, square, active)) =>
      game flatMap (_.takeTurn(piece, square, active))
    }

  def isWon(game: Quarto): Boolean = {
    if (winningLines(game).isEmpty) false else true
  }

  def winningLines(game: Quarto): List[List[Square]] = {
    allLines.filter(winningLine(game, _))
  }

  def winningLine(game: Quarto, line: List[Square]): Boolean = {
    val pieces = line flatMap {piece => game.board.squares get piece}
    val attrCounts = pieces.foldRight(Map(): Map[Attribute, Int])((piece, counts) =>
      counts |+| piece.attrs.foldRight(Map(): Map[Attribute, Int])((attr, m) => m |+| Map(attr -> 1)))
    if (4 <= attrCounts.maxBy(_._2)._2) true else false
  }

}

sealed case class Board(squares: Map[Square, Piece]){
  def contains(p: Piece): Boolean = squares.valuesIterator.contains(p)
  def isFull: Boolean = squares.size >= 16
  def isValid: Boolean = 1 <= squares.foldRight(Map[Piece, Int]())({
    case ((_, piece), map) => map |+| Map(piece -> 1)
  }).maxBy(_._2)._2
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

case class QuartoError() extends Exception
case class BadTurnError(msg: String) extends QuartoError
case class InvalidGameError() extends QuartoError