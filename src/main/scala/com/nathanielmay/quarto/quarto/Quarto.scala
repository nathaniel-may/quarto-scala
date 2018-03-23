package com.nathanielmay.quarto.quarto

import scala.util.Try
import scalaz._
import Scalaz._

final class Quarto private (squares:Map[(Int, Int), Piece] = Map(),
                            val active:Option[Piece] = None,
                            pieces:Set[Piece] = Set(),
                            lines:Map[(Line, Attribute), Int] = Map()){

  private def validate: Boolean = {
    val validSquares = squares forall { case (ix, p) =>
      Quarto.isValidSquare(ix) && squares.values.count(_ == p) == 1
    }

    val validNextPiece = active match {
      case Some(p) => !squares.exists(_._2 == p)
      case None    => squares.isEmpty || isWon
    }

    validSquares && validNextPiece
  }

  def takeTurn(toPlace:Piece, square:(Int, Int), active:Option[Piece]): Try[Quarto] = Try {
    if(!validTurn(toPlace, square, active)) throw new BadTurnError

    val newActive = if(willWin(toPlace, square)) None else active

    new Quarto(
      squares + (square -> toPlace),
      newActive,
      pieces + toPlace,
      Quarto.updateLines(lines, square, toPlace)
    )
  }

  private def validTurn(toPlace:Piece, square:(Int, Int), active:Option[Piece]): Boolean = {
    val winningMove = willWin(toPlace, square)

    Quarto.isValidSquare(square) &&
    !squares.contains(square) &&
    (!Quarto.samePiece(toPlace, active) || (Quarto.samePiece(toPlace, active) && winningMove)) &&
    (!activeIsPlaced(active) || (activeIsPlaced(active) && winningMove))
  }

  protected def activeIsPlaced(active:Option[Piece]): Boolean = {
    active match {
      case Some(piece) => pieces.contains(piece)
      case None        => false
    }
  }

  def willWin(toPlace:Piece, square:(Int, Int)): Boolean = {
    val newLines = lines |+| Quarto.linesFromSquare(square, toPlace)
    Quarto.winningLines(newLines)
  }

  def isWon: Boolean = Quarto.winningLines(this.lines)

  def getActive: Option[Piece] = active

  override def toString: String = squares.toString()

  override def equals(that: Any): Boolean =
    that match {
      case that:Quarto => this.hashCode == that.hashCode
      case _           => false
    }

  override def hashCode: Int = toString().hashCode()

}

object Quarto {

  val emptyBoard: Quarto = new Quarto(Map(), None, Set(), Map())

  def board(squares:Map[(Int, Int), Piece],
               active:Option[Piece],
               pieces:Set[Piece] = Set(),
               lines:Map[(Line, Attribute), Int] = Map()): Try[Quarto] =
  {
    val q = new Quarto(squares, active, pieces, lines)
    Try(if (q.validate) q else throw new InvalidBoardError)
  }

  def takeTurns(q0: => Quarto)(turns: List[(Piece, (Int,Int), Option[Piece])]) : Try[Quarto] =
    turns.foldLeft(Try(q0)) { case (game, (piece, square, active)) =>
      game flatMap (_.takeTurn(piece, square, active)) }

  private def winningLines(lines:Map[(Line, Attribute), Int]): Boolean =
    4 <= lines.foldLeft(0)(_ max _._2)

  protected def samePiece(a:Piece, b:Option[Piece]): Boolean = {
    b match {
      case Some(p) => a == p
      case _ => false
    }
  }

  protected def isValidSquare(square:(Int, Int)): Boolean = {
    square match {
      case (h:Int, v:Int) =>
        if(h < 0 || h > 3 || v < 0 || v > 3) false else true
    }
  }

  protected def piecesFromSquares(squares:Map[(Int, Int), Piece]): Map[Piece, Boolean] = {
    squares.map { keyValue:((Int,Int), Piece) =>
      keyValue match { case (_, value) => (value, true) }
    }
  }

  protected def linesFromSquares(squares:Map[(Int, Int), Piece]): Map[(Line, Attribute), Int] = {
    squares.foldLeft(Map[(Line, Attribute), Int]()) {
      case (lines, (square, piece)) => updateLines(lines, square, piece)
    }
  }

  protected def updateLines(lines:Map[(Line, Attribute), Int], square:(Int, Int), piece:Piece): Map[(Line, Attribute), Int] = {
    lines |+| linesFromSquare(square, piece)
  }

  protected def linesFromSquare(square:(Int, Int), piece:Piece): Map[(Line, Attribute), Int] ={
    square match {
      case (h,v) =>
        linePairs(Line.H(h), piece) ++
        linePairs(Line.V(v), piece) ++
        (if      (h == v)     linePairs(Line.D0, piece)
         else if (h + v == 3) linePairs(Line.D1, piece)
         else                 Map())
    }

    val dlines = square match {
      case (h, v) if h == v => linePairs(Line.D0, piece)
      case (h, v) if h + v == 3 => linePairs(Line.D1, piece)
      case _ => Map()
    }

    dlines ++ linePairs(Horizontal(Index(square._1)), piece) ++ linePairs(Line.V(square._2), piece)
  }

  private def linePairs(line:Line, piece:Piece): Map[(Line, Attribute), Int] = {
    Map((line, piece.color) -> 1,
        (line, piece.size)  -> 1,
        (line, piece.shape) -> 1,
        (line, piece.top)   -> 1
    )
  }

}

sealed abstract class Index(val index: Int)
case object I0 extends Index(0)
case object I1 extends Index(1)
case object I2 extends Index(2)
case object I3 extends Index(3)

sealed trait Angle
case object Forward  extends Angle { override def toString:String = "D1" }
case object Backward extends Angle { override def toString:String = "D0" }

sealed trait Line
case class Horizontal(i:Index)   extends Line { override def toString:String = "H" + i }
case class Vertical(i:Index)     extends Line { override def toString:String = "V" + i }
case class Diagonal(angle:Angle) extends Line { override def toString:String = "D" + angle }

sealed trait Line {
  def direction:String
  def index:Index
}

final case class Piece(color: Color, size: Size, shape: Shape, top: Top) {
  override def toString: String = "" + color + size + shape + top
}

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

class QuartoError extends Exception {}

class BadTurnError extends QuartoError {}

class InvalidBoardError extends QuartoError {}