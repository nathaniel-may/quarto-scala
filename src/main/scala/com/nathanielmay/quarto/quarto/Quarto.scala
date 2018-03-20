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

    //TODO validate function returns boolean??
    if(!Quarto.isValidSquare(square)) throw new SquareDoesNotExistError
    if(squares.contains(square)) throw new BadTurnError

    val winningMove = willWin(toPlace, square)

    if(Quarto.samePiece(toPlace, active) && !winningMove) throw new BadTurnError
    if(activeIsPlaced(active) && !winningMove) throw new BadTurnError

    val newActive = if(winningMove) None else active

    new Quarto(
      squares + (square -> toPlace),
      newActive,
      pieces + toPlace,
      Quarto.updateLines(lines, square, toPlace)
    )
  }

  protected def activeIsPlaced(active:Option[Piece]): Boolean = {
    //active.fold(false)(p => pieces.getOrElse(p, false))
    active match {
      case Some(piece) => pieces.contains(piece)
      case None    => false
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
      case _ => false
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
    val dlines = square match {
      case (h, v) if h == v => linePairs(Line.D0, piece)
      case (h, v) if h + v == 3 => linePairs(Line.D1, piece)
      case _ => Map[(Line, Attribute), Int]()
    }

    dlines ++ linePairs(Line.H(square._1), piece) ++ linePairs(Line.V(square._2), piece)
  }

  private def linePairs(line:Line, piece:Piece): Map[(Line, Attribute), Int] = {
    Map((line, piece.color) -> 1,
        (line, piece.size)  -> 1,
        (line, piece.shape) -> 1,
        (line, piece.top)   -> 1
    )
  }

}

final case class Line private (direction:String, num:Int) {
  override def toString: String = direction + num
}
object Line {
  //TODO control nums
  def H(num:Int):Line = new Line("H", num)
  def V(num:Int):Line = new Line("V", num)
  def D0:Line = new Line("D", 0)
  def D1:Line = new Line("D", 1)
}

final case class Piece(color: Color, size: Size, shape: Shape, top: Top) {
  override def toString: String = "" + color + size + shape + top
}

trait Attribute {}

final case class Color private (char:String) extends Attribute {
  override def toString:String = char
}
object Color {
  def BLACK:Color = new Color("B")
  def WHITE:Color = new Color("W")
}

final case class Size private (char:String) extends Attribute {
  override def toString:String = char
}
object Size {
  def LARGE:Size = new Size("L")
  def SMALL:Size = new Size("S")
}

final case class Shape private (char:String) extends Attribute {
  override def toString:String = char
}
object Shape {
  def ROUND:Shape = new Shape("R")
  def SQUARE:Shape = new Shape("Q")
}

final case class Top private (char:String) extends Attribute {
  override def toString:String = char
}
object Top {
  def FLAT:Top = new Top("F")
  def HOLE:Top = new Top("H")
}

class QuartoError extends Exception {}

class SquareDoesNotExistError extends QuartoError {}

class BadTurnError extends QuartoError {}

class InvalidBoardError extends QuartoError {}