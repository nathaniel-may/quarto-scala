package com.nathanielmay.quarto.quarto

import com.nathanielmay.quarto.java.{Color, IAttribute, Line, Shape, Size, Top}
import scala.util.Try
import scalaz._
import Scalaz._

final class Quarto private (squares:Map[(Int, Int), Piece] = Map(),
                            val active:Option[Piece] = None,
                            pieces:Set[Piece] = Set(),
                            lines:Map[(Line, IAttribute), Int] = Map()){

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
               lines:Map[(Line, IAttribute), Int] = Map()): Try[Quarto] =
  {
    val q = new Quarto(squares, active, pieces, lines)
    Try(if (q.validate) q else throw new InvalidBoardError)
  }

  def takeTurns(q0: => Quarto)(turns: List[(Piece, (Int,Int), Option[Piece])]) : Try[Quarto] =
    turns.foldLeft(Try(q0)) { case (game, (piece, square, active)) =>
      game flatMap (_.takeTurn(piece, square, active)) }

  private def winningLines(lines:Map[(Line, IAttribute), Int]): Boolean =
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

  protected def linesFromSquares(squares:Map[(Int, Int), Piece]): Map[(Line, IAttribute), Int] = {

    squares.foldLeft(Map[(Line, IAttribute), Int]()) {
      case (lines, (square, piece)) => updateLines(lines, square, piece)
    }

  }

  protected def updateLines(lines:Map[(Line, IAttribute), Int], square:(Int, Int), piece:Piece): Map[(Line, IAttribute), Int] = {
    lines |+| linesFromSquare(square, piece)
  }

  protected def linesFromSquare(square:(Int, Int), piece:Piece): Map[(Line, IAttribute), Int] ={
    var lines = Map[(Line, IAttribute), Int]()

    if(square._1 == 0){ lines = lines |+| linePairs(Line.H0, piece) }
    if(square._1 == 1){ lines = lines |+| linePairs(Line.H1, piece) }
    if(square._1 == 2){ lines = lines |+| linePairs(Line.H2, piece) }
    if(square._1 == 3){ lines = lines |+| linePairs(Line.H3, piece) }

    if(square._2 == 0){ lines = lines |+| linePairs(Line.V0, piece) }
    if(square._2 == 1){ lines = lines |+| linePairs(Line.V1, piece) }
    if(square._2 == 2){ lines = lines |+| linePairs(Line.V2, piece) }
    if(square._2 == 3){ lines = lines |+| linePairs(Line.V3, piece) }

    if(square._1 == square._2){ lines = lines |+| linePairs(Line.D0, piece) }
    if(square._1 == 0 && square._2 == 3){ lines = lines |+| linePairs(Line.D1, piece) }
    if(square._1 == 1 && square._2 == 2){ lines = lines |+| linePairs(Line.D1, piece) }
    if(square._1 == 2 && square._2 == 1){ lines = lines |+| linePairs(Line.D1, piece) }
    if(square._1 == 3 && square._2 == 0){ lines = lines |+| linePairs(Line.D1, piece) }

    lines

  }

  private def linePairs(line:Line, piece:Piece): Map[(Line, IAttribute), Int] = {

    val lines: Map[(Line, IAttribute), Int] = Map((line, piece.color) -> 1)
    lines + ((line, piece.size) -> 1, (line, piece.shape) -> 1, (line, piece.top) -> 1)

  }

}

final class Piece(val color: Color, val size: Size, val shape: Shape, val top: Top) {

  private def colorChar: String =
    color match {
      case Color.WHITE => "W"
      case Color.BLACK => "B"
    }

  private def sizeChar: String =
    size match {
      case Size.LARGE => "L"
      case Size.SMALL => "S"
    }

  private def shapeChar: String =
    shape match {
      case Shape.SQUARE => "Q"
      case Shape.ROUND => "R"
    }

  private def topChar: String =
    top match {
      case Top.FLAT => "F"
      case Top.HOLE => "H"
    }

  override def toString: String = colorChar + sizeChar + shapeChar + topChar

  override def equals(that: Any): Boolean =
    that match {
      case that: Piece => this.hashCode() == that.hashCode()
      case _ => false
    }

  override def hashCode: Int = toString().hashCode()


}

class QuartoError extends Exception {}

class SquareDoesNotExistError extends QuartoError {}

class BadTurnError extends QuartoError {}

class InvalidBoardError extends QuartoError {}