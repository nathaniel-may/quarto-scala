package com.nathanielmay.quarto.quarto

import com.nathanielmay.quarto.java.{Color, IAttribute, Line, Shape, Size, Top}
import scala.util.{Try,Success,Failure}
import scalaz._
import Scalaz._


final class Quarto(boardId:String,
                   squares:Map[(Int, Int), Piece] = Map(),
                   active:Option[Piece] = None,
                   pieces:Map[Piece, Boolean] = Map(),
                   lines:Map[(Line, IAttribute), Int] = Map()){

  validate()

  private def validate() {

    val validSquares = squares.foldLeft(true)((r:Boolean, c:((Int,Int), Piece)) =>
      if(r)  Quarto.isValidSquare(c._1) && squares.values.count(_ == c._2) == 1 else false
    )

    if(!validSquares)throw new InvalidBoardError

    active match {
      case Some(p) => if(squares.exists(_._2 == p)) throw new InvalidBoardError
      case _ => if(squares.nonEmpty && !isWon) throw new InvalidBoardError
    }

  }

  def takeTurn(toPlace:Piece, square:(Int, Int), active:Option[Piece]): Quarto = {

    if(!Quarto.isValidSquare(square)) throw new SquareDoesNotExistError
    if(squares.getOrElse(square, "not occupied") != "not occupied") throw new BadTurnError

    val winningMove = willWin(toPlace, square)

    if(Quarto.samePiece(toPlace, active) && !winningMove) throw new BadTurnError
    if(activeIsPlaced(active) && !winningMove) throw new BadTurnError

    var newActive = active
    if(winningMove) newActive = None

    Try(new Quarto(boardId,
      squares + (square -> toPlace),
      newActive,
      pieces + (toPlace -> true),
      Quarto.updateLines(lines, square, toPlace))) match {
      case Success(game) => game
      case Failure(_) => throw new BadTurnError
    }


  }

  protected def activeIsPlaced(active:Option[Piece]): Boolean ={
    active match {
      case Some(p) => pieces.get(p) match {
        case Some(b) => b
        case _ => false
      }
      case _ => false
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

    squares.foldLeft(Map[(Line, IAttribute), Int]())(
      (lines, c) => c match {
        case (square, piece) => updateLines(lines, square, piece)
      }
    )

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