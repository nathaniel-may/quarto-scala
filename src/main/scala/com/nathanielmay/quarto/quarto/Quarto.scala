package com.nathanielmay.quarto.quarto

import com.nathanielmay.quarto.java.{IAttribute, Color, Shape, Size, Top, Line}
import scalaz._
import Scalaz._


final class Quarto(boardId:String,
                   squares:Map[(Int, Int), Piece] = Map(),
                   active:Option[Piece] = None,
                   pieces:Map[Piece, Boolean] = Map(),
                   lines:Map[(Line, IAttribute), Int] = Map()){

  def takeTurn(toPlace:Piece, square:(Int, Int), active:Option[Piece]): Quarto = {

    square match {
      case (h:Int, v:Int) =>
        if(h < 0 || h > 3 || v < 0 || v > 3){
          throw new SquareDoesNotExistError
        }
    }

    match active {
      case active:Piece => if(toPlace == active) { throw new BadTurnError }
      case _ =>
    }

    if(pieces.getOrElse(active, "unused active") != "unused active") {
      throw new BadTurnError
    }

    if(squares.getOrElse(square, "square occupied") != "square occupied") {
      throw new BadTurnError
    }

    new Quarto(boardId,
      squares + (square -> toPlace),
      Some(active),
      pieces + (toPlace -> true),
      Quarto.updateLines(lines, square, toPlace))
  }

  def isWon(): Boolean = 4 <= this.lines.foldLeft(0)(_ max _._2)

  override def toString(): String = squares.toString()

  override def equals(that: Any): Boolean =
    that match {
      case that:Quarto => this.hashCode == that.hashCode
      case _ => false
    }

  override def hashCode(): Int = toString().hashCode()

}

object Quarto {

  final def isValid(game:Quarto): Boolean = {

    //TODO STUB
    false
  }

  protected def piecesFromSquares(squares:Map[(Int, Int), Piece]): Map[Piece, Boolean] = {
    squares.map { keyValue:((Int,Int), Piece) =>
      keyValue match { case (_, value) => (value, true) }
    }
  }

  protected def linesFromSquares(squares:Map[(Int, Int), Piece]): Map[(Line, IAttribute), Int] = {
    var lines = Map[(Line, IAttribute), Int]()
    squares.map {
      keyValue:((Int,Int), Piece) => keyValue match {
        case (square, piece) =>
          lines = updateLines(lines, square, piece)
      }
    }

    lines
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
    if(square._1 == 4 && square._2 == 0){ lines = lines |+| linePairs(Line.D1, piece) }

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

  override def toString(): String = colorChar + sizeChar + shapeChar + topChar

  override def equals(that: Any): Boolean =
    that match {
      case that: Piece => this.hashCode() == that.hashCode()
      case _ => false
    }

  override def hashCode(): Int = toString().hashCode()


}

class QuartoError extends Exception {}

class SquareDoesNotExistError extends QuartoError {}

class BadTurnError extends QuartoError {}