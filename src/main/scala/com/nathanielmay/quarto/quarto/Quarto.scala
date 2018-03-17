package com.nathanielmay.quarto.quarto

import com.nathanielmay.quarto.java.{Attribute, Line}
import com.nathanielmay.quarto.java.Attribute.{Color, Shape, Size, Top}
import scalaz._
import Scalaz._


final class Quarto(boardId:String,
                   squares:Map[(Int, Int), Piece] = Map(),
                   active:Option[Piece] = None,
                   pieces:Map[Piece, Boolean] = Map(),
                   lines:Map[(Line, Attribute), Int] = Map()){

  def takeTurn(toPlace:Piece, square:(Int, Int), active:Piece): Quarto = {

    square match {
      case (h:Int, v:Int) =>
        if(h < 0 || h > 3 || v < 0 || v > 3){
          throw new SquareDoesNotExistError
        }
    }

    if(toPlace == active) {
      throw new BadTurnError
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

}

object Quarto {

  final def isValid(game:Quarto): Boolean = {

    //TODO STUB
    false
  }

  def isWon(game:Quarto): Boolean = {

    //TODO STUB
    false
  }

  protected def piecesFromSquares(squares:Map[(Int, Int), Piece]): Map[Piece, Boolean] = {
    squares.map { keyValue:((Int,Int), Piece) =>
      keyValue match { case (_, value) => (value, true) }
    }
  }

  protected def linesFromSquares(squares:Map[(Int, Int), Piece]): Map[(Line, Attribute), Int] = {
    var lines = Map[(Line, Attribute), Int]()
    squares.map {
      keyValue:((Int,Int), Piece) => keyValue match {
        case (square, piece) =>
          lines = updateLines(lines, square, piece)
      }
    }

    lines
  }

  protected def updateLines(lines:Map[(Line, Attribute), Int], square:(Int, Int), piece:Piece): Map[(Line, Attribute), Int] = {
    lines |+| linesFromSquare(square, piece)
  }

  protected def linesFromSquare(square:(Int, Int), piece:Piece): Map[(Line, Attribute), Int] ={
    val lines = Map[(Line, Attribute), Int]()

    if(square._1 == 0){ lines(linePairs(Line.H0, piece)) = 1 }
    if(square._1 == 1){ lines(linePairs(Line.H1, piece)) = 1 }
    if(square._1 == 2){ lines(linePairs(Line.H2, piece)) = 1 }
    if(square._1 == 3){ lines(linePairs(Line.H3, piece)) = 1 }

    if(square._2 == 0){ lines(linePairs(Line.V0, piece)) = 1 }
    if(square._2 == 1){ lines(linePairs(Line.V1, piece)) = 1 }
    if(square._2 == 2){ lines(linePairs(Line.V2, piece)) = 1 }
    if(square._2 == 3){ lines(linePairs(Line.V3, piece)) = 1 }

    if(square._1 == square._2){ lines(linePairs(Line.D0, piece)) = 1 }
    if(square._1 == 0 && square._2 == 3){ lines(linePairs(Line.D1, piece)) = 1 }
    if(square._1 == 1 && square._2 == 2){ lines(linePairs(Line.D1, piece)) = 1 }
    if(square._1 == 2 && square._2 == 1){ lines(linePairs(Line.D1, piece)) = 1 }
    if(square._1 == 4 && square._2 == 0){ lines(linePairs(Line.D1, piece)) = 1 }

    lines

  }

  private def linePairs(line:Line, piece:Piece): Set[(Line, Attribute)] = {

    val lines = Set[(Line, Attribute)]()
    lines ++ Set((line, piece.color))
    lines ++ Set((line, piece.size))
    lines ++ Set((line, piece.shape))
    lines ++ Set((line, piece.top))

    lines
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

  override def toString: String = {
    colorChar + sizeChar + shapeChar + topChar
  }

  def canEqual(a: Any) = a.isInstanceOf[Piece]

  override def equals(that: Any): Boolean =
    that match {
      case that: Piece => that.canEqual(this) && this.hashCode == that.hashCode
      case _ => false
    }

  override def hashCode: Int = {
    toString().hashCode()
  }

}

class QuartoError extends Exception {}

class SquareDoesNotExistError extends QuartoError {}

class BadTurnError extends QuartoError {}