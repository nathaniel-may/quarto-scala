package com.nathanielmay.quarto.quarto

//import com.nathanielmay.quarto.java.{Color, IAttribute, Line, Shape, Size, Top}
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
    var lines = Map[(Line, Attribute), Int]()

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

  private def linePairs(line:Line, piece:Piece): Map[(Line, Attribute), Int] = {
    val lines: Map[(Line, Attribute), Int] = Map((line, piece.color) -> 1)
    lines + ((line, piece.size) -> 1, (line, piece.shape) -> 1, (line, piece.top) -> 1)
  }

}

final case class Line private (direction:String, num:Int) {
  override def toString: String = direction + num
}
object Line {
  def H0:Line = new Line("H", 0)
  def H1:Line = new Line("H", 1)
  def H2:Line = new Line("H", 2)
  def H3:Line = new Line("H", 3)

  def V0:Line = new Line("V", 0)
  def V1:Line = new Line("V", 1)
  def V2:Line = new Line("V", 2)
  def V3:Line = new Line("V", 3)

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