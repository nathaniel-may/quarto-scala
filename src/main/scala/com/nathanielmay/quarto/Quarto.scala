package com.nathanielmay.quarto

import com.nathanielmay.quarto.board.{Board, Square}
import com.nathanielmay.quarto.piece.{Attribute, Piece}
import com.nathanielmay.quarto.Exceptions.{InvalidPieceForOpponentError,
                                           OutOfTurnError,
                                           InvalidPlacementError,
                                           GameOverError,
                                           InvalidPieceError,
                                           InvalidPieceForOpponent,
                                           CannotPlacePieceOnFirstTurnError,
                                           MustPlacePieceError}
import scala.util.{Try, Success, Failure}

case object Quarto{
  def apply(): Quarto = new Quarto(Board(), None)
  def apply(board: Board, forOpponent: Option[Piece]): Try[Quarto] =
    if (Quarto.validPieceForOpponent(board, forOpponent))
      Success(new Quarto(board, forOpponent))
    else
      Failure(InvalidPieceForOpponentError)

  private val hLines  = Board.indexes.map(h => Board.indexes.map(v => Square(h, v)))
  private val vLines  = Board.indexes.map(v => Board.indexes.map(h => Square(h, v)))
  private val dLines  = List(Board.indexes.zip(Board.indexes).map({case (h, v) => board.Square(h, v)}),
                             Board.indexes.zip(Board.indexes.reverse).map({case (h, v) => board.Square(h, v)}))
  //TODO add squares variant
  val allLines: List[List[Square]] = hLines ++ vLines ++ dLines

  def isWon(board: Board): Boolean =
    allLines.exists(winningLine(board, _))

  private def winningLine(board: Board, line: List[Square]): Boolean = {
    line.flatMap(piece => board.get(piece))
      .foldLeft[Map[Attribute, Int]](Map())((counts, piece) =>
        piece.attrs.foldLeft(counts)((m, attr) =>
          m.updated(attr, m.getOrElse(attr, 0) + 1)))
      .exists(4 <= _._2)
  }

  private def validPieceForOpponent(board: Board, forOpponent: Option[Piece]): Boolean = {
    forOpponent.fold(Quarto.isWon(board) || board.isFull)(p => !board.contains(p) || Quarto.isWon(board) || board.isEmpty)
  }

}

sealed case class Quarto private (board: Board, active: Option[Piece]){
  def isFirstTurn: Boolean = board.isEmpty && active.isEmpty
  def isLastTurn:  Boolean = board.size == 15
  def isComplete:  Boolean = Quarto.isWon(board) || board.isFull
  def player:      Player  = if (board.isEmpty && active.isEmpty) P1
                             else if (board.size % 2 == 0 && active.isDefined) P2
                             else P1

  def takeTurn(p: Player, piece: Piece, square: Square, forOpponent: Option[Piece]): Try[Quarto] = {
    val tryNextBoard     = Board(board.squares + (square -> piece))
    val willWin          = tryNextBoard.map(Quarto.isWon).fold(_ => false, identity)
    val finalTurn        = isLastTurn || willWin
    val validPiece       = active.fold(isFirstTurn)(p =>
      p == piece && !board.contains(piece) && !forOpponent.contains(piece))
    val validForOpponent = forOpponent.fold(isLastTurn || willWin)(p =>
      (!board.contains(p) && p != piece) || finalTurn)

    if(isFirstTurn)
      Failure(CannotPlacePieceOnFirstTurnError)
    if (p != player)
      Failure(OutOfTurnError)
    else if (board.contains(square))
      Failure(InvalidPlacementError)
    else if (isComplete)
      Failure(GameOverError)
    else if (!validPiece)
      Failure(InvalidPieceError)
    else if (!validForOpponent)
      Failure(InvalidPieceForOpponent)
    else if (willWin)
      tryNextBoard.fold[Try[Quarto]](f => Failure(f), board => Success(new Quarto(board, None)))
    else
      tryNextBoard.fold[Try[Quarto]](f => Failure(f), board => Success(new Quarto(board, forOpponent)))
  }

  def takeFirstTurn(player: Player, forOpponent: Piece): Try[Quarto] =
    if (player != P1)
      Failure(OutOfTurnError)
    else if(board != Board())
      Failure(MustPlacePieceError)
    else
      Quarto(Board(), Some(forOpponent))

  override def toString: String =
    if(isFirstTurn)
      s"$player needs to hand a piece to opponent\n$board"
    else {
      val prettyActive = active match {
        case None => "nothing"
        case Some(p) => p.toString
      }
      s"$player needs to place $prettyActive on\n$board"
    }
}

sealed abstract class Player(val num: Int)
case object P1 extends Player(1)
case object P2 extends Player(2)