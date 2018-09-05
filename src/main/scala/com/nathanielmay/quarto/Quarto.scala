package com.nathanielmay.quarto

import com.nathanielmay.quarto
import com.nathanielmay.quarto.Exceptions.{CannotPlacePieceOnFirstTurnError, GameOverError, InvalidPieceForOpponent, InvalidPieceForOpponentError, InvalidPlacementError, MalformedTurnError, MustPlacePieceError, OutOfTurnError}

import scala.util.{Failure, Success, Try}

case object Quarto{
  def apply(): Quarto = new Quarto(Board(), None)
  def apply(board: Board, forOpponent: Option[Piece]): Try[Quarto] =
    if (Quarto.validPieceForOpponent(board, forOpponent))
      Success(new Quarto(board, forOpponent))
    else
      Failure(InvalidPieceForOpponentError)

  private val hLines  = Board.indexes.map(h => Board.indexes.map(v => Tile(h, v)))
  private val vLines  = Board.indexes.map(v => Board.indexes.map(h => Tile(h, v)))
  private val dLines  = List(Board.indexes.zip(Board.indexes).map({case (h, v) => quarto.Tile(h, v)}),
                             Board.indexes.zip(Board.indexes.reverse).map({case (h, v) => quarto.Tile(h, v)}))
  //TODO add squares variant
  val allLines: List[List[Tile]] = hLines ++ vLines ++ dLines

  def isWon(board: Board): Boolean =
    allLines.exists(winningLine(board, _))

  def willWin(game: Quarto, piece: Piece, tile: Tile): Boolean =
    Board(game.board.tiles + (tile -> piece)).map(Quarto.isWon).fold(_ => false, identity)

  private def winningLine(board: Board, line: List[Tile]): Boolean = {
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
  val isFirstTurn: Boolean = board.isEmpty && active.isEmpty
  val isLastTurn:  Boolean = board.size == 15
  val isComplete:  Boolean = Quarto.isWon(board) || board.isFull
  val player:      Player  = if (board.isEmpty && active.isEmpty) P1
                             else if (board.size % 2 == 0 && active.isDefined) P2
                             else P1

  def takeTurn(p: Player, piece: Piece, tile: Tile, forOpponent: Option[Piece]): Try[Quarto] = {
    val tryNextBoard     = Board(board.tiles + (tile -> piece))
    val willWin          = Quarto.willWin(this, piece, tile)
    val finalTurn        = isLastTurn || willWin
    val validPiece       = active.fold(false)(p =>
      p == piece && !board.contains(piece) && !forOpponent.contains(piece))
    val validForOpponent = forOpponent.fold(isLastTurn || willWin)(p =>
      (!board.contains(p) && p != piece) || finalTurn)

    if(isFirstTurn)
      Failure(CannotPlacePieceOnFirstTurnError)
    else if (p != player)
      Failure(OutOfTurnError)
    else if (board.contains(tile))
      Failure(InvalidPlacementError)
    else if (isComplete)
      Failure(GameOverError)
    else if (!validPiece)
      Failure(MalformedTurnError)
    else if (!validForOpponent)
      Failure(InvalidPieceForOpponent)
    else if (willWin)
      tryNextBoard.fold[Try[Quarto]](f => Failure(f), board => Success(new Quarto(board, None)))
    else
      tryNextBoard.fold[Try[Quarto]](f => Failure(f), board => Success(new Quarto(board, forOpponent)))
  }

  def takeFirstTurn(player: Player, forOpponent: Piece): Try[Quarto] =
    if(!isFirstTurn)
      Failure(MustPlacePieceError)
    else if (this.player != P1 || player != P1)
      Failure(OutOfTurnError)
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