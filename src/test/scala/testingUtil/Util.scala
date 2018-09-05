package testingUtil

import com.nathanielmay.quarto.{Board, Piece, Player, Quarto, Tile, Winner}

import scala.util.{Failure, Success, Try}

object Util {

  def failedQuarto(board: Try[Board], forOpponent: Option[Piece]): Boolean =
    quarto(board, forOpponent).isFailure

  def successQuarto(board: Try[Board], forOpponent: Option[Piece]): Boolean =
    quarto(board, forOpponent).isSuccess

  def quarto(board: Try[Board], forOpponent: Option[Piece]): Try[Quarto] =
    board.flatMap(b => Quarto(b,forOpponent))

  def expectError(e: Exception)(turns: List[Turn]): Boolean =
    takeTurns(Quarto())(turns).failed.get == e

  def assertWin(turns: List[Turn]): Unit = {
    assert(turnsWon(turns))
  }

  def assertNoWin(turns: List[Turn]): Unit = {
    assert(!turnsWon(turns))
  }

  private def turnsWon(turns: List[Turn]): Boolean =
    takeTurns(Quarto())(turns).fold(_ => false, game => Quarto.isWon(game.board))

  def takeTurns(q0: Quarto)(turns: List[Turn]): Either[Try[Quarto], Winner] =
    turns.foldLeft[Either[Try[Quarto], Winner]](Left(Success(q0)))({ case (e, turn) =>
      turn match {
        case Pass(person, piece) =>
          e.fold[Either[Try[Quarto], Winner]](
            _.fold[Either[Try[Quarto], Winner]](
              f => Left(Failure(f)),
              game => Left(game.passPiece(person, piece))),
            winner => Right(winner))
        case Place(person, piece, tile) =>
          e.fold[Either[Try[Quarto], Winner]](
            _.fold[Either[Try[Quarto], Winner]](
              f => Left(Failure(f)),
              game => game.placePiece(person, piece, tile)),
            winner => Right(winner))
      }
    })
}

sealed abstract class Turn
case class Pass(person: Player, forOpponent: Piece) extends Turn
case class Place(person: Player, piece: Piece, tile: Tile) extends Turn