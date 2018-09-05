package testingUtil

import com.nathanielmay.quarto.{Board, Piece, Quarto, Player, Tile}
import scala.util.{Failure, Success, Try}

object Util {

  def failedQuarto(board: Try[Board], forOpponent: Option[Piece]): Boolean =
    quarto(board, forOpponent).isFailure

  def successQuarto(board: Try[Board], forOpponent: Option[Piece]): Boolean =
    quarto(board, forOpponent).isSuccess

  def quarto(board: Try[Board], forOpponent: Option[Piece]): Try[Quarto] =
    board.flatMap(b => Quarto(b,forOpponent))

  def expectError(e: Exception)(firstTurnPlayer: Player, firstTurnForOpponent: Piece)(turns: List[(Player, Piece, Tile, Option[Piece])]): Boolean =
    takeTurns(firstTurnPlayer, firstTurnForOpponent)(turns).failed.get == e

  def assertWin(firstTurnPlayer: Player, firstTurnForOpponent: Piece)(turns: List[(Player, Piece, Tile, Option[Piece])]): Unit = {
    assert(turnsWon(firstTurnPlayer, firstTurnForOpponent)(turns))
  }

  def assertNoWin(firstTurnPlayer: Player, firstTurnForOpponent: Piece)(turns: List[(Player, Piece, Tile, Option[Piece])]): Unit = {
    assert(!turnsWon(firstTurnPlayer, firstTurnForOpponent)(turns))
  }

  def turnsWon(firstTurnPlayer: Player, firstTurnForOpponent: Piece)(turns: List[(Player, Piece, Tile, Option[Piece])]): Boolean =
    takeTurns(firstTurnPlayer, firstTurnForOpponent)(turns).fold(_ => false, game => Quarto.isWon(game.board))

  def takeTurns(q0: Quarto)(turns: List[(Player, Piece, Tile, Option[Piece])]): Try[Quarto] = {
    if (q0 == Quarto())
      Failure(new Exception("no piece is placed on the first turn"))
    else
      turns.foldLeft[Try[Quarto]](Success(q0))({case (tryGame, (player, piece, tile, forOpponent)) =>
        tryGame.flatMap(game => game.takeTurn(player, piece, tile, forOpponent))
    })
  }

  def takeTurns(firstTurnPlayer: Player, firstTurnForOpponent: Piece)(turns: List[(Player, Piece, Tile, Option[Piece])]): Try[Quarto] = {
    Quarto().takeFirstTurn(firstTurnPlayer, firstTurnForOpponent)
      .fold(Failure(_), game => takeTurns(game)(turns))
  }
}
