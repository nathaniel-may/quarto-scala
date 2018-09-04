package testingUtil

import com.nathanielmay.quarto.board.Square
import com.nathanielmay.quarto.board.Board
import com.nathanielmay.quarto.piece.Piece
import com.nathanielmay.quarto.{Player, Quarto, Turn}
import scala.util.{Try, Success}

object Util {

  def failedQuarto(board: Try[Board], forOpponent: Option[Piece]): Boolean =
    quarto(board, forOpponent).isFailure

  def successQuarto(board: Try[Board], forOpponent: Option[Piece]): Boolean =
    quarto(board, forOpponent).isSuccess

  def quarto(board: Try[Board], forOpponent: Option[Piece]): Try[Quarto] =
    board.flatMap(b => Quarto(b,forOpponent))

  def assertWin(turns: List[(Player, Piece, Square, Option[Piece])]): Unit = {
    assert(turnsWon(turns))
  }

  def assertNoWin(turns: List[(Player, Piece, Square, Option[Piece])]): Unit = {
    assert(!turnsWon(turns))
  }

  def turnsWon(turns: List[(Player, Piece, Square, Option[Piece])]): Boolean =
    takeTurns(Quarto())(turns).fold(_ => false, game => Quarto.isWon(game.board))

  def takeTurns(q0: Quarto)(turns: List[(Player, Piece, Square, Option[Piece])]): Try[Quarto] = {
    turns.foldLeft[Try[Quarto]](Success(q0))({case (tryGame, (player, piece, square, forOpponent)) =>
      tryGame.flatMap(game => Turn(game, player, piece, square, forOpponent))
            .flatMap(Quarto.takeTurn)
    })
  }
}
