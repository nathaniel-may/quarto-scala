package testingUtil

import com.nathanielmay.quarto.board.Square
import com.nathanielmay.quarto.board.Board
import com.nathanielmay.quarto.piece.Piece
import com.nathanielmay.quarto.{Player, Quarto, Turn}

object Util {

  def emptyQuarto(board: Option[Board], forOpponent: Option[Piece]): Boolean =
    quarto(board, forOpponent).isEmpty

  def definedQuarto(board: Option[Board], forOpponent: Option[Piece]): Boolean =
    quarto(board, forOpponent).isDefined

  def quarto(board: Option[Board], forOpponent: Option[Piece]): Option[Quarto] =
    board.map(Quarto.apply).flatMap[Quarto](f => f(forOpponent))

  def assertWin(turns: List[(Player, Piece, Square, Option[Piece])]): Unit = {
    assert(turnsWon(turns))
  }

  def assertNoWin(turns: List[(Player, Piece, Square, Option[Piece])]): Unit = {
    assert(!turnsWon(turns))
  }

  def turnsWon(turns: List[(Player, Piece, Square, Option[Piece])]): Boolean =
    takeTurns(Quarto())(turns).fold(false)(game => Quarto.isWon(game.board))

  def takeTurns(q0: Quarto)(turns: List[(Player, Piece, Square, Option[Piece])]): Option[Quarto] = {
    turns.foldLeft[Option[Quarto]](Some(q0))({case (opGame, (player, piece, square, forOpponent)) =>
      opGame.flatMap(game => Turn(game, player, piece, square, forOpponent))
            .flatMap(Quarto.takeTurn)
    })
  }
}
