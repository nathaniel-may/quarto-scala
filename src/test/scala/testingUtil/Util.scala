package testingUtil

import com.nathanielmay.quarto.board.Square
import com.nathanielmay.quarto.piece.Piece
import com.nathanielmay.quarto.{Player, Quarto, Turn}

object Util {

  def assertWin(turns: List[(Player, Piece, Square, Option[Piece])]): Unit = {
    assert(turnsWon(turns))
  }

  def assertNoWin(turns: List[(Player, Piece, Square, Option[Piece])]): Unit = {
    assert(!turnsWon(turns))
  }

  def turnsWon(turns: List[(Player, Piece, Square, Option[Piece])]): Boolean = Quarto.isWon(takeTurns(Quarto())(turns).board)

  def takeTurns(q0: Quarto)(turns: List[(Player, Piece, Square, Option[Piece])]): Quarto = {
    turns.foldLeft(q0)({case (game, (player, piece, square, forOpponent)) =>
      Quarto.takeTurn(Turn(game, player, piece, square, forOpponent))
    })
  }
}
