package testingUtil

import com.nathanielmay.quarto.{Board, Piece, Player, Quarto, Tile}

import scala.util.{Failure, Success, Try}

object Util {

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

  def takeTurns(q0: Quarto)(turns: List[Turn]): Try[Quarto] =
    turns.foldLeft[Try[Quarto]](Success(q0))({ case (tryGame, turn) =>
      turn match {
        case Pass(person, piece) =>
          tryGame.fold(f => Failure(f), _.passPiece(person, piece))
        case Place(person, piece, tile) =>
          tryGame.fold(f => Failure(f), _.placePiece(person, piece, tile))
      }
    })
}

sealed abstract class Turn
case class Pass(person: Player, forOpponent: Piece) extends Turn
case class Place(person: Player, piece: Piece, tile: Tile) extends Turn