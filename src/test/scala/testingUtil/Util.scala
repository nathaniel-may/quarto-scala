package testingUtil

import com.nathanielmay.quarto.{Board, Piece, Tile, Quarto, PassQuarto, PlaceQuarto, GameEnd, FinalQuarto, Winner, Tie, Player, P1, P2}

import scala.util.{Failure, Success, Try}

object Util {

  def successQuarto(board: Try[Board], forOpponent: Option[Piece]): Boolean =
    forOpponent match {
      case None => quarto(board).isSuccess
      case Some(p) => quarto(board, p).isSuccess
    }

  def quarto(board: Try[Board]): Try[Quarto] =
    board.flatMap(b => Success(PassQuarto(b)))

  def quarto(board: Try[Board], forOpponent: Piece): Try[Quarto] =
    board.flatMap(b => PlaceQuarto(b, forOpponent))

  def expectError(e: Exception)(turns: List[Turn]): Boolean =
    takeTurns(Quarto())(turns).failed.get == e

  def assertWin(turns: List[Turn]): Unit = {
    turnsEndIn(turns).fold(
      _ => assert(false),
      state => assert(state == Winner(P1) || state == Winner(P2)))
  }

  def assertTie(turns: List[Turn]): Unit = {
    turnsEndIn(turns).fold(_ => assert(false) , result => assert(result == Tie))
  }

  private def turnsEndIn(turns: List[Turn]): Try[GameEnd] =
    takeTurns(Quarto())(turns).flatMap({
      case FinalQuarto(_, state) => Success(state)
      case _ => Failure(new Exception("Bad Test. Game didn't end."))
    })

  def takeTurns(q0: Quarto = Quarto())(turns: List[Turn]): Try[Quarto] =
    turns.foldLeft[Try[Quarto]](Success(q0))({ case (tryGame, turn) =>
      tryGame.flatMap(game => (game, turn) match {
          case (passQ: PassQuarto, Pass(person, piece)) =>
            passQ.passPiece(person, piece)
          case (placeQ: PlaceQuarto, Place(person, tile)) =>
            placeQ.placePiece(person, tile).map(_.merge)
          case _ => Failure(new Exception("Bad Test. The game does not have that method."))
        })
    })

  def getTurns(tiles: List[Tile], pieces: List[Piece]): List[Turn] = {
    def playerSeq(n: Int): List[Player] =
      List.tabulate(n)(i => if (i % 2 == 0) P1 else P2)

    (playerSeq(tiles.size), tiles, pieces)
      .zipped.toList
      .flatMap{case (player: Player, t: Tile, p: Piece) => List(Pass(player, p), Place(player.switch, t))}
  }

  private implicit class switchablePlayer(p: Player){
    def switch: Player = p match {
      case P1 => P2
      case P2 => P1
    }
  }
}

sealed trait Turn
case class Pass(person: Player, forOpponent: Piece) extends Turn
case class Place(person: Player, tile: Tile) extends Turn