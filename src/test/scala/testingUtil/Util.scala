package testingUtil

// Scala
import scala.util.{Failure, Success, Try}

// Project
import com.nathanielmay.quarto._, Quarto.Line

object Util {

  def takeTurn(t: Turn, game: Quarto): Try[Quarto] =
    (game, t) match {
      case (passQ: PassQuarto, Pass(person, piece)) =>
        passQ.passPiece(person, piece)

      case (placeQ: PlaceQuarto, Place(person, tile)) =>
        placeQ.placePiece(person, tile).map(_.merge)

      case (_: FinalQuarto, _)       => Failure(new Exception("game is over. no move moves can be made"))
      case (_: PassQuarto, _: Place) => Failure(new Exception("expected to pass a piece. instead got a place turn"))
      case (_: PlaceQuarto, _: Pass) => Failure(new Exception("expected to place a piece. instead got a pass turn"))
      case _                         => Failure(new Exception("cannot make this move on this kind of board"))
    }

  def takeTurns(turns: List[Turn]): Try[Quarto] =
    turns.foldLeft[Try[Quarto]](Success(Quarto.empty)) { (tryGame, turn) =>
      tryGame flatMap { takeTurn(turn, _) }
    }

  def takeTurnsAndStop(turns: List[Turn]): Quarto =
    turns.foldLeft[Quarto](Quarto.empty) { (g, turn) =>
      takeTurn(turn, g) match {
        case Success(next) => next
        case Failure(_)    => g
      }
    }

  def turnsEndResult(turns: List[Turn]): Option[GameEnd] =
    takeTurns(turns)
      .fold(_ => None, q => Some(q))
      .flatMap {
        case FinalQuarto(_, state) => Some(state)
        case _ => None
      }

  private def switch(p: Player): Player = p match {
    case P1 => P2
    case P2 => P1
  }

  def getTurns(tiles: List[Tile], pieces: List[Piece]): List[Turn] = {
    def playerSeq(n: Int): List[Player] =
      Stream.iterate[Player](P1)(switch).take(n).toList

    (playerSeq(tiles.size), tiles, pieces)
      .zipped
      .toList
      .flatMap { case (player: Player, t: Tile, p: Piece) =>
        List(Pass(player, p), Place(switch(player), t))
      }
  }

  def wonWith(game: FinalQuarto): List[Line] =
    Quarto.wonBy(game.board) map { _._2 }

  def validQuarto(board: Try[Board], forOpponent: Option[Piece]): Boolean =
    forOpponent match {
      case None    => quartoFrom(board).isSuccess
      case Some(p) => quartoFrom(board, p).isSuccess
    }

  def quartoFrom(board: Try[Board]): Try[Quarto] =
    board.flatMap { b => Success(PassQuarto(b)) }

  def quartoFrom(board: Try[Board], forOpponent: Piece): Try[Quarto] =
    board.flatMap { b => PlaceQuarto(b, forOpponent) }

  def expectError(e: Exception)(turns: List[Turn]): Boolean =
    takeTurns(turns).failed.get == e

  def assertResult(turns: List[Turn])(f: GameEnd => Boolean): Unit = {
    turnsEndResult(turns) match {
      case None      => assert(false)
      case Some(end) => assert(f(end))
    }
  }

  def assertWin(turns: List[Turn]): Unit =
    assertResult(turns)(r => r == Winner(P1) || r == Winner(P2))

  def assertTie(turns: List[Turn]): Unit =
    assertResult(turns)(_ == Tie)

}

sealed trait Turn
case class Pass(person: Player, forOpponent: Piece) extends Turn
case class Place(person: Player, tile: Tile)        extends Turn