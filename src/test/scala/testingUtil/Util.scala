package testingUtil

// Scala
import scala.util.{Failure, Success, Try}

// Project
import com.nathanielmay.quarto.{Attribute, Board, Piece, Tile, Quarto, PassQuarto, PlaceQuarto, GameEnd, FinalQuarto, Winner, Tie, Player, P1, P2}
import Quarto.{hLines, vLines, dLines}

object Util {

  def takeTurn(t: Turn, game: Quarto = Quarto()): Try[Quarto] =
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

  def takeTurns(turns: List[Turn], game: Quarto = Quarto()): Try[Quarto] =
    turns.foldLeft[Try[Quarto]](Success(game))((tryGame, turn) =>
      tryGame.flatMap(takeTurn(turn, _)))

  def takeTurnsAndStop(turns: List[Turn], game: Quarto = Quarto()): Try[Quarto] =
    turns.foldLeft[Try[Quarto]](Success(game)) { (tryGame, turn) =>
      tryGame.flatMap(g => (g, turn) match {
        case (passQ: PassQuarto, Pass(person, piece)) =>
          passQ.passPiece(person, piece)
        case (placeQ: PlaceQuarto, Place(person, tile)) =>
          placeQ.placePiece(person, tile).map(_.merge)
        case (q: Quarto, _) => Success(q)
      })}

  def turnsEndResult(turns: List[Turn]): Option[GameEnd] =
    takeTurns(turns).fold(_ => None, q => Some(q)).flatMap {
      case FinalQuarto(_, state) => Some(state)
      case _ => None
    }

  // Using implicit because this function is only useful for internal testing
  private implicit class switchablePlayer(p: Player){
    def switch: Player = p match {
      case P1 => P2
      case P2 => P1
    }
  }

  def getTurns(tiles: List[Tile], pieces: List[Piece]): List[Turn] = {
    def playerSeq(n: Int): List[Player] =
      List.tabulate(n)(i => if (i % 2 == 0) P1 else P2)

    (playerSeq(tiles.size), tiles, pieces)
      .zipped.toList
      .flatMap{case (player: Player, t: Tile, p: Piece) => List(Pass(player, p), Place(player.switch, t))}
  }

  def wonWith(game: FinalQuarto): List[Line] = {

    def wins(line: List[Tile]): Boolean =
      line.flatMap(piece => game.board.get(piece))
        .foldLeft[Map[Attribute, Int]](Map())((counts, piece) =>
        piece.attrs.foldLeft(counts)((m, attr) =>
          m.updated(attr, m.getOrElse(attr, 0) + 1)))
        .exists(4 <= _._2)

    for {
      d   <- dLines.filter(wins).map { _ => Diagonal }
      dh  <- d :: hLines.filter(wins).map { _ => Horizontal }
      dhv <- dh :: vLines.filter(wins).map { _ => Vertical }
    } yield dhv
  }

  def validQuarto(board: Try[Board], forOpponent: Option[Piece]): Boolean =
    forOpponent match {
      case None => quartoFrom(board).isSuccess
      case Some(p) => quartoFrom(board, p).isSuccess
    }

  def quartoFrom(board: Try[Board]): Try[Quarto] =
    board.flatMap(b => Success(PassQuarto(b)))

  def quartoFrom(board: Try[Board], forOpponent: Piece): Try[Quarto] =
    board.flatMap(b => PlaceQuarto(b, forOpponent))

  def expectError(e: Exception)(turns: List[Turn], game: Quarto = Quarto()): Boolean =
    takeTurns(turns).failed.get == e

  def assertResult(turns: List[Turn])(f: GameEnd => Boolean): Unit = {
    turnsEndResult(turns) match {
      case None => assert(false)
      case Some(end) => assert(f(end))
    }
  }

  def assertWin(turns: List[Turn]): Unit = assertResult(turns)(r => r == Winner(P1) || r == Winner(P2))

  def assertTie(turns: List[Turn]): Unit = assertResult(turns)(_ == Tie)

}

sealed trait Line
case object Diagonal   extends Line
case object Horizontal extends Line
case object Vertical   extends Line

sealed trait Turn
case class Pass(person: Player, forOpponent: Piece) extends Turn
case class Place(person: Player, tile: Tile)        extends Turn