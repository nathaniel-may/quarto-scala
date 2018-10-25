package testingUtil

import com.nathanielmay.quarto.{Attribute, Board, Piece, Tile, Quarto, PassQuarto, PlaceQuarto, GameEnd, FinalQuarto, Winner, Tie, Player, P1, P2}

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

  def takeTurn(game: Quarto)(t: Turn): Try[Quarto] =
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

  def takeTurns(q0: Quarto = Quarto())(turns: List[Turn]): Try[Quarto] =
    turns.foldLeft[Try[Quarto]](Success(q0))((tryGame, turn) =>
      tryGame.flatMap(game => takeTurn(game)(turn)))

  def takeTurnsAndStop(q0: Quarto = Quarto())(turns: List[Turn]): Try[Quarto] =
    turns.foldLeft[Try[Quarto]](Success(q0))({ case (tryGame, turn) =>
      tryGame.flatMap(game => (game, turn) match {
        case (passQ: PassQuarto, Pass(person, piece)) =>
          passQ.passPiece(person, piece)
        case (placeQ: PlaceQuarto, Place(person, tile)) =>
          placeQ.placePiece(person, tile).map(_.merge)
        case (q: Quarto, _) => Success(q)
      })
    })

  //TODO is this used anymore?
  def getTurns(tiles: List[Tile], pieces: List[Piece]): List[Turn] = {
    def playerSeq(n: Int): List[Player] =
      List.tabulate(n)(i => if (i % 2 == 0) P1 else P2)

    (playerSeq(tiles.size), tiles, pieces)
      .zipped.toList
      .flatMap{case (player: Player, t: Tile, p: Piece) => List(Pass(player, p), Place(player.switch, t))}
  }

  def wonWith(game: FinalQuarto): List[Line] = {
    import Quarto.{hLines, vLines, dLines}

    def wins(line: List[Tile]): Boolean =
      line.flatMap(piece => game.board.get(piece))
        .foldLeft[Map[Attribute, Int]](Map())((counts, piece) =>
        piece.attrs.foldLeft(counts)((m, attr) =>
          m.updated(attr, m.getOrElse(attr, 0) + 1)))
        .exists(4 <= _._2)

    for {
      d <- dLines.filter(wins).map(_ => Diagonal)
      dh <- d :: hLines.filter(wins).map(_ => Horizontal)
      dhv <- dh :: vLines.filter(wins).map(_ => Vertical)
    } yield dhv
  }

  private implicit class switchablePlayer(p: Player){
    def switch: Player = p match {
      case P1 => P2
      case P2 => P1
    }
  }
}

sealed trait Line
case object Diagonal extends Line
case object Horizontal extends Line
case object Vertical extends Line

sealed trait Turn
case class Pass(person: Player, forOpponent: Piece) extends Turn
case class Place(person: Player, tile: Tile) extends Turn