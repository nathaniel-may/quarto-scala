package com.nathanielmay.quarto

import scala.util.{Failure, Success, Try}
import com.nathanielmay.quarto.Exceptions.{
  GameOverError,
  InvalidPieceForOpponent,
  InvalidPieceForOpponentError,
  InvalidPlacementError,
  BadPieceError,
  MustPlacePieceError,
  MustPassPieceError,
  OutOfTurnError}

case object Quarto{
  def apply(): Quarto = new Quarto(Board(), None)

  /** Smart constructor for Quarto game
    *
    * @param board locations of all placed pieces
    * @param forOpponent the piece to be placed next. None signifies
    *                    the game is over or one must be chosen to
    *                    complete the turn
    * @return Success(game) or Failure(exception)
    */
  def apply(board: Board, forOpponent: Option[Piece]): Try[Quarto] =
    if (forOpponent.fold(true)(!board.contains(_)))
      Success(new Quarto(board, forOpponent))
    else
      Failure(InvalidPieceForOpponentError)

  private val hLines  = Board.indexes.map(h => Board.indexes.map(v => Tile(h, v)))
  private val vLines  = Board.indexes.map(v => Board.indexes.map(h => Tile(h, v)))
  private val dLines  = List(Board.indexes.zip(Board.indexes).map({case (h, v) => Tile(h, v)}),
                             Board.indexes.zip(Board.indexes.reverse).map({case (h, v) => Tile(h, v)}))
  //TODO add squares variant
  private val allLines: List[List[Tile]] = hLines ++ vLines ++ dLines

  def isWon(board: Board): Boolean = {
    def winningLine(board: Board, line: List[Tile]): Boolean = {
      line.flatMap(piece => board.get(piece))
        .foldLeft[Map[Attribute, Int]](Map())((counts, piece) =>
        piece.attrs.foldLeft(counts)((m, attr) =>
          m.updated(attr, m.getOrElse(attr, 0) + 1)))
        .exists(4 <= _._2)
    }

    allLines.exists(winningLine(board, _))
  }

}

/** class used for Quarto game representation and logic
  *
  * @constructor only used by apply methods
  * @param board locations of all placed pieces
  * @param active the piece to be placed next
  */
sealed case class Quarto private (board: Board, active: Option[Piece]) {
  val isFirstTurn: Boolean = board.isEmpty && active.isEmpty
  val isLastTurn: Boolean = board.size == 15
  val isComplete: Boolean = Quarto.isWon(board) || board.isFull
  val player: Player =
    if(board.size % 2 == 0 && active.isEmpty && !board.isFull) P1
    else if (board.size % 2 == 0 && active.isDefined) P2
    else if(board.size % 2 == 1 && active.isEmpty && !board.isFull) P2
    else P1 //TODO better way to do this? also what about the way a full or won board works?
  val lastPlacedBy: Player = if (board.size % 2 == 0) P1 else P2
  val state: GameState =
    if (Quarto.isWon(board)) Winner(lastPlacedBy)
    else if(board.isFull) Tie
    else InProgress

  /** place a piece on the board if the game isn't won,
    * a piece must be passed to the opponent
    *
    * @param person      player taking this turn
    * @param piece       piece being placed
    * @param tile        where the piece is being placed
    * @return Success(game) with the next state of the game or Failure(exception)
    */
  def placePiece(person: Player, piece: Piece, tile: Tile): Try[Quarto] = {
    if (person != player)
      Failure(OutOfTurnError)
    else if (board.contains(tile))
      Failure(InvalidPlacementError)
    else if (isComplete)
      Failure(GameOverError)
    else if (active.isEmpty)
      MustPassPieceError
    else if (active.fold(true)(_ != piece || board.contains(piece)))
      Failure(BadPieceError)

    Board(board.tiles + (tile -> piece))
      .fold(f => Failure(f), board => Quarto(board, None)
    )
  }

  /** hand a piece to the other player to place. it becomes their turn.
    *
    * @param person      player taking the first turn. Must be player 1.
    * @param forOpponent piece the opponent must place on their next turn
    * @return Success(game) or Failure(exception)
    */
  def passPiece(person: Player, forOpponent: Piece): Try[Quarto] = {
    if (board.contains(forOpponent))
      Failure(InvalidPieceForOpponent)
    else if (person != player)
      Failure(OutOfTurnError)
    else if (active.isDefined)
      Failure(MustPlacePieceError)
    else
      Quarto(board, Some(forOpponent))
  }

  override def toString: String =
    if(active.isEmpty)
      s"$player needs to hand a piece to opponent\n$board"
    else {
      val prettyActive = active match {
        case None => "nothing"
        case Some(p) => p.toString
      }
      s"$player needs to place $prettyActive on\n$board"
    }
}

/** singleton objects representing player 1 and player 2
  *
  * @constructor only to be used here for P1 and P2 objects
  * @param num Int used for logic and strings for players
  */
sealed abstract class Player(val num: Int)
case object P1 extends Player(1)
case object P2 extends Player(2)

/** Sum type to denote winners, ties, or an inprogress game
  */
sealed trait GameState
case class Winner(p: Player) extends GameState
case object Tie extends GameState
case object InProgress extends GameState


