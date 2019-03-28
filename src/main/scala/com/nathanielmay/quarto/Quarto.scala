package com.nathanielmay.quarto

import scala.util.{Failure, Success, Try}
import com.nathanielmay.quarto.Exceptions.{
  InvalidPieceForOpponent,
  InvalidPieceForOpponentError,
  InvalidPlacementError,
  OutOfTurnError}

case object Quarto{
  val hLines: List[List[Tile]]  = Board.indexes.map(h => Board.indexes.map(v => Tile(h, v)))
  val vLines: List[List[Tile]]  = Board.indexes.map(v => Board.indexes.map(h => Tile(h, v)))
  val dLines: List[List[Tile]] = List(Board.indexes.zip(Board.indexes).map({case (h, v) => Tile(h, v)}),
                                      Board.indexes.zip(Board.indexes.reverse).map({case (h, v) => Tile(h, v)}))
  //TODO add squares variant
  private val allLines: List[List[Tile]] = hLines ++ vLines ++ dLines

  val quarto: PassQuarto = Quarto()

  def apply(): PassQuarto = PassQuarto(Board())

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

/**
  * Parent class used for Quarto games. Child objects contain game representation and logic
  *
  * @param board locations of all placed pieces
  * @param active the piece to be placed next or None if one needs to be passed
  */
sealed abstract class Quarto (board: Board, active: Option[Piece])

case object PassQuarto{
  def apply(board: Board): PassQuarto = new PassQuarto(board)
}

/**
  * Represents a Quarto game that requires a piece to be passed to the opponent
  * a piece cannot be placed in this state
  *
  * @param board locations of all placed pieces
  */
case class PassQuarto private (board: Board) extends Quarto(board, None){
  val player: Player = if (board.size % 2 == 0) P1 else P2

  /**
    * Hand a piece to the other player to place. it becomes their turn.
    *
    * @param person      player taking the first turn. Must be player 1.
    * @param forOpponent piece the opponent must place on their next turn
    * @return Success(game) or Failure(exception)
    */
  def passPiece(person: Player, forOpponent: Piece): Try[PlaceQuarto] = {
    if (board.contains(forOpponent))
      Failure(InvalidPieceForOpponent)
    else if (person != player)
      Failure(OutOfTurnError)
    else
      PlaceQuarto(board, forOpponent)
  }

  override def toString: String = s"$player needs to hand a piece to opponent\n$board"
}

case object PlaceQuarto{
  /**
    * Smart constructor for Quarto game where the next step is to place a piece
    *
    * @param board locations of all placed pieces
    * @param forOpponent the piece to be placed next
    * @return Success(game) or Failure(exception)
    */
  def apply(board: Board, forOpponent: Piece): Try[PlaceQuarto] =
    if (board.contains(forOpponent))
      Failure(InvalidPieceForOpponentError)
    else
      Success(new PlaceQuarto(board, forOpponent))
}

/**
  * Represents a Quarto game that requires a piece to be placed
  * a piece cannot be passed in this state
  *
  * @param board locations of all placed pieces
  * @param toPlace the piece that must be placed
  */
case class PlaceQuarto private (board: Board, toPlace: Piece) extends Quarto(board, Some(toPlace)){
  val player: Player = if (board.size % 2 == 0) P2 else P1

  /**
    * place a piece on the board. if the game isn't won,
    * a piece must be passed to the opponent
    *
    * @param person      player taking this turn
    * @param tile        where the piece is being placed
    * @return Success(game) with the next state of the game or Failure(exception)
    */
  def placePiece(person: Player, tile: Tile): Try[Either[PassQuarto, FinalQuarto]] =
    if (person != player)
      Failure(OutOfTurnError)
    else if (board.contains(tile))
      Failure(InvalidPlacementError)
    else
      Board(board.tiles + (tile -> toPlace))
        .flatMap { board =>
          if      (Quarto.isWon(board)) Success(Right(FinalQuarto(board, Winner(person))))
          else if (board.isFull)        Success(Right(FinalQuarto(board, Tie)))
          else                          Success(Left(PassQuarto(board))) }

  override def toString: String = s"$player needs to place $toPlace on\n$board"
}

/**
  * Represents the end state of a game. No moves can be made on it
  * and it contains the results
  *
  * @param board locations of all placed pieces
  * @param state denotes the winning player or a tie
  */
case class FinalQuarto(board: Board, state: GameEnd) extends Quarto(board, None){
  override def toString: String = s"$state!\n$board"
}

/**
  * singleton objects representing player 1 and player 2
  *
  * @constructor only to be used here for P1 and P2 objects
  * @param num Int used for logic and strings for players
  */
sealed abstract class Player(val num: Int)
case object P1 extends Player(1)
case object P2 extends Player(2)

/**
  * Sum type to denote winners and tie games
  */
sealed trait GameEnd
case class Winner(p: Player) extends GameEnd
case object Tie extends GameEnd


