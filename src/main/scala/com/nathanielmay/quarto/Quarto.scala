package com.nathanielmay.quarto

import scala.util.{Failure, Success, Try}
import com.nathanielmay.quarto.Exceptions.{
  InvalidPieceForOpponent,
  InvalidPieceForOpponentError,
  OutOfTurnError}

// TODO add squares variant
object Quarto {
  val empty: PassQuarto = PassQuarto(Board())

  def apply(): PassQuarto = empty

  def isWon(board: Board): Boolean =
    wonBy(board).nonEmpty

  def wonBy(board: Board): List[(Attribute, Line)] = {
    def winningAttrs(line: List[Tile]) = line.flatMap(board.get)
      .flatMap(_.attrs)
      .groupBy(identity)
      .mapValues { _.size }
      .filter { case (_, count) => count >= 4 }
      .keys

    (Horizontal.lines
      .map(winningAttrs)
      .flatMap { _.map { (_, Horizontal) } } :::

    Vertical.lines
      .map(winningAttrs)
      .flatMap { _.map { (_, Vertical) } } :::

    Diagonal.lines
      .map(winningAttrs)
      .flatMap { _.map { (_, Diagonal) } })
  }

  sealed trait Line { val lines: List[List[Tile]] }

  case object Diagonal extends Line {
    val lines = List(
      Board.indexes.zip(Board.indexes).map { case (h, v) => Tile(h, v) } ,
      Board.indexes.zip(Board.indexes.reverse).map { case (h, v) => Tile(h, v) }
    )
  }

  case object Horizontal extends Line {
    val lines = Board.indexes
      .map { h => Board.indexes.map { Tile(h, _) } }
  }

  case object Vertical   extends Line {
    val lines = Board.indexes
      .map { v => Board.indexes.map { Tile(_, v) } }
  }
}

/**
  * Parent class used for Quarto games. Child objects contain game representation and logic
  *
  * board - locations of all placed pieces
  * active - the piece to be placed next or None if one needs to be passed
  */
sealed trait Quarto {
  val board: Board
}

object PassQuarto {
  def apply(board: Board): PassQuarto = new PassQuarto(board)
}

/**
  * Represents a Quarto game that requires a piece to be passed to the opponent
  * a piece cannot be placed in this state
  *
  * @param board locations of all placed pieces
  */
case class PassQuarto private (board: Board) extends Quarto {
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

object PlaceQuarto {
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
case class PlaceQuarto private (board: Board, toPlace: Piece) extends Quarto {
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
    else
      board.place(toPlace, tile) map { board =>
        if      (Quarto.isWon(board)) Right(FinalQuarto(board, Winner(person)))
        else if (board.isFull)        Right(FinalQuarto(board, Tie))
        else                          Left(PassQuarto(board))
      }

  override def toString: String = s"$player needs to place $toPlace on\n$board"
}

/**
  * Represents the end state of a game. No moves can be made on it
  * and it contains the results
  *
  * @param board locations of all placed pieces
  * @param state denotes the winning player or a tie
  */
case class FinalQuarto(board: Board, state: GameEnd) extends Quarto {
  override def toString: String = s"$state!\n$board"
}

/**
  * singleton objects representing player 1 and player 2
  *
  * num - Int used for logic and strings for players
  */
sealed trait Player { val num: Int }
case object P1 extends Player { val num = 1 }
case object P2 extends Player { val num = 2 }

/**
  * Sum type to denote winners and tie games
  */
sealed trait GameEnd
case class Winner(p: Player) extends GameEnd
case object Tie extends GameEnd


