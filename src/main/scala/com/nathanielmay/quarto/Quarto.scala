package com.nathanielmay.quarto

import com.nathanielmay.quarto.board.{Board, Square}
import com.nathanielmay.quarto.piece.{Attribute, Piece}

case object Quarto{
  def apply(): Quarto = new Quarto(Board(), None)
  def apply(board: Board, forOpponent: Option[Piece]): Option[Quarto] =
    if (Quarto.validActive(board, forOpponent))
      Some(new Quarto(board, forOpponent))
    else
      None

  private val hLines  = Board.indexes.map(h => Board.indexes.map(v => Square(h, v)))
  private val vLines  = Board.indexes.map(v => Board.indexes.map(h => Square(h, v)))
  private val dLines  = List(Board.indexes.zip(Board.indexes).map({case (h, v) => board.Square(h, v)}),
                             Board.indexes.zip(Board.indexes.reverse).map({case (h, v) => board.Square(h, v)}))
  //TODO add squares variant
  val allLines: List[List[Square]] = hLines ++ vLines ++ dLines

  def takeTurn(turn: Turn): Option[Quarto] = {
    turn.nextBoard.flatMap(board => Quarto(board, turn.forOpponent))
      .flatMap(next =>
        turn.forOpponent match {
          case Some(p) if turn.game.board.squares.values.exists(_ == p) &&
                          Quarto.isWon(next.board) =>
            Quarto(next.board, None)
          case _ =>
            Some(next)
        })
  }

  def isWon(board: Board): Boolean =
    allLines.exists(winningLine(board, _))

  private def winningLine(board: Board, line: List[Square]): Boolean = {
    val pieces     = line.flatMap(piece => board.squares.get(piece))
    val attrCounts = pieces.foldLeft(Map[Attribute, Int]())((counts, piece) =>
      piece.attrs.foldLeft(counts)((m, attr) =>
        m.updated(attr, m.getOrElse(attr, 0) + 1)))
    attrCounts.exists(4 <= _._2)
  }

  private def validActive(board: Board, forOpponent: Option[Piece]): Boolean = {
    forOpponent.fold(board == Board() || Quarto.isWon(board) || board.isFull)(p => !board.contains(p) || Quarto.isWon(board))
  }

}

sealed case class Quarto private (board: Board, active: Option[Piece]){
  def isLastTurn: Boolean = board.squares.size == 15
  def isComplete: Boolean = Quarto.isWon(board) || board.isFull
  def player:     Player  = if (board.squares.size % 2 == 0) P1 else P2
}

object Turn {
  //error handling logic
  def apply(game: Quarto, player: Player, piece: Piece, square: Square, forOpponent: Option[Piece]): Option[Turn] = {
    val nextBoard        = Board(game.board.squares + (square -> piece))
    val willWin          = nextBoard.map(Quarto.isWon).fold(false)(identity)
    val finalTurn        = game.isLastTurn || willWin
    val validPiece       = game.active.fold(game.active.isEmpty || finalTurn)(p =>
      p == piece && !game.board.contains(piece) && !forOpponent.contains(piece))
    val validForOpponent = forOpponent.fold(game.isLastTurn || willWin)(p =>
      (!game.board.contains(p) && p != piece) || finalTurn)

    if (player == game.player &&
        !game.board.contains(square) &&
        !game.isComplete &&
        validPiece &&
        validForOpponent)
      Some(new Turn(game, player, piece, square, forOpponent))
    else
      None
  }
}

sealed case class Turn private (game: Quarto, player: Player, piece: Piece, square: Square, forOpponent: Option[Piece]){
  lazy val nextBoard = Board(game.board.squares + (square -> piece))
}

sealed abstract class Player(val num: Int)
case object P1 extends Player(1)
case object P2 extends Player(2)