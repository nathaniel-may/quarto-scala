package com.nathanielmay.quarto

import com.nathanielmay.quarto.board.{Board, Square}
import com.nathanielmay.quarto.piece.{Attribute, Piece}

case object Quarto{
  def apply(): Quarto = Quarto(Board(), None)

  private val hLines  = Board.indexes.map(h => Board.indexes.map(v => Square(h, v)))
  private val vLines  = Board.indexes.map(v => Board.indexes.map(h => Square(h, v)))
  private val dLines  = List(Board.indexes.zip(Board.indexes).map({case (h, v) => board.Square(h, v)}),
                             Board.indexes.zip(Board.indexes.reverse).map({case (h, v) => board.Square(h, v)}))
  //TODO add squares variant
  val allLines: List[List[Square]] = hLines ++ vLines ++ dLines

  def takeTurn(turn: Turn): Quarto = {
    val next = Quarto(turn.nextBoard, turn.forOpponent)
    turn.forOpponent.fold(next)(p =>
      if (turn.game.board.squares.values.exists(_ == p) && Quarto.isWon(next.board))
        Quarto(next.board, None)
      else next)
  }

  def isWon(board: Board): Boolean = {
    allLines.exists(winningLine(board, _))
  }

  private def winningLine(board: Board, line: List[Square]): Boolean = {
    val pieces     = line.flatMap(piece => board.squares.get(piece))
    val attrCounts = pieces.foldLeft(Map[Attribute, Int]())((counts, piece) =>
      piece.attrs.foldLeft(counts)((m, attr) =>
        m.updated(attr, m.getOrElse(attr, 0) + 1)))
    attrCounts.exists(4 <= _._2)
  }

  private def validActive(game: Quarto): Boolean = {
    game.active.fold(game.board == Board() || Quarto.isWon(game.board) || game.board.isFull)(p => !game.board.contains(p) || Quarto.isWon(game.board))
  }

}

case class Quarto(board: Board, active: Option[Piece]){
  require(Quarto.validActive(this), s"invalid active piece $active")

  def isLastTurn: Boolean = board.squares.size == 15
  def isComplete: Boolean = Quarto.isWon(board) || board.isFull
  def player:     Player  = if (board.squares.size % 2 == 0) P1 else P2
}

sealed case class Turn(game: Quarto, player: Player, piece: Piece, square: Square, forOpponent: Option[Piece]) {
  require(player == game.player,        s"it is not player ${player.num}'s turn")
  require(!game.board.contains(square), s"square $square is already occupied")
  require(!game.isComplete,             s"cannot take a turn on a completed game")

  val nextBoard                = Board(game.board.squares + (square -> piece))
  private val willWin          = Quarto.isWon(nextBoard)
  private val finalTurn        = game.isLastTurn || willWin
  private val validPiece       = game.active.fold(game.active.isEmpty || finalTurn)(p =>
                                   p == piece && !game.board.contains(piece) && !forOpponent.contains(piece))
  private val validForOpponent = forOpponent.fold(game.isLastTurn || willWin)(p =>
                                   (!game.board.contains(p) && p != piece) || finalTurn)

  require(validPiece,       s"piece is an illegal piece to place")
  require(validForOpponent, s"invalid piece $forOpponent chosen for a non-final turn")
}

sealed abstract class Player(val num: Int)
case object P1 extends Player(1)
case object P2 extends Player(2)