package com.nathanielmay.quarto.quarto

import scala.util.{Try, Failure, Success}

case class Quarto(board: Board, active: Option[Piece]){

  def takeTurn(piece: Piece, square: Square, forOpponent: Option[Piece]): Try[Quarto] = {
    Try({
      Quarto.validateGame(this) match {
        case Failure(f) => throw f
        case Success(_) =>
      }

      Quarto.validateTurn(this, piece, square, forOpponent) match {
        case Failure(f) => throw f
        case Success(_) =>
      }

      val next = Quarto(Board(board.squares + (square -> piece)), forOpponent)
      forOpponent.map(p =>
        if (board.squares.values.exists(_ == p) && Quarto.isWon(next))
          Quarto(Board(board.squares + (square -> piece)), None)
        else next)
        .getOrElse(next)
    })
  }

  def isValid: Boolean = board.isValid && active.map(p => !board.contains(p))
                                                .getOrElse(board == Board.newBoard || Quarto.isWon(this) || board.isFull)

  def isLastTurn: Boolean = board.squares.size == 15
}

case object Quarto{
  val newGame = Quarto(Board.newBoard, None)

  private val indexes = List(I0, I1, I2, I3)
  private val hLines = indexes.map(h => indexes.map(v => Square(h, v)))
  private val vLines = indexes.map(v => indexes.map(h => Square(h, v)))
  private val dLines = List(indexes zip indexes map {case (h, v) => Square(h, v)},
                                        indexes zip indexes.reverse map {case (h, v) => Square(h, v)})
  //TODO add squares variant
  val allLines: List[List[Square]] = hLines ++ vLines ++ dLines

  def takeTurns(q0: => Quarto)(turns: List[(Piece, Square, Option[Piece])]) : Try[Quarto] =
    turns.foldLeft(Try(q0)) { case (game, (piece, square, active)) =>
      game flatMap (_.takeTurn(piece, square, active))
    }

  def isWon(game: Quarto): Boolean = {
    allLines.exists(winningLine(game, _))
  }

  def winningLine(game: Quarto, line: List[Square]): Boolean = {
    val pieces = line flatMap {piece => game.board.squares.get(piece)}
    val attrCounts = pieces.foldLeft(Map[Attribute, Int]())((counts, piece) =>
      piece.attrs.foldLeft(counts)((m, attr) =>
        m.updated(attr, m.getOrElse(attr, 0) + 1)))
    attrCounts.exists(4 <= _._2)
  }

  def willWin(game: Quarto, piece: Piece, square: Square): Boolean = {
    Quarto.isWon(Quarto(Board(game.board.squares + (square -> piece)), None))
  }

  def validateGame(game: Quarto): Try[Unit] = {
    Try({
      if (!game.isValid)                                 throw InvalidGameError("not a valid game")
      if (Quarto.isWon(game))                            throw InvalidGameError("cannot take a turn on a completed game")
      if (game.active.isEmpty && game != Quarto.newGame) throw InvalidGameError(s"no active piece set for in progress game")
    })
  }

  def validateTurn(game: Quarto, piece: Piece, square: Square, forOpponent: Option[Piece]): Try[Unit] = {
    Try({
      if (game.board.squares.contains(square))          throw BadTurnError(s"square $square is already occupied")
      if (game.board.squares.values.exists(_ == piece)) throw BadTurnError(s"piece $piece has already been placed")
      if (game.active.exists(p => p != piece))          throw BadTurnError(s"must place the active piece: $game.active. actual piece placed: $piece")
      if (forOpponent.contains(piece))                  throw BadTurnError("piece being placed and piece for opponent are the same")
      if (forOpponent.exists(p => game.board.squares.values.exists(_ == p) && !Quarto.willWin(game, piece, square)))
        throw BadTurnError("piece for opponent has already been placed")
      if (forOpponent.isEmpty && !Quarto.willWin(game, piece, square) && !game.isLastTurn)
        throw BadTurnError(s"no piece chosen for opponent and game still has more turns")
    })
  }

}

abstract class QuartoError(msg: String) extends Exception {
  override def toString: String = super.toString + s"\n$msg"
}
case class BadTurnError(msg: String) extends QuartoError(msg)
case class InvalidGameError(msg: String) extends QuartoError(msg)