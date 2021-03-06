package com.nathanielmay.quarto

object Exceptions {
  sealed class QuartoError(msg: String) extends Exception(msg)
  sealed class BoardError(msg: String)  extends QuartoError(msg)

  case object DuplicatePieceError          extends BoardError("each piece may only appear once on a board")
  case object InvalidPieceForOpponentError extends QuartoError("must choose a valid piece for opponent")
  case object OutOfTurnError               extends QuartoError("not this player's turn")
  case object InvalidPlacementError        extends QuartoError("this board space is already occupied")
  case object GameOverError                extends QuartoError("cannot take a turn on a completed game")
  case object InvalidPieceForOpponent      extends QuartoError("cannot give this piece to opponent")
}
