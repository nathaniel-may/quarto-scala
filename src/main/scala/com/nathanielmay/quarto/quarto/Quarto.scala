package com.nathanielmay.quarto.quarto

case class Quarto(board: Board, active: Option[Piece]){
  require(Quarto.validActive(this), s"invalid active piece $active")

  def isLastTurn: Boolean = board.squares.size == 15
  def isComplete: Boolean = Quarto.isWon(this) || board.isFull
  def player: Player      = if (board.squares.size % 2 == 0) P1 else P2
}

case object Quarto{
  def apply(): Quarto = Quarto(Board(), None)

  private val indexes = List(I0, I1, I2, I3)
  private val hLines  = indexes.map(h => indexes.map(v => Square(h, v)))
  private val vLines  = indexes.map(v => indexes.map(h => Square(h, v)))
  private val dLines  = List(indexes zip indexes map {case (h, v) => Square(h, v)},
                                        indexes zip indexes.reverse map {case (h, v) => Square(h, v)})
  //TODO add squares variant
  val allLines: List[List[Square]] = hLines ++ vLines ++ dLines

  def takeTurn(turn: Turn): Quarto = {
    val next = Quarto(Board(turn.game.board.squares + (turn.square -> turn.piece)), turn.forOpponent)
    turn.forOpponent.map(p =>
      if (turn.game.board.squares.values.exists(_ == p) && Quarto.isWon(next))
        Quarto(Board(turn.game.board.squares + (turn.square -> turn.piece)), None)
      else next)
      .getOrElse(next)
  }

  def isWon(game: Quarto): Boolean = {
    allLines.exists(winningLine(game, _))
  }

  def willWin(game: Quarto, piece: Piece, square: Square): Boolean = {
    //TODO can I remove this try catch???
    try {
      Quarto.isWon(Quarto(Board(game.board.squares + (square -> piece)), None))
    } catch {
      case _: Throwable => false
    }
  }

  private def winningLine(game: Quarto, line: List[Square]): Boolean = {
    val pieces = line flatMap {piece => game.board.squares.get(piece)}
    val attrCounts = pieces.foldLeft(Map[Attribute, Int]())((counts, piece) =>
      piece.attrs.foldLeft(counts)((m, attr) =>
        m.updated(attr, m.getOrElse(attr, 0) + 1)))
    attrCounts.exists(4 <= _._2)
  }

  private def validActive(game: Quarto): Boolean = {
    game.active.map(p => !game.board.contains(p) || Quarto.isWon(game))
               .getOrElse(game.board == Board() || Quarto.isWon(game) || game.board.isFull)
  }

}

sealed case class Turn(game: Quarto, player: Player, piece: Piece, square: Square, forOpponent: Option[Piece]) {
  private val finalTurn        = game.isLastTurn || Quarto.willWin(game, piece, square)
  private val validPiece       = game.active.map(p => p == piece && !game.board.contains(piece) && !forOpponent.contains(piece))
                                            .getOrElse(game.active.isEmpty || finalTurn)
  private val validForOpponent = forOpponent.map(p => (!game.board.contains(p) && p != piece) || finalTurn)
                                            .getOrElse(game.isLastTurn || Quarto.willWin(game, piece, square))


  require(player == game.player,        s"it is not player ${player.num}'s turn")
  require(!game.board.contains(square), s"square $square is already occupied")
  require(validPiece,                   s"piece is an illegal piece to place")
  require(validForOpponent,             s"invalid piece $forOpponent chosen for a non-final turn")
  require(!game.isComplete,             s"cannot take a turn on a completed game")
}

sealed abstract class Player(val num: Int)
case object P1 extends Player(1)
case object P2 extends Player(2)

abstract class QuartoError(msg: String) extends Exception {
  override def toString: String = super.toString + s"\n$msg"
}
case class BadTurnError(msg: String) extends QuartoError(msg)
case class InvalidGameError(msg: String) extends QuartoError(msg)