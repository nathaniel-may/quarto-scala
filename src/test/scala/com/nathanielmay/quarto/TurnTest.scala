package com.nathanielmay.quarto

import testingUtil.Pieces._
import testingUtil.Util.{assertNoWin, assertWin, takeTurns, expectError}
import org.scalatest.{FlatSpec, Matchers}
import scala.util.{Failure, Success}
import com.nathanielmay.quarto.Exceptions.{
  InvalidPieceForOpponentError,
  OutOfTurnError,
  InvalidPlacementError,
  GameOverError,
  InvalidPieceForOpponent,
  MalformedTurnError,
  CannotPlacePieceOnFirstTurnError,
  MustPlacePieceError}

class TurnTest extends FlatSpec with Matchers {

  "a Turn" should "fail when active and toPlace are the same" in {
    expectError(InvalidPieceForOpponentError)(P1, WLQF)(List((P2, WLQF, Tile(I0, I0), Some(WLQF))))
  }

  it should "fail when active piece is already placed" in {
    expectError(InvalidPieceForOpponentError)(P1, WLQF)(List(
      (P2, WLQF, Tile(I0, I0), Some(BLQF)),
      (P1, BLQF, Tile(I1, I0), Some(WLQF))
    ))
  }

  it should "fail when tile is occupied" in {
    expectError(InvalidPlacementError)(P1, WLQF)(List(
      (P2, WLQF, Tile(I0, I0), Some(BLQF)),
      (P1, BLQF, Tile(I0, I0), Some(BSRH))
    ))
  }

  it should "fail when player 2 takes the first turn" in {
    expectError(OutOfTurnError)(P2, WLQF)(List())
  }

  it should "fail when it's not the player's turn" in {
    expectError(OutOfTurnError)(P1, WLQF)(List(
      (P1, WLQF, Tile(I0, I0), Some(BLQF))
    ))
  }

  it should "fail when giving opponent a piece that is already placed" in {
    expectError(InvalidPieceForOpponent)(P1, WLQF)(List(
      (P2, WLQF, Tile(I0, I0), Some(BLQF)),
      (P1, BLQF, Tile(I0, I1), Some(WLQF))
    ))
  }

  it should "fail when handing the opponent the piece just placed" in {
    expectError(MalformedTurnError)(P1, WLQF)(List(
      (P2, WLQF, Tile(I0, I0), Some(WLQF)))
    )
  }

  it should "fail when attempting to place a piece other than the one the opponent chose" in {
    expectError(MalformedTurnError)(P1, WLQF)(List(
      (P2, BLQF, Tile(I0, I0), Some(WLRF)))
    )
  }

  it should "fail when attempting to place a piece on the first turn" in {
    assert(
      Quarto().takeTurn(P1, WLQF, Tile(I0, I0), Some(BLQF))
      .failed.get == CannotPlacePieceOnFirstTurnError)
  }

  it should "fail when not placing a piece on a turn that isn't the first turn" in {
    assert(
      Quarto()
        .takeFirstTurn(P1, WLQF)
        .flatMap(game => game.takeFirstTurn(P2, WLQF))
        .failed.get == MustPlacePieceError
    )
  }

  it should "fail when not placing a piece on a turn that isn't the first turn regardless of player" in {
    assert(
      Quarto()
        .takeFirstTurn(P1, WLQF)
        .flatMap(game => game.takeFirstTurn(P1, BLQF))
        .failed.get == MustPlacePieceError
    )
  }

  it should "accept valid active piece for winning move" in {
    assertWin(P1, WLQF)(List(
      (P2, WLQF, Tile(I0, I0), Some(BLQF)),
      (P1, BLQF, Tile(I0, I1), Some(BLRH)),
      (P2, BLRH, Tile(I0, I2), Some(WLQH)),
      (P1, WLQH, Tile(I0, I3), Some(BSRH)))
    )
  }

  it should "accept None forOpponent for winning move" in {
    assertWin(P1, WLQF)(List(
      (P2, WLQF, Tile(I0, I0), Some(BLQF)),
      (P1, BLQF, Tile(I0, I1), Some(BLRH)),
      (P2, BLRH, Tile(I0, I2), Some(WLQH)),
      (P1, WLQH, Tile(I0, I3), None))
    )
  }

  it should "accept invalid piece forOpponent for winning move but not save it" in {
    takeTurns(P1, WLQF)(List(
      (P2, WLQF, Tile(I0, I0), Some(BLQF)),
      (P1, BLQF, Tile(I0, I1), Some(BLRH)),
      (P2, BLRH, Tile(I0, I2), Some(WLQH)),
      (P1, WLQH, Tile(I0, I3), Some(WLQF)))
    ) match {
      case Failure(_) => fail()
      case Success(game) => assert(Quarto.isWon(game.board) && game.active.isEmpty)
    }
  }

  it should "accept when all pieces are played and the last piece wins" in {
    assertWin(P1, BLRF)(List(
      (P2, BLRF, Tile(I1, I1), Some(BLRH)),
      (P1, BLRH, Tile(I1, I3), Some(BLQF)),
      (P2, BLQF, Tile(I0, I0), Some(BLQH)),
      (P1, BLQH, Tile(I2, I0), Some(BSRF)),
      (P2, BSRF, Tile(I3, I2), Some(BSRH)),
      (P1, BSRH, Tile(I2, I1), Some(BSQF)),
      (P2, BSQF, Tile(I1, I0), Some(BSQH)),
      (P1, BSQH, Tile(I0, I1), Some(WLQF)),
      (P2, WLQF, Tile(I3, I3), Some(WLRH)),
      (P1, WLRH, Tile(I3, I1), Some(WLQH)),
      (P2, WLQH, Tile(I1, I2), Some(WSRH)),
      (P1, WSRH, Tile(I0, I2), Some(WSQF)),
      (P2, WSQF, Tile(I2, I3), Some(WSQH)),
      (P1, WSQH, Tile(I0, I3), Some(WLRF)),
      (P2, WLRF, Tile(I3, I0), Some(WSRF)),
      (P1, WSRF, Tile(I2, I2), None))
    )
  }

  it should "accept when all pieces are played and the last piece does not win" in {
    assertNoWin(P1, BLRF)(List(
      (P2, BLRF, Tile(I1, I1), Some(BLRH)),
      (P1, BLRH, Tile(I1, I3), Some(BLQF)),
      (P2, BLQF, Tile(I0, I0), Some(BLQH)),
      (P1, BLQH, Tile(I2, I0), Some(BSRF)),
      (P2, BSRF, Tile(I3, I2), Some(BSRH)),
      (P1, BSRH, Tile(I2, I1), Some(BSQF)),
      (P2, BSQF, Tile(I1, I0), Some(BSQH)),
      (P1, BSQH, Tile(I0, I1), Some(WLRF)),
      (P2, WLRF, Tile(I3, I0), Some(WLRH)),
      (P1, WLRH, Tile(I3, I1), Some(WLQF)),
      (P2, WLQF, Tile(I3, I3), Some(WLQH)),
      (P1, WLQH, Tile(I1, I2), Some(WSRF)),
      (P2, WSRF, Tile(I2, I3), Some(WSRH)),
      (P1, WSRH, Tile(I0, I2), Some(WSQF)),
      (P2, WSQF, Tile(I0, I3), Some(WSQH)),
      (P1, WSQH, Tile(I2, I2), None))
    )
  }

  it should "reject when game is already won" in {
    expectError(GameOverError)(P1, WLQF)(List(
      (P2, WLQF, Tile(I0, I0), Some(BLQF)),
      (P1, BLQF, Tile(I0, I1), Some(BLRH)),
      (P2, BLRH, Tile(I0, I2), Some(WLQH)),
      (P1, WLQH, Tile(I0, I3), None),
      (P2, BSRH, Tile(I0, I0), Some(WSQF)))
    )
  }

}
