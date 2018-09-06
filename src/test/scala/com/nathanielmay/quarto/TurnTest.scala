package com.nathanielmay.quarto

import testingUtil.Pieces._
import testingUtil.{Pass, Place}
import testingUtil.Util.{assertNoWin, assertWin, expectError}
import org.scalatest.{FlatSpec, Matchers}
import com.nathanielmay.quarto.Exceptions.{
  InvalidPieceForOpponentError,
  OutOfTurnError,
  InvalidPlacementError,
  GameOverError,
  InvalidPieceForOpponent,
  BadPieceError,
  MustPlacePieceError,
  MustPassPieceError}

class TurnTest extends FlatSpec with Matchers {

  "a Turn" should "fail when giving opponent a piece that is already placed" in {
    expectError(InvalidPieceForOpponentError)(List(
      Pass(P1, WLQF),
      Place(P2, WLQF, Tile(I0, I0)),
      Pass(P2, WLQF)))
  }

  it should "fail when tile is occupied" in {
    expectError(InvalidPlacementError)(List(
      Pass(P1, WLQF),
      Place(P2, WLQF, Tile(I0, I0)),
      Pass(P2, BLQF),
      Place(P1, BLQF, Tile(I0, I0))))
  }

  it should "fail when player 2 takes the first turn" in {
    expectError(OutOfTurnError)(List(Pass(P2, WLQF)))
  }

  it should "fail when player 1 tries to take a second turn in a row" in {
    expectError(OutOfTurnError)(List(
      Pass(P1, WLQF),
      Place(P1, WLQF, Tile(I0, I0)),
      Pass(P1, BLQF)))
  }

  it should "fail when player 2 tries to go first" in {
    expectError(OutOfTurnError)(List(Pass(P2, WLQF)))
  }

  it should "fail when attempting to place a piece other than the one the opponent chose" in {
    expectError(BadPieceError)(List(
      Pass(P1, WLQF),
      Place(P2, BLQF, Tile(I0, I0))))
  }

  it should "fail when attempting to place a piece before one has been passed on the first turn" in {
    Quarto()
      .placePiece(P1, WLQF, Tile(I0, I0))
      .fold(e => assert(e == MustPassPieceError), fail("shouldn't be able to place a piece first"))
  }

  it should "fail when passing two pieces in a row" in {
    assert(
      Quarto()
        .passPiece(P1, WLQF)
        .flatMap(game => game.passPiece(P2, BLQF))
        .failed.get == MustPlacePieceError)
  }

  it should "fail when not placing a piece on a turn that isn't the first turn regardless of player" in {
    expectError(MustPlacePieceError)(List(
      Pass(P1, WLQF),
      Pass(P1, BLQF)))
  }

  it should "accept valid active piece for winning move" in {
    assertWin(List(
      Pass(P1, WLQF),
      Place(P2, WLQF, Tile(I0, I0)),
      Pass(P2, BLQF),
      Place(P1, BLQF, Tile(I0, I1)),
      Pass(P1, BLRH),
      Place(P2, BLRH, Tile(I0, I2)),
      Pass(P2, WLQH),
      Place(P1, WLQH, Tile(I0, I3))))
  }

  it should "accept None forOpponent for winning move" in {
    assertWin(List(
      Pass(P1, WLQF),
      Place(P2, WLQF, Tile(I0, I0)),
      Pass(P2, BLQF),
      Place(P1, BLQF, Tile(I0, I1)),
      Pass(P1, BLRH),
      Place(P2, BLRH, Tile(I0, I2)),
      Pass(P2, WLQH),
      Place(P1, WLQH, Tile(I0, I3))))
  }

  it should "cannot pass a piece after placing a winning piece" in {
    expectError(GameOverError)(List(
      Pass(P1, WLQF),
      Place(P2, WLQF, Tile(I0, I0)),
      Pass(P2, BLQF),
      Place(P1, BLQF, Tile(I0, I1)),
      Pass(P1, BLRH),
      Place(P2, BLRH, Tile(I0, I2)),
      Pass(P2, WLQH),
      Place(P1, WLQH, Tile(I0, I3)),
      Pass(P1, WLQF)))
  }

  it should "fail to place a piece after placing a winning piece" in {
    expectError(GameOverError)(List(
      Pass(P1, WLQF),
      Place(P2, WLQF, Tile(I0, I0)),
      Pass(P2, BLQF),
      Place(P1, BLQF, Tile(I0, I1)),
      Pass(P1, BLRH),
      Place(P2, BLRH, Tile(I0, I2)),
      Pass(P2, WLQH),
      Place(P1, WLQH, Tile(I0, I3)),
      Place(P1, BLQH, Tile(I3, I3))))
  }

  it should "accept when all pieces are played and the last piece wins" in {
    assertWin(List(
      Pass(P1, BLRF),
      Place(P2, BLRF, Tile(I1, I1)),
      Pass(P2, BLRH),
      Place(P1, BLRH, Tile(I1, I3)),
      Pass(P1, BLQF),
      Place(P2, BLQF, Tile(I0, I0)),
      Pass(P2, BLQH),
      Place(P1, BLQH, Tile(I2, I0)),
      Pass(P1, BSRF),
      Place(P2, BSRF, Tile(I3, I2)),
      Pass(P2, BSRH),
      Place(P1, BSRH, Tile(I2, I1)),
      Pass(P1, BSQF),
      Place(P2, BSQF, Tile(I1, I0)),
      Pass(P2, BSQH),
      Place(P1, BSQH, Tile(I0, I1)),
      Pass(P1, WLQF),
      Place(P2, WLQF, Tile(I3, I3)),
      Pass(P2, WLRH),
      Place(P1, WLRH, Tile(I3, I1)),
      Pass(P1, WLQH),
      Place(P2, WLQH, Tile(I1, I2)),
      Pass(P2, WSRH),
      Place(P1, WSRH, Tile(I0, I2)),
      Pass(P1, WSQF),
      Place(P2, WSQF, Tile(I2, I3)),
      Pass(P2, WSQH),
      Place(P1, WSQH, Tile(I0, I3)),
      Pass(P1, WLRF),
      Place(P2, WLRF, Tile(I3, I0)),
      Pass(P2, WSRF),
      Place(P1, WSRF, Tile(I2, I2))))
  }

  it should "accept when all pieces are played and the last piece does not win" in {
    assertNoWin(List(
      Pass(P1, BLRF),
      Place(P2, BLRF, Tile(I1, I1)),
      Pass(P2, BLRH),
      Place(P1, BLRH, Tile(I1, I3)),
      Pass(P1, BLQF),
      Place(P2, BLQF, Tile(I0, I0)),
      Pass(P2, BLQH),
      Place(P1, BLQH, Tile(I2, I0)),
      Pass(P1, BSRF),
      Place(P2, BSRF, Tile(I3, I2)),
      Pass(P2, BSRH),
      Place(P1, BSRH, Tile(I2, I1)),
      Pass(P1, BSQF),
      Place(P2, BSQF, Tile(I1, I0)),
      Pass(P2, BSQH),
      Place(P1, BSQH, Tile(I0, I1)),
      Pass(P1, WLRF),
      Place(P2, WLRF, Tile(I3, I0)),
      Pass(P2, WLRH),
      Place(P1, WLRH, Tile(I3, I1)),
      Pass(P1, WLQF),
      Place(P2, WLQF, Tile(I3, I3)),
      Pass(P2, WLQH),
      Place(P1, WLQH, Tile(I1, I2)),
      Pass(P1, WSRF),
      Place(P2, WSRF, Tile(I2, I3)),
      Pass(P2, WSRH),
      Place(P1, WSRH, Tile(I0, I2)),
      Pass(P1, WSQF),
      Place(P2, WSQF, Tile(I0, I3)),
      Pass(P2, WSQH),
      Place(P1, WSQH, Tile(I2, I2))))
  }

}
