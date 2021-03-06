package com.nathanielmay.quarto

// Scalatest
import org.scalatest.{FlatSpec, Matchers}

// Project
import Exceptions.{GameOverError, InvalidPieceForOpponentError, InvalidPlacementError, OutOfTurnError}
import testingUtil.{Pass, Place}
import testingUtil.Pieces._
import testingUtil.Util._

class QuartoTest extends FlatSpec with Matchers {

  "a Quarto game"  should "empty should equal apply()" in {
    Quarto.empty shouldBe Quarto()
  }

  it  should "fail with active that is already placed" in {
    val tiles = Map(Tile(I1, I2) -> WLQF, Tile(I2, I2) -> BSRH)
    assert(Board(tiles)
      .flatMap { PlaceQuarto(_, WLQF) }
      .failed.get == InvalidPieceForOpponentError)
  }

  it should "should be valid without active piece if game is won" in {
    val tiles = Map(Tile(I0, I0) -> WLQF,
      Tile(I0, I1) -> BLQF,
      Tile(I0, I2) -> BLRH,
      Tile(I0, I3) -> WLQH)
    assert(validQuarto(Board(tiles), None))
  }

  it should "be equal to a game from saved state" in {
    val q1 = takeTurns(List(
      Pass(P1, WLQF),
      Place(P2, Tile(I1, I2)),
      Pass(P2, BLQF),
      Place(P1, Tile(I2, I2))))

    val tiles = Map(
      Tile(I1, I2) -> Piece(White, Large, Square, Flat),
      Tile(I2, I2) -> Piece(Black, Large, Square, Flat)
    )

    val q2 = quartoFrom(Board(tiles))

    assert(q1.isSuccess)
    assert(q2.isSuccess)
    q1.get shouldBe q2.get

  }

  it should "recognize a horizontal win" in {
    assertWin(List(
      Pass(P1, WLQF),
      Place(P2, Tile(I0, I0)),
      Pass(P2, BLQF),
      Place(P1, Tile(I0, I1)),
      Pass(P1, BLRH),
      Place(P2, Tile(I0, I2)),
      Pass(P2, WLQH),
      Place(P1, Tile(I0, I3))))
  }

  it should "recognize a vertical win" in {
    assertWin(List(
      Pass(P1, WLQF),
      Place(P2, Tile(I0, I2)),
      Pass(P2, BLQF),
      Place(P1, Tile(I1 ,I2)),
      Pass(P1, BLRH),
      Place(P2, Tile(I2, I2)),
      Pass(P2, WLQH),
      Place(P1, Tile(I3, I2))))
  }

  it should "recognize a diagonal0 win" in {
    assertWin(List(
      Pass(P1, WLQF),
      Place(P2, Tile(I0, I0)),
      Pass(P2, BLQF),
      Place(P1, Tile(I1, I1)),
      Pass(P1, BLRH),
      Place(P2, Tile(I2, I2)),
      Pass(P2, WLQH),
      Place(P1, Tile(I3, I3))))
  }

  it should "recognize a diagonal1 win" in {
    assertWin(List(
      Pass(P1, WLQF),
      Place(P2, Tile(I3, I0)),
      Pass(P2, BLQF),
      Place(P1, Tile(I2, I1)),
      Pass(P1, BLRH),
      Place(P2, Tile(I1, I2)),
      Pass(P2, WLQH),
      Place(P1, Tile(I0, I3))))
  }

  it should "recognize a multi-line win" in {
    assertWin(List(
      Pass(P1, WLQF),
      Place(P2, Tile(I0, I0)),
      Pass(P2, BLQF),
      Place(P1, Tile(I0, I1)),
      Pass(P1, BLRH),
      Place(P2, Tile(I0, I2)),
      Pass(P2, BSRH),
      Place(P1, Tile(I1, I3)),
      Pass(P1, BSRF),
      Place(P2, Tile(I2, I3)),
      Pass(P2, BLQH),
      Place(P1, Tile(I3, I3)),
      Pass(P1, BLRF),
      Place(P2, Tile(I0, I3))))
  }

  it should "not recognize a new game as won" in {
    assert(!Quarto.isWon(Quarto.empty.board))
  }

  it should "not recognize a game with one placed piece as won" in {
    Quarto.empty
      .passPiece(P1, WLQF)
      .getOrElse(fail())
      .placePiece(P2, Tile(I1, I2))
      .getOrElse(fail()) match {
        case Right(FinalQuarto(_, _)) => fail()
        case _ =>
      }
  }

  "a Turn" should "fail when giving opponent a piece that is already placed" in {
    expectError(InvalidPieceForOpponentError)(List(
      Pass(P1, WLQF),
      Place(P2, Tile(I0, I0)),
      Pass(P2, WLQF)))
  }

  it should "fail when tile is occupied" in {
    expectError(InvalidPlacementError)(List(
      Pass(P1, WLQF),
      Place(P2, Tile(I0, I0)),
      Pass(P2, BLQF),
      Place(P1, Tile(I0, I0))))
  }

  it should "fail when player 2 takes the first turn" in {
    expectError(OutOfTurnError)(List(Pass(P2, WLQF)))
  }

  it should "fail when player 1 tries to take a second turn in a row" in {
    expectError(OutOfTurnError)(List(
      Pass(P1, WLQF),
      Place(P1, Tile(I0, I0)),
      Pass(P1, BLQF)))
  }

  it should "cannot pass a piece after placing a winning piece" in {
    expectError(GameOverError)(List(
      Pass(P1, WLQF),
      Place(P2, Tile(I0, I0)),
      Pass(P2, BLQF),
      Place(P1, Tile(I0, I1)),
      Pass(P1, BLRH),
      Place(P2, Tile(I0, I2)),
      Pass(P2, WLQH),
      Place(P1, Tile(I0, I3)),
      Pass(P1, WLQF)))
  }

  it should "accept when all pieces are played and the last piece wins" in {
    assertWin(List(
      Pass(P1, BLRF),
      Place(P2, Tile(I1, I1)),
      Pass(P2, BLRH),
      Place(P1, Tile(I1, I3)),
      Pass(P1, BLQF),
      Place(P2, Tile(I0, I0)),
      Pass(P2, BLQH),
      Place(P1, Tile(I2, I0)),
      Pass(P1, BSRF),
      Place(P2, Tile(I3, I2)),
      Pass(P2, BSRH),
      Place(P1, Tile(I2, I1)),
      Pass(P1, BSQF),
      Place(P2, Tile(I1, I0)),
      Pass(P2, BSQH),
      Place(P1, Tile(I0, I1)),
      Pass(P1, WLQF),
      Place(P2, Tile(I3, I3)),
      Pass(P2, WLRH),
      Place(P1, Tile(I3, I1)),
      Pass(P1, WLQH),
      Place(P2, Tile(I1, I2)),
      Pass(P2, WSRH),
      Place(P1, Tile(I0, I2)),
      Pass(P1, WSQF),
      Place(P2, Tile(I2, I3)),
      Pass(P2, WSQH),
      Place(P1, Tile(I0, I3)),
      Pass(P1, WLRF),
      Place(P2, Tile(I3, I0)),
      Pass(P2, WSRF),
      Place(P1, Tile(I2, I2))))
  }

  it should "register a tie when all pieces are played and the last piece does not win" in {
    assertTie(List(
      Pass(P1, BLRF),
      Place(P2, Tile(I1, I1)),
      Pass(P2, BLRH),
      Place(P1, Tile(I1, I3)),
      Pass(P1, BLQF),
      Place(P2, Tile(I0, I0)),
      Pass(P2, BLQH),
      Place(P1, Tile(I2, I0)),
      Pass(P1, BSRF),
      Place(P2, Tile(I3, I2)),
      Pass(P2, BSRH),
      Place(P1, Tile(I2, I1)),
      Pass(P1, BSQF),
      Place(P2, Tile(I1, I0)),
      Pass(P2, BSQH),
      Place(P1, Tile(I0, I1)),
      Pass(P1, WLRF),
      Place(P2, Tile(I3, I0)),
      Pass(P2, WLRH),
      Place(P1, Tile(I3, I1)),
      Pass(P1, WLQF),
      Place(P2, Tile(I3, I3)),
      Pass(P2, WLQH),
      Place(P1, Tile(I1, I2)),
      Pass(P1, WSRF),
      Place(P2, Tile(I2, I3)),
      Pass(P2, WSRH),
      Place(P1, Tile(I0, I2)),
      Pass(P1, WSQF),
      Place(P2, Tile(I0, I3)),
      Pass(P2, WSQH),
      Place(P1, Tile(I2, I2))))
  }

}