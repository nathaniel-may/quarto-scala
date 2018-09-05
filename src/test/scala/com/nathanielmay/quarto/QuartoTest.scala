package com.nathanielmay.quarto

import com.nathanielmay
import testingUtil.Pieces._
import testingUtil.Util.{assertNoWin, assertWin, failedQuarto, quarto, successQuarto, takeTurns}
import org.scalatest.{FlatSpec, Matchers}

class QuartoTest extends FlatSpec with Matchers {

  "a Quarto game"  should "be None with active that is already placed" in {
    val tiles = Map(Tile(I1, I2) -> WLQF, Tile(I2, I2) -> BSRH)
    assert(failedQuarto(Board(tiles),Some(WLQF)))
  }

  it should "should be valid without active piece if game is won" in {
    val tiles = Map(Tile(I0, I0) -> WLQF,
      Tile(I0, I1) -> BLQF,
      Tile(I0, I2) -> BLRH,
      Tile(I0, I3) -> WLQH)
    assert(successQuarto(Board(tiles), None))
  }

  it should "be None without active if board is not new" in {
    assert(failedQuarto(Board(Map(Tile(I1, I2) -> WLQF)), None))
  }

  it should "be equal to a game from saved state" in {
    val q1 = takeTurns(P1, WLQF)(List(
      (P2, WLQF, Tile(I1, I2), Some(BLQF)),
      (P1, BLQF, Tile(I2, I2), Some(BSRH)))
    )

    val tiles = Map(Tile(I1, I2) -> Piece(White, Large, Square, Flat),
      Tile(I2, I2) -> Piece(Black, Large, Square, Flat)
    )

    val q2 = quarto(Board(tiles), Some(Piece(Black, Small, Round, Hole)))

    assert(q1.isSuccess)
    assert(q2.isSuccess)
    assert(q1.get == q2.get)
    assert(q1 == q2)

  }

  it should "should be valid without active piece if board is won" in {
    val tiles = Map(Tile(I0, I0) -> WLQF,
      Tile(I0, I1) -> BLQF,
      Tile(I0, I2) -> BLRH,
      Tile(I0, I3) -> WLQH)
    assert(successQuarto(nathanielmay.quarto.Board(tiles), None))
  }

  it should "recognize a horizontal win" in {
    assertWin(P1, WLQF)(List(
      (P2, WLQF, Tile(I0, I0), Some(BLQF)),
      (P1, BLQF, Tile(I0, I1), Some(BLRH)),
      (P2, BLRH, Tile(I0, I2), Some(WLQH)),
      (P1, WLQH, Tile(I0, I3), None))
    )
  }

  it should "recognize a vertical win" in {
    assertWin(P1, WLQF)(List(
      (P2, WLQF, Tile(I0, I2), Some(BLQF)),
      (P1, BLQF, Tile(I1 ,I2), Some(BLRH)),
      (P2, BLRH, Tile(I2, I2), Some(WLQH)),
      (P1, WLQH, Tile(I3, I2), None)
    ))
  }

  it should "recognize a diagonal0 win" in {
    assertWin(P1, WLQF)(List(
      (P2, WLQF, Tile(I0, I0), Some(BLQF)),
      (P1, BLQF, Tile(I1, I1), Some(BLRH)),
      (P2, BLRH, Tile(I2, I2), Some(WLQH)),
      (P1, WLQH, Tile(I3, I3), None))
    )
  }

  it should "recognize a diagonal1 win" in {
    assertWin(P1, WLQF)(List(
      (P2, WLQF, Tile(I3, I0), Some(BLQF)),
      (P1, BLQF, Tile(I2, I1), Some(BLRH)),
      (P2, BLRH, Tile(I1, I2), Some(WLQH)),
      (P1, WLQH, Tile(I0, I3), None))
    )
  }

  it should "recognize a multi-line win" in {
    assertWin(P1, WLQF)(List(
      (P2, WLQF, Tile(I0, I0), Some(BLQF)),
      (P1, BLQF, Tile(I0, I1), Some(BLRH)),
      (P2, BLRH, Tile(I0, I2), Some(BSRH)),

      (P1, BSRH, Tile(I1, I3), Some(BSRF)),
      (P2, BSRF, Tile(I2, I3), Some(BLQH)),
      (P1, BLQH, Tile(I3, I3), Some(BLRF)),

      (P2, BLRF, Tile(I0, I3), None))
    )
  }

  it should "not recognize a new game as won" in {
    assert(!Quarto.isWon(Quarto().board))
  }

  it should "not recognize a game with one placed piece as won" in {
    assertNoWin(P1, WLQF)(List((P2, WLQF, Tile(I1, I2), Some(BLQF))))
  }

}
