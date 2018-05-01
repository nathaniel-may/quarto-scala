package com.nathanielmay.quarto

import com.nathanielmay.quarto.piece.{Piece, White, Black, Large, Small, Round, Square => Sq, Flat, Hole}
import testingUtil.Pieces._
import testingUtil.Util.{takeTurns, assertWin, assertNoWin}
import org.scalatest.{FlatSpec, Matchers}

class QuartoTest extends FlatSpec with Matchers {

  "a Quarto game"  should "reject with active that is already placed" in {
    val squares = Map(Square(I1, I2) -> WLQF, Square(I2, I2) -> BSRH)
    intercept[Exception] {
      Quarto(Board(squares), Some(WLQF))
    }
  }

  it should "should be valid without active piece if game is won" in {
    val squares = Map(Square(I0, I0) -> WLQF,
      Square(I0, I1) -> BLQF,
      Square(I0, I2) -> BLRH,
      Square(I0, I3) -> WLQH)
    Quarto(Board(squares), None)
  }

  it should "reject game creation without active if board is not new" in {
    intercept[Exception] {
      Quarto(Board(Map(Square(I1, I2) -> WLQF)), None)
    }
  }

  it should "be equal to a game from saved state" in {
    val q1 = takeTurns(Quarto())(List(
      (P1, WLQF, Square(I1, I2), Some(BLQF)),
      (P2, BLQF, Square(I2, I2), Some(BSRH)))
    )

    val squares = Map(Square(I1, I2) -> Piece(White, Large, Sq, Flat),
      Square(I2, I2) -> Piece(Black, Large, Sq, Flat)
    )

    val q2 = Quarto(Board(squares), Some(Piece(Black, Small, Round, Hole)))

    assert(q1 == q2)
  }

  it should "should be valid without active piece if board is won" in {
    val squares = Map(Square(I0, I0) -> WLQF,
      Square(I0, I1) -> BLQF,
      Square(I0, I2) -> BLRH,
      Square(I0, I3) -> WLQH)
    Quarto(Board(squares), None)
  }

  it should "recognize a horizontal win" in {
    assertWin(List(
      (P1, WLQF, Square(I0, I0), Some(BLQF)),
      (P2, BLQF, Square(I0, I1), Some(BLRH)),
      (P1, BLRH, Square(I0, I2), Some(WLQH)),
      (P2, WLQH, Square(I0, I3), None))
    )
  }

  it should "recognize a vertical win" in {
    assertWin(List(
      (P1, WLQF, Square(I0, I2), Some(BLQF)),
      (P2, BLQF, Square(I1 ,I2), Some(BLRH)),
      (P1, BLRH, Square(I2, I2), Some(WLQH)),
      (P2, WLQH, Square(I3, I2), None)
    ))
  }

  it should "recognize a diagonal0 win" in {
    assertWin(List(
      (P1, WLQF, Square(I0, I0), Some(BLQF)),
      (P2, BLQF, Square(I1, I1), Some(BLRH)),
      (P1, BLRH, Square(I2, I2), Some(WLQH)),
      (P2, WLQH, Square(I3, I3), None))
    )
  }

  it should "recognize a diagonal1 win" in {
    assertWin(List(
      (P1, WLQF, Square(I3, I0), Some(BLQF)),
      (P2, BLQF, Square(I2, I1), Some(BLRH)),
      (P1, BLRH, Square(I1, I2), Some(WLQH)),
      (P2, WLQH, Square(I0, I3), None))
    )
  }

  it should "recognize a multi-line win" in {
    assertWin(List(
      (P1, WLQF, Square(I0, I0), Some(BLQF)),
      (P2, BLQF, Square(I0, I1), Some(BLRH)),
      (P1, BLRH, Square(I0, I2), Some(BSRH)),

      (P2, BSRH, Square(I1, I3), Some(BSRF)),
      (P1, BSRF, Square(I2, I3), Some(BLQH)),
      (P2, BLQH, Square(I3, I3), Some(BLRF)),

      (P1, BLRF, Square(I0, I3), None))
    )
  }

  it should "not recognize a new game as won" in {
    assert(!Quarto.isWon(Quarto().board))
  }

  it should "not recognize a game with one placed piece as won" in {
    assertNoWin(List((P1, WLQF, Square(I1, I2), Some(BLQF))))
  }

}
