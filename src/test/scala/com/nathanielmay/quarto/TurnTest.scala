package com.nathanielmay.quarto

import com.nathanielmay.quarto.board.{Square, I0, I1, I2, I3}
import testingUtil.Pieces._
import testingUtil.Util.{assertNoWin, assertWin, takeTurns}
import org.scalatest.{FlatSpec, Matchers}

class TurnTest extends FlatSpec with Matchers {

  "a Turn" should "reject when active and toPlace are the same" in {
    intercept[Exception] {
      Turn(Quarto(), P1, WLQF, Square(I0, I0), Some(WLQF))
    }
  }

  it should "reject when active piece is already placed" in {
    intercept[Exception] {
      takeTurns(Quarto())(List(
        (P1, WLQF, Square(I0, I0), Some(BLQF)),
        (P2, BLQF, Square(I1, I0), Some(WLQF))
      ))
    }
  }

  it should "reject when square is occupied" in {
    intercept[Exception] {
      takeTurns(Quarto())(List(
        (P1, WLQF, Square(I0, I0), Some(BLQF)),
        (P2, BLQF, Square(I0, I0), Some(BSRH))
      ))
    }
  }

  it should "accept valid active piece for winning move" in {
    assertWin(List(
      (P1, WLQF, Square(I0, I0), Some(BLQF)),
      (P2, BLQF, Square(I0, I1), Some(BLRH)),
      (P1, BLRH, Square(I0, I2), Some(WLQH)),
      (P2, WLQH, Square(I0, I3), Some(BSRH)))
    )
  }

  it should "accept active piece of None for winning move" in {
    assertWin(List(
      (P1, WLQF, Square(I0, I0), Some(BLQF)),
      (P2, BLQF, Square(I0, I1), Some(BLRH)),
      (P1, BLRH, Square(I0, I2), Some(WLQH)),
      (P2, WLQH, Square(I0, I3), None))
    )
  }

  it should "accept invalid active piece for winning move but not save it" in {
    takeTurns(Quarto())(List(
      (P1, WLQF, Square(I0, I0), Some(BLQF)),
      (P2, BLQF, Square(I0, I1), Some(BLRH)),
      (P1, BLRH, Square(I0, I2), Some(WLQH)),
      (P2, WLQH, Square(I0, I3), Some(WLQF)))
    ) should matchPattern {
      case game: Quarto if Quarto.isWon(game.board) && game.active.isEmpty =>
    }
  }

  it should "accept when all pieces are played and the last piece wins" in {
    assertWin(List(
      (P1, BLRF, Square(I1, I1), Some(BLRH)),
      (P2, BLRH, Square(I1, I3), Some(BLQF)),
      (P1, BLQF, Square(I0, I0), Some(BLQH)),
      (P2, BLQH, Square(I2, I0), Some(BSRF)),
      (P1, BSRF, Square(I3, I2), Some(BSRH)),
      (P2, BSRH, Square(I2, I1), Some(BSQF)),
      (P1, BSQF, Square(I1, I0), Some(BSQH)),
      (P2, BSQH, Square(I0, I1), Some(WLQF)),
      (P1, WLQF, Square(I3, I3), Some(WLRH)),
      (P2, WLRH, Square(I3, I1), Some(WLQH)),
      (P1, WLQH, Square(I1, I2), Some(WSRH)),
      (P2, WSRH, Square(I0, I2), Some(WSQF)),
      (P1, WSQF, Square(I2, I3), Some(WSQH)),
      (P2, WSQH, Square(I0, I3), Some(WLRF)),
      (P1, WLRF, Square(I3, I0), Some(WSRF)),
      (P2, WSRF, Square(I2, I2), None))
    )
  }

  it should "accept when all pieces are played and the last piece does not win" in {
    assertNoWin(List(
      (P1, BLRF, Square(I1, I1), Some(BLRH)),
      (P2, BLRH, Square(I1, I3), Some(BLQF)),
      (P1, BLQF, Square(I0, I0), Some(BLQH)),
      (P2, BLQH, Square(I2, I0), Some(BSRF)),
      (P1, BSRF, Square(I3, I2), Some(BSRH)),
      (P2, BSRH, Square(I2, I1), Some(BSQF)),
      (P1, BSQF, Square(I1, I0), Some(BSQH)),
      (P2, BSQH, Square(I0, I1), Some(WLRF)),
      (P1, WLRF, Square(I3, I0), Some(WLRH)),
      (P2, WLRH, Square(I3, I1), Some(WLQF)),
      (P1, WLQF, Square(I3, I3), Some(WLQH)),
      (P2, WLQH, Square(I1, I2), Some(WSRF)),
      (P1, WSRF, Square(I2, I3), Some(WSRH)),
      (P2, WSRH, Square(I0, I2), Some(WSQF)),
      (P1, WSQF, Square(I0, I3), Some(WSQH)),
      (P2, WSQH, Square(I2, I2), None))
    )
  }

  it should "reject when game is already won" in {
    intercept[Exception] {
      takeTurns(Quarto())(List(
        (P1, WLQF, Square(I0, I0), Some(BLQF)),
        (P2, BLQF, Square(I0, I1), Some(BLRH)),
        (P1, BLRH, Square(I0, I2), Some(WLQH)),
        (P2, WLQH, Square(I0, I3), None),
        (P1, BSRH, Square(I0, I0), Some(WSQF)))
      )
    }
  }

}
