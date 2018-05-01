package com.nathanielmay.quarto.board

import org.scalatest.{FlatSpec, Matchers}
import testingUtil.Pieces.WLQF

class BoardTest extends FlatSpec with Matchers{

  "a Quarto board" should "reject if piece is placed twice" in {
    intercept[Exception] {
      Board(Map(Square(I1, I2) -> WLQF, Square(I2, I2) -> WLQF))
    }
  }

}
