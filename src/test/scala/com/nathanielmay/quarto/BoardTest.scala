package com.nathanielmay.quarto

import testingUtil.Pieces.WLQF
import org.scalatest.{FlatSpec, Matchers}

class BoardTest extends FlatSpec with Matchers{

  "a Quarto board" should "remove duplicates if piece is placed twice" in {
    Board(Map(Square(I1, I2) -> WLQF, Square(I2, I2) -> WLQF)).squares.size shouldBe 1
  }

}
