package com.nathanielmay.quarto

import com.nathanielmay.quarto.Exceptions.InvalidPlacementError
import org.scalatest.{FlatSpec, Matchers}
import testingUtil.Pieces.WLQF

class BoardTest extends FlatSpec with Matchers{

  "a Quarto board" should "fail if piece is placed twice" in {
    Board(Map(Tile(I1, I2) -> WLQF, Tile(I2, I2) -> WLQF))
      .failed.get == InvalidPlacementError
  }

}
