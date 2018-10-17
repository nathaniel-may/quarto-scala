package com.nathanielmay.quarto

import com.nathanielmay.quarto.Exceptions.DuplicatePieceError
import org.scalatest.{FlatSpec, Matchers}
import testingUtil.Pieces.{WLQF, BLQF}

class BoardTest extends FlatSpec with Matchers{

  "a Quarto board" should "fail if piece is placed twice" in {
    Board(Map(Tile(I1, I2) -> WLQF, Tile(I2, I2) -> WLQF))
      .fold(_ == DuplicatePieceError, _ => false)
  }

  it should "ignore the first piece when tile is used twice" in {
    Board(Map(Tile(I2, I2) -> WLQF, Tile(I2, I2) -> BLQF))
      .fold(_ => false, b => b.contains(BLQF) && b.size == 1)
  }

}
