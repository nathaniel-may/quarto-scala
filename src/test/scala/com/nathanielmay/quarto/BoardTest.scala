package com.nathanielmay.quarto

// Scalatest
import org.scalatest.{FlatSpec, Matchers}

// Project
import Exceptions.DuplicatePieceError
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
