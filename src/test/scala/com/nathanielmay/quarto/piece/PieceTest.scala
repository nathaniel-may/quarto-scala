package com.nathanielmay.quarto.piece

import testingUtil.Pieces.WLQF
import org.scalatest.{FlatSpec, Matchers}

class PieceTest extends FlatSpec with Matchers{

  "a Quarto piece" should "be equal to a piece with the same attributes" in {
    assert(WLQF == Piece(White, Large, Square, Flat))
  }

  it should "not be equal to a piece with different attributes" in {
    assert(WLQF != Piece(Black, Large, Square, Flat))
  }

}
