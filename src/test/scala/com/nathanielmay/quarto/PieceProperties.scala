package com.nathanielmay.quarto

//scalacheck
import org.scalacheck.Properties
import org.scalacheck.Prop.{BooleanOperators, forAll}
import testingUtil.Arbitrarily.{aPiece, aTile}

object PieceProperties extends Properties("Piece"){

  property("equal to piece with same attributes") = forAll { p: Piece =>
    p == Piece(p.color, p.size, p.shape, p.top)
  }
  
//  it should "not be equal to a piece with different attributes" in {
//    assert(WLQF != Piece(Black, Large, Square, Flat))
//  }

}
