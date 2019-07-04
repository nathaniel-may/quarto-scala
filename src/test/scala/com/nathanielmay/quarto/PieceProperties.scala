package com.nathanielmay.quarto

//scalacheck
import org.scalacheck.Properties
import org.scalacheck.Prop.{BooleanOperators, forAll}
import testingUtil.Arbs._

object PieceProperties extends Properties("Piece"){

  property("equal to piece with same attributes") = forAll { p: Piece =>
    p == Piece(p.color, p.size, p.shape, p.top)
  }

  property("does not equal piece with different attributes") = forAll {
    (p: Piece, color: Color, size: Size, shape: Shape, top: Top) =>
      (p.color != color || p.size != size || p.shape != shape || p.top != top) ==>
        (p != Piece(color, size, shape, top))
  }

}
