package com.nathanielmay.quarto

//scalacheck
import org.scalacheck.Properties
import org.scalacheck.Prop.{BooleanOperators, forAll}
import testingUtil.Arbitrarily.{aTile, aPiece}

//project
import com.nathanielmay.quarto.Exceptions.DuplicatePieceError

object BoardProperties extends Properties("Board"){

  property("decodes reqs") = forAll { (piece: Piece, t1: Tile, t2: Tile) =>
    t1 != t2 ==> Board(Map(t1 -> piece, t2 -> piece)).fold(_ == DuplicatePieceError, _ => false)
  }

}

object m {
  def main(args: Array[String]): Unit = {
    import testingUtil.Pieces._

    println(Board(Map(Tile(I1, I2) -> WLRH, Tile(I0, I0) -> WLRH)).failed.get)
  }
}