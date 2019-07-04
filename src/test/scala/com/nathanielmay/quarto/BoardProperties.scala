package com.nathanielmay.quarto

//scalacheck
import org.scalacheck.Properties
import org.scalacheck.Prop.{BooleanOperators, forAll}
import testingUtil.Arbs._

//project
import com.nathanielmay.quarto.Exceptions.DuplicatePieceError

object BoardProperties extends Properties("Board"){

  property("cannot place a piece twice") = forAll { (piece: Piece, t1: Tile, t2: Tile) =>
    t1 != t2 ==> Board(Map(t1 -> piece, t2 -> piece)).fold(_ == DuplicatePieceError, _ => false)
  }

  property("cannot occupy a tile twice") = forAll { (p1: Piece, p2: Piece, tile: Tile) =>
    Board(Map(tile -> p1, tile -> p2)).fold(_ => false, b => b.contains(p2) && b.size == 1)
  }

}