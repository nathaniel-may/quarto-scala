package com.nathanielmay.quarto

// Scalacheck
import Exceptions._
import org.scalacheck.Properties
import org.scalacheck.Prop.{BooleanOperators, forAll}
import testingUtil.Arbs._

// Scala
import scala.util.{Success, Failure}

// Project
import com.nathanielmay.quarto.Exceptions.DuplicatePieceError

object BoardProperties extends Properties("Board"){

  property("cannot place a piece twice via apply") = forAll {
    (piece: Piece, t1: Tile, t2: Tile) => t1 != t2 ==>
      Board(Map(t1 -> piece, t2 -> piece))
        .fold(_ == DuplicatePieceError, _ => false)
  }

  property("placing a piece handles exceptions when not valid") = forAll {
    (p1: Piece, p2: Piece, t1: Tile, t2: Tile) =>
      Board(Map(t1 -> p1)) map { b =>
        b.place(p2, t2) match {
          case Success(_)                     => p1 != p2 && t1 != t2
          case Failure(DuplicatePieceError)   => p1 == p2
          case Failure(InvalidPlacementError) => t1 == t2
          case Failure(_)                     => false // unexpected error
        }
      } fold(_ => false, identity)
  }

}