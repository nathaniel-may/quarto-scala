package com.nathanielmay.quarto

//scalacheck
import com.nathanielmay.quarto.Exceptions.InvalidPieceForOpponentError
import org.scalacheck.Properties
import org.scalacheck.Prop.{BooleanOperators, forAll}
import testingUtil.Arbitrarily.{aPiece, aTile}

object QuartoProperties extends Properties("Quarto") {

  property("fail with active that is already placed") = forAll {
    (t1: Tile, t2: Tile, p1: Piece, p2: Piece) =>
      (t1 != t2 && p1 != p2) ==> (for {
        b <- Board(Map(t1 -> p1, t2 -> p2))
      } yield PlaceQuarto(b, p1))
        .fold(_ == InvalidPieceForOpponentError, _ => false)
  }

  property("game played with turns and constructed from map are equal") = forAll {
    (t1: Tile, t2: Tile, p1: Piece, p2: Piece) =>
      (t1 != t2 && p1 != p2) ==> (for {
        b <- Board(Map(t1 -> p1, t2 -> p2))
      } yield PlaceQuarto(b, p1))
        .fold(_ == InvalidPieceForOpponentError, _ => false)
  }

}

object m{
  import testingUtil.Pieces._
  def main(args: Array[String]): Unit = {

    Board(Map(Tile(I1,I0) -> WSRH, Tile(I3,I3) -> BSRF))
      .flatMap(b => PlaceQuarto(b, WSRH))
      .fold(e => print(e), _ => print("HOORAY"))
  }
}

  //TODO checklist
//  "should be valid without active piece if game is won"
//  "be equal to a game from saved state"
//  "recognize a horizontal win"
//  "recognize a vertical win"
//  "recognize a diagonal0 win"
//"recognize a diagonal1 win"
//"recognize a multi-line win"
//"not recognize a new game as won"
//"not recognize a game with one placed piece as won"
//
//  "a Turn" should "fail when giving opponent a piece that is already placed"
//"fail when tile is occupied"
//"fail when player 2 takes the first turn"
//"fail when player 1 tries to take a second turn in a row"
//"cannot pass a piece after placing a winning piece"
//"accept when all pieces are played and the last piece wins"
//"register a tie when all pieces are played and the last piece does not win"
//

