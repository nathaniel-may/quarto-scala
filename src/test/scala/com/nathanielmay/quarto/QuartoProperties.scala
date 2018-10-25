package com.nathanielmay.quarto

//scalacheck
import com.nathanielmay.quarto.Exceptions.InvalidPieceForOpponentError
import org.scalacheck.Properties
import org.scalacheck.Prop.{BooleanOperators, forAll}
import testingUtil.Arbitrarily.{aPiece, aTile, aGame}

object QuartoProperties extends Properties("Quarto") {

  property("fail with active that is already placed") = forAll {
    (t1: Tile, t2: Tile, p1: Piece, p2: Piece) =>
      (t1 != t2 && p1 != p2) ==> (for {
        b <- Board(Map(t1 -> p1, t2 -> p2))
        q <- PlaceQuarto(b, p1)
      } yield q).fold(_ == InvalidPieceForOpponentError, _ => false)
  }

  property("game played with turns and constructed from map are equal") = forAll {
    game: Quarto => (game match {
      case PassQuarto(b)     => PassQuarto(b)
      case PlaceQuarto(b, p) => PlaceQuarto(b, p).get
      case FinalQuarto(b, s) => FinalQuarto(b, s)
    }) == game
  }

}

  //TODO checklist
//  "should be valid without active piece if game is won"
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

