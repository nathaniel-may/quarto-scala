package com.nathanielmay.quarto

//scalacheck
import org.scalacheck.Properties
import org.scalacheck.Prop.{BooleanOperators, forAll, exists}

//testing
import testingUtil.Arbitrarily.{a3PieceGame, aGame, aCompletedGame, aPiece, aTile}
import testingUtil.Arbitrarily.Q3
import testingUtil.{Pass, Place, Horizontal, Vertical, Diagonal}
import testingUtil.Util.{testableQuarto, wonWith}

//project
import com.nathanielmay.quarto.Quarto.quarto
import com.nathanielmay.quarto.Exceptions.{InvalidPieceForOpponentError, InvalidPlacementError, OutOfTurnError}


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


  property("a game with less than 4 pieces cannot be won") = forAll {
    q3: Q3 => q3.game match {
      case _: FinalQuarto => false
      case _              => true
    }
  }

  property("player 2 cannot take the first turn") = forAll {
    p: Piece => quarto.passPiece(P2, p)
      .fold(_ == OutOfTurnError, _ => false)
  }

  property("player 1 cannot pass and place") = forAll {
    (t: Tile, p: Piece) => (for {
      q1 <- quarto.passPiece(P1, p)
      q2 <- q1.placePiece(P1, t).map(_.merge)
    } yield q2).fold(_ == OutOfTurnError, _ => false)
  }

  property("player 2 cannot pass and place") = forAll {
    (t1: Tile, t2: Tile, p1: Piece, p2: Piece) =>
      (t1 != t2 && p1 != p2) ==> {
        quarto.takeTurns(List(
          Pass (P1, p1),
          Place(P2, t1),
          Pass (P2, p2),
          Place(P2, t2)))
          .fold(_ == OutOfTurnError, _ => false)
      }
  }

  property("cannot place piece on an occupied tile") = forAll {
    (t: Tile, p1: Piece, p2: Piece) => (p1 != p2) ==>
      quarto.takeTurns(List(
        Pass (P1, p1),
        Place(P2, t),
        Pass (P2, p2),
        Place(P1, t)))
        .fold(_ == InvalidPlacementError, _ => false)
  }

  property("recognizes a diagonal win") = exists {
    game: FinalQuarto => wonWith(game).contains(Diagonal)
  }

  property("recognizes a horizontal win") = exists {
    game: FinalQuarto => wonWith(game).contains(Horizontal)
  }

  property("recognizes a vertical win") = exists {
    game: FinalQuarto => wonWith(game).contains(Vertical)
  }

  property("recognizes a tie game") = exists {
    game: FinalQuarto => game.state == Tie
  }

  property("recognizes a multi-line win") = exists {
    game: FinalQuarto => wonWith(game).size > 1
  }

  property("recognizes a win with the last piece") = exists {
    game: FinalQuarto => game.board.isFull
  }
}

