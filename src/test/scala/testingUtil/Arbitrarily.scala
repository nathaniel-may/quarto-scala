package testingUtil

// Scalacheck
import org.scalacheck.Arbitrary, Arbitrary.arbBool
import org.scalacheck.Gen
import org.scalacheck.Gen.{choose, oneOf, pick}

// Scala
import scala.util.{Try, Success, Failure}

// Project
import com.nathanielmay.quarto.{Quarto, FinalQuarto, PassQuarto, PlaceQuarto, Tile, Piece, Color, Shape, Size, Top}
import com.nathanielmay.quarto.{White, Black, Round, Square, Large, Small, Flat, Hole}
import com.nathanielmay.quarto.{I0, I1, I2, I3}
import Pieces._
import Util._

object Arbitrarily {
  import Generators._

  case class Q3(game: Quarto)
  case class Q16(game: Quarto)

  implicit val aPiece: Arbitrary[Piece] = Arbitrary(oneOf(pieceList))
  implicit val aTile:  Arbitrary[Tile]  = Arbitrary(oneOf(tileList))
  implicit val aColor: Arbitrary[Color] = Arbitrary(oneOf(colors))
  implicit val aShape: Arbitrary[Shape] = Arbitrary(oneOf(shapes))
  implicit val aSize:  Arbitrary[Size]  = Arbitrary(oneOf(sizes))
  implicit val aTop:   Arbitrary[Top]   = Arbitrary(oneOf(tops))

  implicit val aGame:          Arbitrary[Quarto]      = Arbitrary(genAnySizeGame)
  implicit val a3PieceGame:    Arbitrary[Q3]          = Arbitrary(genGame(3).map(q => Q3(q)))
  implicit val aCompletedGame: Arbitrary[FinalQuarto] = Arbitrary(genFinalGame)

  private object Generators {
    val pieceList: List[Piece] =
      List(WLQF, WLQH, WLRF, WLRH, WSQF, WSQH, WSRF, WSRH,
           BLQF, BLQH, BLRF, BLRH, BSQF, BSQH, BSRF, BSRH)

    val indexes = List(I0, I1, I2, I3)
    val colors  = List(White, Black)
    val shapes  = List(Round, Square)
    val sizes   = List(Large, Small)
    val tops    = List(Flat, Hole)

    val tileList: List[Tile] = for {
      h <- indexes
      v <- indexes
    } yield Tile(h, v)

    val genAnySizeGame: Gen[Quarto] =
      choose(0, pieceList.size).flatMap(n => genGame(n))

    def genGame(n: Int): Gen[Quarto] = for {
      turns  <- choose(0, n) //takeTurnsAndStop handles n > 16
      tiles  <- pick(turns, tileList) map { _.toList }
      pieces <- pick(turns, pieceList) map { _.toList }
      pass   <- arbBool.arbitrary
      raw    =  getTurns(tiles, pieces)
      turns  =  if (pass || raw.isEmpty) raw else raw.init
    } yield takeTurnsAndStop(turns).get

    def nextTurns(q: Quarto): Stream[Turn] = {
      q match {
        case passQ: PassQuarto =>
          pieceList.toStream
            .filterNot(passQ.board.contains)
            .map { Pass(passQ.player, _) }
        case placeQ: PlaceQuarto =>
          tileList.toStream
            .filterNot(placeQ.board.contains)
            .map { Place(placeQ.player, _) }
        case _: FinalQuarto => Stream.empty
      }
    }

    val genFinalGame: Gen[FinalQuarto] = {
      def go(game: Gen[Quarto]): Gen[FinalQuarto] =
        game.flatMap { start =>
          oneOf(nextTurns(start)
            .map { takeTurn(_, start) }
            .flatMap {
              case Success(g) => Stream(g)
              case Failure(_) => Stream.empty
            }
          ) flatMap {
            case end: FinalQuarto => end
            case g                => go(g)
          }
        }

      go(Gen.const(Quarto.empty))
    }

  }

}


