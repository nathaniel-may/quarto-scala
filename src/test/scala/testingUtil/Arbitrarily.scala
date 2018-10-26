package testingUtil

//scala check
import org.scalacheck.Arbitrary
import org.scalacheck.Gen
import org.scalacheck.Gen.{choose, oneOf, pick}

//project
import com.nathanielmay.quarto.{Quarto, FinalQuarto, PassQuarto, PlaceQuarto, Tile, Piece, Color, Shape, Size, Top}
import com.nathanielmay.quarto.{White, Black, Round, Square, Large, Small, Flat, Hole}
import com.nathanielmay.quarto.{I0, I1, I2, I3}
import Pieces._
import Util.{takeTurn, takeTurnsAndStop, getTurns}

object Arbitrarily {
  import Generators._

  case class Q3(game: Quarto)
  case class Q16(game: Quarto)

  implicit val aGame:  Arbitrary[Quarto] = Arbitrary(genAnySizeGame)
  implicit val a3PieceGame: Arbitrary[Q3] = Arbitrary(genGame(3).flatMap(q => Q3(q)))
  implicit val aCompletedGame: Arbitrary[FinalQuarto] = Arbitrary(genFinalGame)
  implicit val aPiece: Arbitrary[Piece]  = Arbitrary(oneOf(pieceList))
  implicit val aTile:  Arbitrary[Tile]   = Arbitrary(oneOf(tileList))
  implicit val aColor: Arbitrary[Color]  = Arbitrary(oneOf(colors))
  implicit val aShape: Arbitrary[Shape]  = Arbitrary(oneOf(shapes))
  implicit val aSize:  Arbitrary[Size]   = Arbitrary(oneOf(sizes))
  implicit val aTop:   Arbitrary[Top]    = Arbitrary(oneOf(tops))

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
    } yield takeTurnsAndStop()(getTurns(tiles, pieces)).get //todo better way?

    def nextTurns(q: Quarto): List[Turn] = {
      q match {
        case passQ: PassQuarto =>
          pieceList.filterNot(p => passQ.board.contains(p)).map(Pass(passQ.player, _))
        case placeQ: PlaceQuarto =>
          tileList.filterNot(t => placeQ.board.contains(t)).map(Place(placeQ.player, _))
        case _: FinalQuarto => List()
      }
    }

    lazy val genFinalGame: Gen[FinalQuarto] = {
      def go(gen: Either[Gen[Quarto], Gen[FinalQuarto]]): Gen[FinalQuarto] = gen match {
        case Right(end) => end
        case Left(game) => game
          .flatMap(q => oneOf(nextTurns(q)).map(takeTurn(q)))
          .map(_.get) //TODO better way to do this?
          .flatMap {
            case end: FinalQuarto => go(Right(end))
            case game: Quarto => go(Left(game))
          }
        }

      go(Left(Quarto()))
    }

  }

}


