package testingUtil

//scala check
import org.scalacheck.Arbitrary
import org.scalacheck.Gen
import org.scalacheck.Gen.{pick, choose, oneOf}

//project
import com.nathanielmay.quarto.{Quarto, Tile, Piece, Color, Shape, Size, Top}
import com.nathanielmay.quarto.{White, Black, Round, Square, Large, Small, Flat, Hole}
import com.nathanielmay.quarto.{I0, I1, I2, I3}
import Pieces._
import Util.{takeTurnsAndStop, getTurns}

object Arbitrarily {
  import Generators._

  case class Q3(game: Quarto)

  implicit val aGame:  Arbitrary[Quarto] = Arbitrary(genAnySizeGame)
  implicit val a3PieceGame: Arbitrary[Q3] = Arbitrary(genGame(3).flatMap(q => Q3(q)))
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

  }

//  //TODO fix z param
//  def sequence[A](l: List[Gen[A]]): Gen[List[A]] =
//    l.foldRight[Gen[List[A]]](Gen.oneOf(List(List())))((x,y) => map2(x,y)(_ :: _))
//
//  def map2[A, B, C](a: Gen[A], b: Gen[B])(f: (A, B) => C): Gen[C] =
//    a.flatMap { x => b.map {y => f(x, y)} }

}


