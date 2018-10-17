package testingUtil

//scala check
import org.scalacheck.Arbitrary
import org.scalacheck.Gen
import org.scalacheck.Gen.oneOf

//project
import com.nathanielmay.quarto.{Tile, Piece, Color, Shape, Size, Top}
import com.nathanielmay.quarto.{White, Black, Round, Square, Large, Small, Flat, Hole}
import com.nathanielmay.quarto.{I0, I1, I2, I3}
import Pieces._

object Arbitrarily {
  import Generators._

  implicit val aPiece: Arbitrary[Piece] = Arbitrary(oneOf(pieceList))
  implicit val aTile:  Arbitrary[Tile]  = Arbitrary(tileGen)
  implicit val aColor: Arbitrary[Color] = Arbitrary(oneOf(colors))
  implicit val aShape: Arbitrary[Shape] = Arbitrary(oneOf(shapes))
  implicit val aSize:  Arbitrary[Size]  = Arbitrary(oneOf(sizes))
  implicit val aTop:   Arbitrary[Top]   = Arbitrary(oneOf(tops))

}

object Generators {
  val pieceList: List[Piece] =
    List(WLQF, WLQH, WLRF, WLRH, WSQF, WSQH, WSRF, WSRH,
      BLQF, BLQH, BLRF, BLRH, BSQF, BSQH, BSRF, BSRH)

  val indexes = List(I0, I1, I2, I3)
  val colors  = List(White, Black)
  val shapes  = List(Round, Square)
  val sizes   = List(Large, Small)
  val tops    = List(Flat, Hole)

  val tileGen: Gen[Tile] = for {
    h <- oneOf(indexes)
    v <- oneOf(indexes)
  } yield Tile(h, v)

}
