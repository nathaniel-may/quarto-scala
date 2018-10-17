package testingUtil

//scala check
import org.scalacheck.Arbitrary
import org.scalacheck.Gen
import org.scalacheck.Gen.oneOf

//project
import com.nathanielmay.quarto.{Piece, Tile}
import com.nathanielmay.quarto.{I0, I1, I2, I3}
import Pieces._

object Arbitrarily {
  import Generators._

  implicit val aPiece: Arbitrary[Piece] = Arbitrary(oneOf(pieceList))
  implicit val aTile: Arbitrary[Tile] = Arbitrary(tileGen)

}

object Generators {
  val pieceList: List[Piece] =
    List(WLQF, WLQH, WLRF, WLRH, WSQF, WSQH, WSRF, WSRH,
      BLQF, BLQH, BLRF, BLRH, BSQF, BSQH, BSRF, BSRH)

  val indexes = List(I0, I1, I2, I3)

  val tileGen: Gen[Tile] = for {
    h <- oneOf(indexes)
    v <- oneOf(indexes)
  } yield Tile(h, v)

}
