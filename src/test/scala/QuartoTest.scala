import org.scalatest._
import com.nathanielmay.quarto.quarto.{Piece, Quarto, SquareDoesNotExistError}
import com.nathanielmay.quarto.java.Attribute.Color
import com.nathanielmay.quarto.java.Attribute.Size
import com.nathanielmay.quarto.java.Attribute.Shape
import com.nathanielmay.quarto.java.Attribute.Top

class ExampleSpec extends FlatSpec with Matchers {

  "A Quarto Game" should "not place a piece off the board" in {
    val q = new Quarto("test")

    a [SquareDoesNotExistError] should be thrownBy {
      q.takeTurn(WLQF, (-1,0), BLQF)
    }

    a [SquareDoesNotExistError] should be thrownBy {
      q.takeTurn(WLQF, (0, -1), BLQF)
    }

    a [SquareDoesNotExistError] should be thrownBy {
      q.takeTurn(WLQF, (4,0), BLQF)
    }

    a [SquareDoesNotExistError] should be thrownBy {
      q.takeTurn(WLQF, (0, 4), BLQF)
    }

  }

  //piece declarations
  val WLQF = new Piece(Color.WHITE, Size.LARGE, Shape.ROUND, Top.FLAT)
  val BLQF = new Piece(Color.BLACK, Size.LARGE, Shape.ROUND, Top.FLAT)

}
