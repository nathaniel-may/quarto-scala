import org.scalatest._
import com.nathanielmay.quarto.quarto.{Piece, Quarto, SquareDoesNotExistError, BadTurnError}
import com.nathanielmay.quarto.java.{Color, Size, Shape, Top}

class QuartoTest extends FlatSpec with Matchers {

  "A Quarto Game" should "not place a piece off the board" in {
    val q = new Quarto("test")

    a [SquareDoesNotExistError] should be thrownBy {
      q.takeTurn(WLQF, (-1,0), Some(BLQF))
    }

    a [SquareDoesNotExistError] should be thrownBy {
      q.takeTurn(WLQF, (0, -1), Some(BLQF))
    }

    a [SquareDoesNotExistError] should be thrownBy {
      q.takeTurn(WLQF, (4,0), Some(BLQF))
    }

    a [SquareDoesNotExistError] should be thrownBy {
      q.takeTurn(WLQF, (0, 4), Some(BLQF))
    }

  }

  it should "reject when active and toPlace are the same" in {
    a [BadTurnError] should be thrownBy {
      new Quarto("test")
        .takeTurn(WLQF, (0, 0), Some(WLQF))
    }
  }

  it should "reject when active piece is already placed" in {
    val q = new Quarto("test")
      .takeTurn(WLQF, (0, 0), Some(BLQF))

    a [BadTurnError] should be thrownBy {
      q.takeTurn(BLQF, (1, 0), Some(WLQF))
    }
  }

  it should "reject when square is occupied" in {
    val q = new Quarto("test")
      .takeTurn(WLQF, (0, 0), Some(BLQF))

    a [BadTurnError] should be thrownBy {
      q.takeTurn(BLQF, (0, 0), Some(BSRH))
    }
  }

  it should "be equal to a game from saved state" in {
    val q1 = new Quarto("test")
      .takeTurn(WLQF, (1, 2), Some(BLQF))
      .takeTurn(BLQF, (2, 2), Some(BSRH))

    val squares = Map((1, 2) -> new Piece(Color.WHITE, Size.LARGE, Shape.SQUARE, Top.FLAT),
      (2, 2) -> new Piece(Color.BLACK, Size.LARGE, Shape.SQUARE, Top.FLAT)
    )

    val q2 = new Quarto("test",
      squares,
      Some(new Piece(Color.BLACK, Size.SMALL, Shape.ROUND, Top.HOLE))
    )

    assert(q1 == q2)
  }

  it should "recognize a horizontal win" in {
    val q1 = new Quarto("test")
//      .takeTurn(WLQF, (0, 0), Some(BLQF))
//      .takeTurn(BLQF, (0, 1), Some(BLRH))
//      .takeTurn(BLRH, (0, 2), Some(WLQH))
      .takeTurn(WLQF, (0, 3), None)
  }

  it should "recognize a vertical win" in {

  }

  it should "recognize a diagonal0 win" in {

  }

  it should "recognize a diagonal win" in {

  }

  it should "recognize a multi-line win" in {

  }

  "A Piece" should "be equal to a piece with the same attributes" in {
    assert(WLQF == new Piece(Color.WHITE, Size.LARGE, Shape.SQUARE, Top.FLAT))
  }

  it should "not be equal to a piece with different attributes" in {
    assert(WLQF != new Piece(Color.BLACK, Size.LARGE, Shape.SQUARE, Top.FLAT))
  }

  //piece declarations
  val WLQF = new Piece(Color.WHITE, Size.LARGE, Shape.SQUARE, Top.FLAT)
  val BLQF = new Piece(Color.BLACK, Size.LARGE, Shape.SQUARE, Top.FLAT)
  val BSRH = new Piece(Color.BLACK, Size.SMALL, Shape.ROUND, Top.HOLE)
  val BLRH = new Piece(Color.BLACK, Size.LARGE, Shape.ROUND, Top.HOLE)
    val WLQH = new Piece(Color.BLACK, Size.LARGE, Shape.SQUARE, Top.HOLE)

}
