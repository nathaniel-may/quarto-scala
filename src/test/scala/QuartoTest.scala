import org.scalatest._
import com.nathanielmay.quarto.quarto.{Piece, Quarto, SquareDoesNotExistError, BadTurnError}
import com.nathanielmay.quarto.java.{Color, Size, Shape, Top}

class QuartoTest extends FlatSpec with Matchers {

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

  it should "reject when active and toPlace are the same" in {
    a [BadTurnError] should be thrownBy {
      new Quarto("test")
        .takeTurn(WLQF, (0, 0), WLQF)
    }
  }

  it should "reject when active piece is already placed" in {
    val q = new Quarto("test")
      .takeTurn(WLQF, (0, 0), BLQF)

    a [BadTurnError] should be thrownBy {
      q.takeTurn(BLQF, (1, 0), WLQF)
    }
  }

  it should "reject when square is occupied" in {
    val q = new Quarto("test")
      .takeTurn(WLQF, (0, 0), BLQF)

    a [BadTurnError] should be thrownBy {
      q.takeTurn(BLQF, (0, 0), BSRH)
    }
  }

  it should "be equal to a game from saved state" in {
    val q1 = new Quarto("test")
      .takeTurn(WLQF, (1, 2), BLQF)
      .takeTurn(BLQF, (2, 2), BSRH)

    val squares = Map((1, 2) -> new Piece(Color.WHITE, Size.LARGE, Shape.SQUARE, Top.FLAT),
      (2, 2) -> new Piece(Color.BLACK, Size.LARGE, Shape.SQUARE, Top.FLAT)
    )

    val q2 = new Quarto("test",
      squares,
      Some(new Piece(Color.BLACK, Size.SMALL, Shape.ROUND, Top.HOLE))
    )

    assert(q1 == q2)
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

}
