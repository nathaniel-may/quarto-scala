import org.scalatest._
import com.nathanielmay.quarto.quarto.{Quarto, _}
import com.nathanielmay.quarto.java.{Color, Shape, Size, Top}

import scala.util.{Failure, Success}

object QuartoTest {

  def assertWin(turns: List[(Piece, (Int,Int), Option[Piece])]): Unit = {
    assert(Quarto.takeTurns(Quarto.emptyBoard)(turns) match {
      case Success(q) => q.isWon
      case Failure(_) => false
    })
  }

}

class QuartoTest extends FlatSpec with Matchers {
/**
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

  it should "reject turn when active and toPlace are the same" in {
    a [BadTurnError] should be thrownBy {
      new Quarto("test")
        .takeTurn(WLQF, (0, 0), Some(WLQF))
    }
  }

  it should "reject turn when active piece is already placed" in {
    val q = new Quarto("test")
      .takeTurn(WLQF, (0, 0), Some(BLQF))

    a [BadTurnError] should be thrownBy {
      q.takeTurn(BLQF, (1, 0), Some(WLQF))
    }
  }

  it should "reject turn when square is occupied" in {
    val q = new Quarto("test")
      .takeTurn(WLQF, (0, 0), Some(BLQF))

    a [BadTurnError] should be thrownBy {
      q.takeTurn(BLQF, (0, 0), Some(BSRH))
    }
  }

  it should "accept active piece for winning move" in {
    val q = new Quarto("test")
      .takeTurn(WLQF, (0, 0), Some(BLQF))
      .takeTurn(BLQF, (0, 1), Some(BLRH))
      .takeTurn(BLRH, (0, 2), Some(WLQH))
      .takeTurn(WLQH, (0, 3), Some(BSRH))

    assert(q.isWon)
  }

  it should "accept active piece of None for winning move" in {
    val q = new Quarto("test")
      .takeTurn(WLQF, (0, 0), Some(BLQF))
      .takeTurn(BLQF, (0, 1), Some(BLRH))
      .takeTurn(BLRH, (0, 2), Some(WLQH))
      .takeTurn(WLQH, (0, 3), None)

    assert(q.isWon)
  }

  it should "accept invalid active piece for winning move but not save it" in {
    val q = new Quarto("test")
      .takeTurn(WLQF, (0, 0), Some(BLQF))
      .takeTurn(BLQF, (0, 1), Some(BLRH))
      .takeTurn(BLRH, (0, 2), Some(WLQH))
      .takeTurn(WLQH, (0, 3), Some(WLQF))

    assert(q.isWon)
    assert(q.getActive.isEmpty)
  }

  it should "reject when game is already won" in {
    val q = new Quarto("test")
      .takeTurn(WLQF, (0, 0), Some(BLQF))
      .takeTurn(BLQF, (0, 1), Some(BLRH))
      .takeTurn(BLRH, (0, 2), Some(WLQH))
      .takeTurn(WLQH, (0, 3), None)

    a [BadTurnError] should be thrownBy {
      q.takeTurn(BSRH, (0, 0), Some(WSQF))
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

  it should "reject board creation with piece on the board twice" in {

    val squares = Map((1, 2) -> WLQF, (2, 2) -> WLQF)

    a [InvalidBoardError] should be thrownBy {
      new Quarto("test", squares, Some(BSRH))
    }

  }

  it should "reject board creation with active that is already placed" in {

    val squares = Map((1, 2) -> WLQF, (2, 2) -> BSRH)

    a [InvalidBoardError] should be thrownBy {
      new Quarto("test", squares, Some(WLQF))
    }

  }

  it should "reject board creation with out active if board is not new" in {

    val squares = Map((1, 2) -> WLQF)

    a [InvalidBoardError] should be thrownBy {
      new Quarto("test", squares, None)
    }

  }

  it should "not reject board creation with out active if board is won" in {

    val squares = Map((0, 0) -> WLQF, (0, 1) -> BLQF, (0, 2) -> BLRH, (0, 3) -> WLQH)

    a [InvalidBoardError] should be thrownBy {
      new Quarto("test", squares, None)
    }

  }

  it should "reject board creation with pieces placed off the board" in {

    val squares1 = Map((4, 2) -> WLQF, (0, 0) -> BSRH)

    a [InvalidBoardError] should be thrownBy {
      new Quarto("test", squares1, Some(WLQH))
    }

    val squares2 = Map((-1, 2) -> WLQF, (0, 0) -> BSRH)

    a [InvalidBoardError] should be thrownBy {
      new Quarto("test", squares2, Some(WLQH))
    }

  }

  it should "recognize a horizontal win" in {
    val q = new Quarto("test")
      .takeTurn(WLQF, (0, 0), Some(BLQF))
      .takeTurn(BLQF, (0, 1), Some(BLRH))
      .takeTurn(BLRH, (0, 2), Some(WLQH))
      .takeTurn(WLQH, (0, 3), None)

    assert(q.isWon)
  }
*/

  "a quarto" should "recognize a vertical win" in {

    QuartoTest.assertWin(List(
      (WLQF, (0,2), Some(BLQF)),
      (BLQF, (1,2), Some(BLRH)),
      (BLRH, (2,2), Some(WLQH)),
      (WLQH, (3,2), None)
    ))

  }

  /*
  it should "recognize a diagonal0 win" in {
    val q = new Quarto("test")
      .takeTurn(WLQF, (0, 0), Some(BLQF))
      .takeTurn(BLQF, (1, 1), Some(BLRH))
      .takeTurn(BLRH, (2, 2), Some(WLQH))
      .takeTurn(WLQH, (3, 3), None)

    assert(q.isWon)
  }

  it should "recognize a diagonal1 win" in {
    val q = new Quarto("test")
      .takeTurn(WLQF, (3, 0), Some(BLQF))
      .takeTurn(BLQF, (2, 1), Some(BLRH))
      .takeTurn(BLRH, (1, 2), Some(WLQH))
      .takeTurn(WLQH, (0, 3), None)

    assert(q.isWon)
  }

  it should "recognize a multi-line win" in {
    val q = new Quarto("test")
      .takeTurn(WLQF, (0, 0), Some(BLQF))
      .takeTurn(BLQF, (0, 1), Some(BLRH))
      .takeTurn(BLRH, (0, 2), Some(BSRH))

      .takeTurn(BSRH, (1, 3), Some(BSRF))
      .takeTurn(BSRF, (2, 3), Some(BLQH))
      .takeTurn(BLQH, (3, 3), Some(BLRF))

      .takeTurn(BLRF, (0, 3), None)

    assert(q.isWon)
  }
  */

  "A Piece" should "be equal to a piece with the same attributes" in {
    assert(WLQF == new Piece(Color.WHITE, Size.LARGE, Shape.SQUARE, Top.FLAT))
  }

  it should "not be equal to a piece with different attributes" in {
    assert(WLQF != new Piece(Color.BLACK, Size.LARGE, Shape.SQUARE, Top.FLAT))
  }

  //piece declarations
  val WLQF = new Piece(Color.WHITE, Size.LARGE, Shape.SQUARE, Top.FLAT)
  val WLQH = new Piece(Color.WHITE, Size.LARGE, Shape.SQUARE, Top.HOLE)
  val WLRF = new Piece(Color.WHITE, Size.LARGE, Shape.ROUND, Top.FLAT)
  val WLRH = new Piece(Color.WHITE, Size.LARGE, Shape.ROUND, Top.HOLE)
  val WSQF = new Piece(Color.WHITE, Size.SMALL, Shape.SQUARE, Top.FLAT)
  val WSQH = new Piece(Color.WHITE, Size.SMALL, Shape.SQUARE, Top.HOLE)
  val WSRF = new Piece(Color.WHITE, Size.SMALL, Shape.ROUND, Top.FLAT)
  val WSRH = new Piece(Color.WHITE, Size.SMALL, Shape.ROUND, Top.HOLE)
  val BLQF = new Piece(Color.BLACK, Size.LARGE, Shape.SQUARE, Top.FLAT)
  val BLQH = new Piece(Color.BLACK, Size.LARGE, Shape.SQUARE, Top.HOLE)
  val BLRF = new Piece(Color.BLACK, Size.LARGE, Shape.ROUND, Top.FLAT)
  val BLRH = new Piece(Color.BLACK, Size.LARGE, Shape.ROUND, Top.HOLE)
  val BSQF = new Piece(Color.BLACK, Size.SMALL, Shape.SQUARE, Top.FLAT)
  val BSQH = new Piece(Color.BLACK, Size.SMALL, Shape.SQUARE, Top.HOLE)
  val BSRF = new Piece(Color.BLACK, Size.SMALL, Shape.ROUND, Top.FLAT)
  val BSRH = new Piece(Color.BLACK, Size.SMALL, Shape.ROUND, Top.HOLE)

}
