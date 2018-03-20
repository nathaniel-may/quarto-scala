import org.scalatest._
import org.scalatest.TryValues._
import com.nathanielmay.quarto.quarto._
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

  "A Quarto Game" should "not place a piece off the board" in {
    val q = Quarto.emptyBoard
    q.takeTurn(WLQF, (-1,0), Some(BLQF)).failure.exception shouldBe a [SquareDoesNotExistError]
    q.takeTurn(WLQF, (0, -1), Some(BLQF)).failure.exception shouldBe a [SquareDoesNotExistError]
    q.takeTurn(WLQF, (4,0), Some(BLQF)).failure.exception shouldBe a [SquareDoesNotExistError]
    q.takeTurn(WLQF, (0, 4), Some(BLQF)).failure.exception shouldBe a [SquareDoesNotExistError]
  }

  it should "reject turn when active and toPlace are the same" in {
    Quarto.emptyBoard.takeTurn(WLQF, (0, 0), Some(WLQF)).failure.exception shouldBe a [BadTurnError]
  }

  it should "reject turn when active piece is already placed" in {
    Quarto.takeTurns(Quarto.emptyBoard)(List(
      (WLQF, (0, 0), Some(BLQF)),
      (BLQF, (1, 0), Some(WLQF))
    )).failure.exception shouldBe a [BadTurnError]
  }

  it should "reject turn when square is occupied" in {
    Quarto.takeTurns(Quarto.emptyBoard)(List(
      (WLQF, (0, 0), Some(BLQF)),
      (BLQF, (0, 0), Some(BSRH))
    )).failure.exception shouldBe a [BadTurnError]
  }

  it should "accept active piece for winning move" in {
    QuartoTest.assertWin(List(
      (WLQF, (0, 0), Some(BLQF)),
      (BLQF, (0, 1), Some(BLRH)),
      (BLRH, (0, 2), Some(WLQH)),
      (WLQH, (0, 3), Some(BSRH)))
    )
  }

  it should "accept active piece of None for winning move" in {
    QuartoTest.assertWin(List(
      (WLQF, (0, 0), Some(BLQF)),
      (BLQF, (0, 1), Some(BLRH)),
      (BLRH, (0, 2), Some(WLQH)),
      (WLQH, (0, 3), None))
    )
  }

  //TODO does the success case work
  it should "accept invalid active piece for winning move but not save it" in {
    Quarto.takeTurns(Quarto.emptyBoard)(List(
      (WLQF, (0, 0), Some(BLQF)),
      (BLQF, (0, 1), Some(BLRH)),
      (BLRH, (0, 2), Some(WLQH)),
      (WLQH, (0, 3), Some(WLQF)))
    ) should matchPattern {
      case Success(game: Quarto) if game.isWon && game.active.isEmpty =>
    }
  }

  it should "reject when game is already won" in {
    Quarto.takeTurns(Quarto.emptyBoard)(List(
      (WLQF, (0, 0), Some(BLQF)),
      (BLQF, (0, 1), Some(BLRH)),
      (BLRH, (0, 2), Some(WLQH)),
      (WLQH, (0, 3), None),
      (BSRH, (0, 0), Some(WSQF)))
    ).failure.exception shouldBe a [BadTurnError]

  }

  it should "be equal to a game from saved state" in {
    val q1 = Quarto.takeTurns(Quarto.emptyBoard)(List(
      (WLQF, (1, 2), Some(BLQF)),
      (BLQF, (2, 2), Some(BSRH)))
    )

    val squares = Map((1, 2) -> new Piece(Color.WHITE, Size.LARGE, Shape.SQUARE, Top.FLAT),
      (2, 2) -> new Piece(Color.BLACK, Size.LARGE, Shape.SQUARE, Top.FLAT)
    )

    val q2 = Quarto.board(squares,
      Some(new Piece(Color.BLACK, Size.SMALL, Shape.ROUND, Top.HOLE))
    )

    assert(q1 == q2)
  }

  it should "reject board creation with piece on the board twice" in {
    val squares = Map((1, 2) -> WLQF, (2, 2) -> WLQF)
    Quarto.board(squares, Some(BSRH)).failure.exception shouldBe an [InvalidBoardError]
  }

  it should "reject board creation with active that is already placed" in {
    val squares = Map((1, 2) -> WLQF, (2, 2) -> BSRH)
    Quarto.board(squares, Some(WLQF)).failure.exception shouldBe an [InvalidBoardError]
  }

  it should "reject board creation with out active if board is not new" in {
    val squares = Map((1, 2) -> WLQF)
    Quarto.board(squares, None).failure.exception shouldBe an [InvalidBoardError]
  }

  it should "not reject board creation with out active if board is won" in {
    val squares = Map((0, 0) -> WLQF, (0, 1) -> BLQF, (0, 2) -> BLRH, (0, 3) -> WLQH)
    Quarto.board(squares, None).failure.exception shouldBe an [InvalidBoardError]
  }

  it should "reject board creation with pieces placed off the board" in {
    val squares1 = Map((4, 2) -> WLQF, (0, 0) -> BSRH)
    Quarto.board(squares1, Some(WLQH)).failure.exception shouldBe an [InvalidBoardError]

    val squares2 = Map((-1, 2) -> WLQF, (0, 0) -> BSRH)
    Quarto.board(squares2, Some(WLQH)).failure.exception shouldBe an [InvalidBoardError]
  }

  it should "recognize a horizontal win" in {
    QuartoTest.assertWin(List(
      (WLQF, (0, 0), Some(BLQF)),
      (BLQF, (0, 1), Some(BLRH)),
      (BLRH, (0, 2), Some(WLQH)),
      (WLQH, (0, 3), None))
    )
  }


  "a quarto" should "recognize a vertical win" in {
    QuartoTest.assertWin(List(
      (WLQF, (0,2), Some(BLQF)),
      (BLQF, (1,2), Some(BLRH)),
      (BLRH, (2,2), Some(WLQH)),
      (WLQH, (3,2), None)
    ))
  }

  it should "recognize a diagonal0 win" in {
    QuartoTest.assertWin(List(
      (WLQF, (0, 0), Some(BLQF)),
      (BLQF, (1, 1), Some(BLRH)),
      (BLRH, (2, 2), Some(WLQH)),
      (WLQH, (3, 3), None))
    )
  }

  it should "recognize a diagonal1 win" in {
    QuartoTest.assertWin(List(
      (WLQF, (3, 0), Some(BLQF)),
      (BLQF, (2, 1), Some(BLRH)),
      (BLRH, (1, 2), Some(WLQH)),
      (WLQH, (0, 3), None))
    )
  }

  it should "recognize a multi-line win" in {
    QuartoTest.assertWin(List(
      (WLQF, (0, 0), Some(BLQF)),
      (BLQF, (0, 1), Some(BLRH)),
      (BLRH, (0, 2), Some(BSRH)),

      (BSRH, (1, 3), Some(BSRF)),
      (BSRF, (2, 3), Some(BLQH)),
      (BLQH, (3, 3), Some(BLRF)),

      (BLRF, (0, 3), None))
    )
  }

  "A Piece" should "be equal to a piece with the same attributes" in {
    assert(WLQF == new Piece(Color.WHITE, Size.LARGE, Shape.SQUARE, Top.FLAT))
  }

  it should "not be equal to a piece with different attributes" in {
    assert(WLQF != new Piece(Color.BLACK, Size.LARGE, Shape.SQUARE, Top.FLAT))
  }

  //piece declarations
  val WLQF = Piece(Color.WHITE, Size.LARGE, Shape.SQUARE, Top.FLAT)
  val WLQH = Piece(Color.WHITE, Size.LARGE, Shape.SQUARE, Top.HOLE)
  val WLRF = Piece(Color.WHITE, Size.LARGE, Shape.ROUND, Top.FLAT)
  val WLRH = Piece(Color.WHITE, Size.LARGE, Shape.ROUND, Top.HOLE)
  val WSQF = Piece(Color.WHITE, Size.SMALL, Shape.SQUARE, Top.FLAT)
  val WSQH = Piece(Color.WHITE, Size.SMALL, Shape.SQUARE, Top.HOLE)
  val WSRF = Piece(Color.WHITE, Size.SMALL, Shape.ROUND, Top.FLAT)
  val WSRH = Piece(Color.WHITE, Size.SMALL, Shape.ROUND, Top.HOLE)
  val BLQF = Piece(Color.BLACK, Size.LARGE, Shape.SQUARE, Top.FLAT)
  val BLQH = Piece(Color.BLACK, Size.LARGE, Shape.SQUARE, Top.HOLE)
  val BLRF = Piece(Color.BLACK, Size.LARGE, Shape.ROUND, Top.FLAT)
  val BLRH = Piece(Color.BLACK, Size.LARGE, Shape.ROUND, Top.HOLE)
  val BSQF = Piece(Color.BLACK, Size.SMALL, Shape.SQUARE, Top.FLAT)
  val BSQH = Piece(Color.BLACK, Size.SMALL, Shape.SQUARE, Top.HOLE)
  val BSRF = Piece(Color.BLACK, Size.SMALL, Shape.ROUND, Top.FLAT)
  val BSRH = Piece(Color.BLACK, Size.SMALL, Shape.ROUND, Top.HOLE)

}
