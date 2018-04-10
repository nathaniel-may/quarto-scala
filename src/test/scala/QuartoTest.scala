import org.scalatest._
import org.scalatest.TryValues._
import com.nathanielmay.quarto.quarto._
import scala.util.{Failure, Success}

object QuartoTest {

  def assertWin(turns: List[(Piece, Square, Option[Piece])]): Unit = {
    assert(Quarto.takeTurns(Quarto.newGame)(turns) match {
      case Success(q) => Quarto.isWon(q)
      case Failure(_) => false
    })
  }

}

class QuartoTest extends FlatSpec with Matchers {

  it should "reject turn when active and toPlace are the same" in {
    Quarto.newGame
      .takeTurn(WLQF, Square(I0, I0), Some(WLQF)).failure.exception shouldBe a [BadTurnError]
  }

  it should "reject turn when active piece is already placed" in {
    Quarto.takeTurns(Quarto.newGame)(List(
      (WLQF, Square(I0, I0), Some(BLQF)),
      (BLQF, Square(I1, I0), Some(WLQF))
    )).failure.exception shouldBe a [BadTurnError]
  }

  it should "reject turn when square is occupied" in {
    Quarto.takeTurns(Quarto.newGame)(List(
      (WLQF, Square(I0, I0), Some(BLQF)),
      (BLQF, Square(I0, I0), Some(BSRH))
    )).failure.exception shouldBe a [BadTurnError]
  }

  it should "accept active piece for winning move" in {
    QuartoTest.assertWin(List(
      (WLQF, Square(I0, I0), Some(BLQF)),
      (BLQF, Square(I0, I1), Some(BLRH)),
      (BLRH, Square(I0, I2), Some(WLQH)),
      (WLQH, Square(I0, I3), Some(BSRH)))
    )
  }

  it should "accept active piece of None for winning move" in {
    QuartoTest.assertWin(List(
      (WLQF, Square(I0, I0), Some(BLQF)),
      (BLQF, Square(I0, I1), Some(BLRH)),
      (BLRH, Square(I0, I2), Some(WLQH)),
      (WLQH, Square(I0, I3), None))
    )
  }

  it should "accept invalid active piece for winning move but not save it" in {
    Quarto.takeTurns(Quarto.newGame)(List(
      (WLQF, Square(I0, I0), Some(BLQF)),
      (BLQF, Square(I0, I1), Some(BLRH)),
      (BLRH, Square(I0, I2), Some(WLQH)),
      (WLQH, Square(I0, I3), Some(WLQF)))
    ) should matchPattern {
      case Success(game: Quarto) if Quarto.isWon(game) && game.active.isEmpty =>
    }
  }

  it should "accept when all pieces are played and the last piece wins" in {
    QuartoTest.assertWin(List(
      (BLRF, Square(I1, I1), Some(BLRH)),
      (BLRH, Square(I1, I3), Some(BLQF)),
      (BLQF, Square(I0, I0), Some(BLQH)),
      (BLQH, Square(I2, I0), Some(BSRF)),
      (BSRF, Square(I3, I2), Some(BSRH)),
      (BSRH, Square(I2, I1), Some(BSQF)),
      (BSQF, Square(I1, I0), Some(BSQH)),
      (BSQH, Square(I0, I1), Some(WLQF)),
      (WLQF, Square(I3, I3), Some(WLRH)),
      (WLRH, Square(I3, I1), Some(WLQH)),
      (WLQH, Square(I1, I2), Some(WSRF)),
      (WSRF, Square(I2, I2), Some(WSRH)),
      (WSRH, Square(I0, I2), Some(WSQF)),
      (WSQF, Square(I2, I3), Some(WSQH)),
      (WSQH, Square(I0, I3), Some(WLRF)),
      (WLRF, Square(I3, I0), None))
    )
  }

  it should "accept when when all pieces are played and the last piece does not win" in {
    QuartoTest.assertWin(List(
      (BLRF, Square(I1, I1), Some(BLRH)),
      (BLRH, Square(I1, I3), Some(BLQF)),
      (BLQF, Square(I0, I0), Some(BLQH)),
      (BLQH, Square(I2, I0), Some(BSRF)),
      (BSRF, Square(I3, I2), Some(BSRH)),
      (BSRH, Square(I2, I1), Some(BSQF)),
      (BSQF, Square(I1, I0), Some(BSQH)),
      (BSQH, Square(I0, I1), Some(WLRF)),
      (WLRF, Square(I3, I0), Some(WLRH)),
      (WLRH, Square(I3, I1), Some(WLQF)),
      (WLQF, Square(I3, I3), Some(WLQH)),
      (WLQH, Square(I1, I2), Some(WSRF)),
      (WSRF, Square(I2, I2), Some(WSRH)),
      (WSRH, Square(I0, I2), Some(WSQF)),
      (WSQF, Square(I2, I3), Some(WSQH)),
      (WSQH, Square(I0, I3), None))
    )
  }

  it should "reject when game is already won" in {
    Quarto.takeTurns(Quarto.newGame)(List(
      (WLQF, Square(I0, I0), Some(BLQF)),
      (BLQF, Square(I0, I1), Some(BLRH)),
      (BLRH, Square(I0, I2), Some(WLQH)),
      (WLQH, Square(I0, I3), None),
      (BSRH, Square(I0, I0), Some(WSQF)))
    ).failure.exception shouldBe a [BadTurnError]

  }

  it should "be equal to a game from saved state" in {
    val q1 = Quarto.takeTurns(Quarto.newGame)(List(
      (WLQF, Square(I1, I2), Some(BLQF)),
      (BLQF, Square(I2, I2), Some(BSRH)))
    )

    val squares = Map(Square(I1, I2) -> Piece(White, Large, Square, Flat),
                      Square(I2, I2) -> Piece(Black, Large, Square, Flat)
    )

    val q2 = Quarto(Board(squares), Some(Piece(Black, Small, Round, Hole)))

    q1 match {
      case Success(game) => assert(game == q2)
      case Failure(_) => fail()
    }
  }

  //TODO illegal states are representable (board filled with one kind of piece etc)
  it should "reject board creation with piece on the board twice" in {
    val squares = Map(Square(I1, I2) -> WLQF, Square(I2, I2) -> WLQF)
    val game = Quarto(Board(squares), Some(BSRH))
    assert(!game.isValid)
    game.takeTurn(WLRF, Square(I0,I0), Some(BLRH)).failure.exception shouldBe an [InvalidGameError]
  }

  it should "reject board creation with active that is already placed" in {
    val squares = Map(Square(I1, I2) -> WLQF, Square(I2, I2) -> BSRH)
    val game = Quarto(Board(squares), Some(WLQF))
    assert(!game.isValid)
    game.takeTurn(WLRF, Square(I0,I0), Some(BLRH)).failure.exception shouldBe an [InvalidGameError]
  }

  it should "reject board creation with out active if board is not new" in {
    val squares = Map(Square(I1, I2) -> WLQF)
    val game = Quarto(Board(squares), None)
    assert(!game.isValid)
    game.takeTurn(WLRF, Square(I0,I0), Some(BLRH)).failure.exception shouldBe an [InvalidGameError]
  }

  it should "not reject board creation with out active if board is won" in {
    val squares = Map(Square(I0, I0) -> WLQF,
                      Square(I0, I1) -> BLQF,
                      Square(I0, I2) -> BLRH,
                      Square(I0, I3) -> WLQH)
    val game = Quarto(Board(squares), None)
    assert(!game.isValid)
    game.takeTurn(WLRF, Square(I0,I0), Some(BLRH)).failure.exception shouldBe an [InvalidGameError]
  }

  it should "recognize a horizontal win" in {
    QuartoTest.assertWin(List(
      (WLQF, Square(I0, I0), Some(BLQF)),
      (BLQF, Square(I0, I1), Some(BLRH)),
      (BLRH, Square(I0, I2), Some(WLQH)),
      (WLQH, Square(I0, I3), None))
    )
  }


  "a quarto" should "recognize a vertical win" in {
    QuartoTest.assertWin(List(
      (WLQF, Square(I0, I2), Some(BLQF)),
      (BLQF, Square(I1 ,I2), Some(BLRH)),
      (BLRH, Square(I2, I2), Some(WLQH)),
      (WLQH, Square(I3, I2), None)
    ))
  }

  it should "recognize a diagonal0 win" in {
    QuartoTest.assertWin(List(
      (WLQF, Square(I0, I0), Some(BLQF)),
      (BLQF, Square(I1, I1), Some(BLRH)),
      (BLRH, Square(I2, I2), Some(WLQH)),
      (WLQH, Square(I3, I3), None))
    )
  }

  it should "recognize a diagonal1 win" in {
    QuartoTest.assertWin(List(
      (WLQF, Square(I3, I0), Some(BLQF)),
      (BLQF, Square(I2, I1), Some(BLRH)),
      (BLRH, Square(I1, I2), Some(WLQH)),
      (WLQH, Square(I0, I3), None))
    )
  }

  it should "recognize a multi-line win" in {
    QuartoTest.assertWin(List(
      (WLQF, Square(I0, I0), Some(BLQF)),
      (BLQF, Square(I0, I1), Some(BLRH)),
      (BLRH, Square(I0, I2), Some(BSRH)),

      (BSRH, Square(I1, I3), Some(BSRF)),
      (BSRF, Square(I2, I3), Some(BLQH)),
      (BLQH, Square(I3, I3), Some(BLRF)),

      (BLRF, Square(I0, I3), None))
    )
  }

  "A Piece" should "be equal to a piece with the same attributes" in {
    assert(WLQF == Piece(White, Large, Square, Flat))
  }

  it should "not be equal to a piece with different attributes" in {
    assert(WLQF != Piece(Black, Large, Square, Flat))
  }

  //piece declarations
  val WLQF = Piece(White, Large, Square, Flat)
  val WLQH = Piece(White, Large, Square, Hole)
  val WLRF = Piece(White, Large, Round, Flat)
  val WLRH = Piece(White, Large, Round, Hole)
  val WSQF = Piece(White, Small, Square, Flat)
  val WSQH = Piece(White, Small, Square, Hole)
  val WSRF = Piece(White, Small, Round, Flat)
  val WSRH = Piece(White, Small, Round, Hole)
  val BLQF = Piece(Black, Large, Square, Flat)
  val BLQH = Piece(Black, Large, Square, Hole)
  val BLRF = Piece(Black, Large, Round, Flat)
  val BLRH = Piece(Black, Large, Round, Hole)
  val BSQF = Piece(Black, Small, Square, Flat)
  val BSQH = Piece(Black, Small, Square, Hole)
  val BSRF = Piece(Black, Small, Round, Flat)
  val BSRH = Piece(Black, Small, Round, Hole)

}
