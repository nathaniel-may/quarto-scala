import org.scalatest._
import org.scalatest.TryValues._
import scala.reflect._
import com.nathanielmay.quarto.quarto._

import scala.util.{Failure, Success}

object QuartoTest {

  def assertWin(turns: List[(Piece, Square, Option[Piece])]): Unit = {
    assert(turnsWon(turns))
  }

  def assertNoWin(turns: List[(Piece, Square, Option[Piece])]): Unit = {
    assert(!turnsWon(turns))
  }

  def turnsWon(turns: List[(Piece, Square, Option[Piece])]): Boolean = {
    Quarto.takeTurns(Quarto.newGame)(turns) match {
      case Success(q) => Quarto.isWon(q)
      case Failure(_) => false
    }
  }

  def assertException[E <: Exception : ClassTag](turns: List[(Piece, Square, Option[Piece])]): Unit = {
    assert(Quarto.takeTurns(Quarto.newGame)(turns) match {
      case Failure(_: E) => true
      case x => false
    })
  }

}

class QuartoTest extends FlatSpec with Matchers {

  "a Quarto game" should "reject turn when active and toPlace are the same" in {
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

  it should "reject turn on invalid board" in {
    val squares = Map(Square(I1, I2) -> WLQF, Square(I2, I2) -> WLQF)
    assert(!Board(squares).isValid)

    val game = Quarto(Board(squares), Some(BSRH))
    assert(!game.isValid)
    game.takeTurn(BSRH, Square(I0,I0), Some(BLRH)).failure.exception shouldBe a [InvalidGameError]
  }

  it should "should be valid without active piece if game is won" in {
    val squares = Map(Square(I0, I0) -> WLQF,
      Square(I0, I1) -> BLQF,
      Square(I0, I2) -> BLRH,
      Square(I0, I3) -> WLQH)
    assert(Quarto(Board(squares), None).isValid)
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
      (WLQH, Square(I1, I2), Some(WSRH)),
      (WSRH, Square(I0, I2), Some(WSQF)),
      (WSQF, Square(I2, I3), Some(WSQH)),
      (WSQH, Square(I0, I3), Some(WLRF)),
      (WLRF, Square(I3, I0), Some(WSRF)),
      (WSRF, Square(I2, I2), None))
    )
  }

  it should "accept when when all pieces are played and the last piece does not win" in {
    QuartoTest.assertNoWin(List(
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
    QuartoTest.assertException[InvalidGameError](List(
      (WLQF, Square(I0, I0), Some(BLQF)),
      (BLQF, Square(I0, I1), Some(BLRH)),
      (BLRH, Square(I0, I2), Some(WLQH)),
      (WLQH, Square(I0, I3), None),
      (BSRH, Square(I0, I0), Some(WSQF)))
    )

  }

  it should "be equal to a game from saved state" in {
    val q1 = Quarto.takeTurns(Quarto.newGame)(List(
      (WLQF, Square(I1, I2), Some(BLQF)),
      (BLQF, Square(I2, I2), Some(BSRH)))
    )

    val squares = Map(Square(I1, I2) -> Piece(White, Large, Squar, Flat),
                      Square(I2, I2) -> Piece(Black, Large, Squar, Flat)
    )

    val q2 = Quarto(Board(squares), Some(Piece(Black, Small, Round, Hole)))

    q1 match {
      case Success(game) => assert(game == q2)
      case Failure(_) => fail()
    }
  }

  it should "should be valid without active piece if board is won" in {
    val squares = Map(Square(I0, I0) -> WLQF,
      Square(I0, I1) -> BLQF,
      Square(I0, I2) -> BLRH,
      Square(I0, I3) -> WLQH)
    assert(Quarto(Board(squares), None).isValid)
  }

  it should "recognize a horizontal win" in {
    QuartoTest.assertWin(List(
      (WLQF, Square(I0, I0), Some(BLQF)),
      (BLQF, Square(I0, I1), Some(BLRH)),
      (BLRH, Square(I0, I2), Some(WLQH)),
      (WLQH, Square(I0, I3), None))
    )
  }


  it should "recognize a vertical win" in {
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

  it should "not recognize a new game as won" in {
    assert(!Quarto.isWon(Quarto.newGame))
  }

  it should "not recognize a game with one placed piece as won" in {
    QuartoTest.assertNoWin(List((WLQF, Square(I1, I2), Some(BLQF))))
  }

  //TODO illegal states are representable (board filled with one kind of piece etc)
  "a Quarto board"  should "reject board creation with active that is already placed" in {
    val squares = Map(Square(I1, I2) -> WLQF, Square(I2, I2) -> BSRH)
    val game = Quarto(Board(squares), Some(WLQF))
    assert(!game.isValid)
    game.takeTurn(WLRF, Square(I0,I0), Some(BLRH)).failure.exception shouldBe an [InvalidGameError]
  }

  it should "be invalid if the same piece is placed twice" in {
    val squares = Map(Square(I1, I2) -> WLQF, Square(I2, I2) -> WLQF)
    assert(!Board(squares).isValid)
  }

  it should "reject board creation without active if board is not new" in {
    val squares = Map(Square(I1, I2) -> WLQF)
    val game = Quarto(Board(squares), None)
    assert(!game.isValid)
    game.takeTurn(WLRF, Square(I0,I0), Some(BLRH)).failure.exception shouldBe an [InvalidGameError]
  }

  "a Quarto piece" should "be equal to a piece with the same attributes" in {
    assert(WLQF == Piece(White, Large, Squar, Flat))
  }

  it should "not be equal to a piece with different attributes" in {
    assert(WLQF != Piece(Black, Large, Squar, Flat))
  }

  //piece declarations
  val WLQF = Piece(White, Large, Squar, Flat)
  val WLQH = Piece(White, Large, Squar, Hole)
  val WLRF = Piece(White, Large, Round, Flat)
  val WLRH = Piece(White, Large, Round, Hole)
  val WSQF = Piece(White, Small, Squar, Flat)
  val WSQH = Piece(White, Small, Squar, Hole)
  val WSRF = Piece(White, Small, Round, Flat)
  val WSRH = Piece(White, Small, Round, Hole)
  val BLQF = Piece(Black, Large, Squar, Flat)
  val BLQH = Piece(Black, Large, Squar, Hole)
  val BLRF = Piece(Black, Large, Round, Flat)
  val BLRH = Piece(Black, Large, Round, Hole)
  val BSQF = Piece(Black, Small, Squar, Flat)
  val BSQH = Piece(Black, Small, Squar, Hole)
  val BSRF = Piece(Black, Small, Round, Flat)
  val BSRH = Piece(Black, Small, Round, Hole)

}
