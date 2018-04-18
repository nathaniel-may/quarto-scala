import org.scalatest._
import com.nathanielmay.quarto.quarto._

class QuartoTest extends FlatSpec with Matchers {

  def assertWin(turns: List[(Player, Piece, Square, Option[Piece])]): Unit = {
    assert(turnsWon(turns))
  }

  def assertNoWin(turns: List[(Player, Piece, Square, Option[Piece])]): Unit = {
    assert(!turnsWon(turns))
  }

  def turnsWon(turns: List[(Player, Piece, Square, Option[Piece])]): Boolean = Quarto.isWon(takeTurns(Quarto())(turns).board)

  def takeTurns(q0: Quarto)(turns: List[(Player, Piece, Square, Option[Piece])]): Quarto = {
    turns.foldLeft(q0)({case (game, (player, piece, square, forOpponent)) =>
      Quarto.takeTurn(Turn(game, player, piece, square, forOpponent))
    })
  }

  "a Quarto game"  should "reject with active that is already placed" in {
    val squares = Map(Square(I1, I2) -> WLQF, Square(I2, I2) -> BSRH)
    intercept[Exception] {
      Quarto(Board(squares).getOrElse(fail()), Some(WLQF))
    }
  }

  it should "should be valid without active piece if game is won" in {
    val squares = Map(Square(I0, I0) -> WLQF,
      Square(I0, I1) -> BLQF,
      Square(I0, I2) -> BLRH,
      Square(I0, I3) -> WLQH)
    Quarto(Board(squares).getOrElse(fail()), None)
  }

  it should "reject game creation without active if board is not new" in {
    intercept[Exception] {
      Quarto(Board(Map(Square(I1, I2) -> WLQF)).getOrElse(fail()), None)
    }
  }

  it should "be equal to a game from saved state" in {
    val q1 = takeTurns(Quarto())(List(
      (P1, WLQF, Square(I1, I2), Some(BLQF)),
      (P2, BLQF, Square(I2, I2), Some(BSRH)))
    )

    val squares = Map(Square(I1, I2) -> Piece(White, Large, Squar, Flat),
      Square(I2, I2) -> Piece(Black, Large, Squar, Flat)
    )

    val q2 = Quarto(Board(squares).getOrElse(fail()), Some(Piece(Black, Small, Round, Hole)))

    assert(q1 == q2)
  }

  it should "should be valid without active piece if board is won" in {
    val squares = Map(Square(I0, I0) -> WLQF,
      Square(I0, I1) -> BLQF,
      Square(I0, I2) -> BLRH,
      Square(I0, I3) -> WLQH)
    Quarto(Board(squares).getOrElse(fail()), None)
  }

  it should "recognize a horizontal win" in {
    assertWin(List(
      (P1, WLQF, Square(I0, I0), Some(BLQF)),
      (P2, BLQF, Square(I0, I1), Some(BLRH)),
      (P1, BLRH, Square(I0, I2), Some(WLQH)),
      (P2, WLQH, Square(I0, I3), None))
    )
  }


  it should "recognize a vertical win" in {
    assertWin(List(
      (P1, WLQF, Square(I0, I2), Some(BLQF)),
      (P2, BLQF, Square(I1 ,I2), Some(BLRH)),
      (P1, BLRH, Square(I2, I2), Some(WLQH)),
      (P2, WLQH, Square(I3, I2), None)
    ))
  }

  it should "recognize a diagonal0 win" in {
    assertWin(List(
      (P1, WLQF, Square(I0, I0), Some(BLQF)),
      (P2, BLQF, Square(I1, I1), Some(BLRH)),
      (P1, BLRH, Square(I2, I2), Some(WLQH)),
      (P2, WLQH, Square(I3, I3), None))
    )
  }

  it should "recognize a diagonal1 win" in {
    assertWin(List(
      (P1, WLQF, Square(I3, I0), Some(BLQF)),
      (P2, BLQF, Square(I2, I1), Some(BLRH)),
      (P1, BLRH, Square(I1, I2), Some(WLQH)),
      (P2, WLQH, Square(I0, I3), None))
    )
  }

  it should "recognize a multi-line win" in {
    assertWin(List(
      (P1, WLQF, Square(I0, I0), Some(BLQF)),
      (P2, BLQF, Square(I0, I1), Some(BLRH)),
      (P1, BLRH, Square(I0, I2), Some(BSRH)),

      (P2, BSRH, Square(I1, I3), Some(BSRF)),
      (P1, BSRF, Square(I2, I3), Some(BLQH)),
      (P2, BLQH, Square(I3, I3), Some(BLRF)),

      (P1, BLRF, Square(I0, I3), None))
    )
  }

  it should "not recognize a new game as won" in {
    assert(!Quarto.isWon(Quarto().board))
  }

  it should "not recognize a game with one placed piece as won" in {
    assertNoWin(List((P1, WLQF, Square(I1, I2), Some(BLQF))))
  }

  "a Turn" should "reject when active and toPlace are the same" in {
    intercept[Exception] {
      Turn(Quarto(), P1, WLQF, Square(I0, I0), Some(WLQF))
    }
  }

  it should "reject when active piece is already placed" in {
    intercept[Exception] {
      takeTurns(Quarto())(List(
        (P1, WLQF, Square(I0, I0), Some(BLQF)),
        (P2, BLQF, Square(I1, I0), Some(WLQF))
      ))
    }
  }

  it should "reject when square is occupied" in {
    intercept[Exception] {
      takeTurns(Quarto())(List(
        (P1, WLQF, Square(I0, I0), Some(BLQF)),
        (P2, BLQF, Square(I0, I0), Some(BSRH))
      ))
    }
  }

  it should "accept valid active piece for winning move" in {
    assertWin(List(
      (P1, WLQF, Square(I0, I0), Some(BLQF)),
      (P2, BLQF, Square(I0, I1), Some(BLRH)),
      (P1, BLRH, Square(I0, I2), Some(WLQH)),
      (P2, WLQH, Square(I0, I3), Some(BSRH)))
    )
  }

  it should "accept active piece of None for winning move" in {
    assertWin(List(
      (P1, WLQF, Square(I0, I0), Some(BLQF)),
      (P2, BLQF, Square(I0, I1), Some(BLRH)),
      (P1, BLRH, Square(I0, I2), Some(WLQH)),
      (P2, WLQH, Square(I0, I3), None))
    )
  }

  it should "accept invalid active piece for winning move but not save it" in {
    takeTurns(Quarto())(List(
      (P1, WLQF, Square(I0, I0), Some(BLQF)),
      (P2, BLQF, Square(I0, I1), Some(BLRH)),
      (P1, BLRH, Square(I0, I2), Some(WLQH)),
      (P2, WLQH, Square(I0, I3), Some(WLQF)))
    ) should matchPattern {
      case game: Quarto if Quarto.isWon(game.board) && game.active.isEmpty =>
    }
  }

  it should "accept when all pieces are played and the last piece wins" in {
    assertWin(List(
      (P1, BLRF, Square(I1, I1), Some(BLRH)),
      (P2, BLRH, Square(I1, I3), Some(BLQF)),
      (P1, BLQF, Square(I0, I0), Some(BLQH)),
      (P2, BLQH, Square(I2, I0), Some(BSRF)),
      (P1, BSRF, Square(I3, I2), Some(BSRH)),
      (P2, BSRH, Square(I2, I1), Some(BSQF)),
      (P1, BSQF, Square(I1, I0), Some(BSQH)),
      (P2, BSQH, Square(I0, I1), Some(WLQF)),
      (P1, WLQF, Square(I3, I3), Some(WLRH)),
      (P2, WLRH, Square(I3, I1), Some(WLQH)),
      (P1, WLQH, Square(I1, I2), Some(WSRH)),
      (P2, WSRH, Square(I0, I2), Some(WSQF)),
      (P1, WSQF, Square(I2, I3), Some(WSQH)),
      (P2, WSQH, Square(I0, I3), Some(WLRF)),
      (P1, WLRF, Square(I3, I0), Some(WSRF)),
      (P2, WSRF, Square(I2, I2), None))
    )
  }

  it should "accept when when all pieces are played and the last piece does not win" in {
    assertNoWin(List(
          (P1, BLRF, Square(I1, I1), Some(BLRH)),
          (P2, BLRH, Square(I1, I3), Some(BLQF)),
          (P1, BLQF, Square(I0, I0), Some(BLQH)),
          (P2, BLQH, Square(I2, I0), Some(BSRF)),
          (P1, BSRF, Square(I3, I2), Some(BSRH)),
          (P2, BSRH, Square(I2, I1), Some(BSQF)),
          (P1, BSQF, Square(I1, I0), Some(BSQH)),
          (P2, BSQH, Square(I0, I1), Some(WLRF)),
          (P1, WLRF, Square(I3, I0), Some(WLRH)),
          (P2, WLRH, Square(I3, I1), Some(WLQF)),
          (P1, WLQF, Square(I3, I3), Some(WLQH)),
          (P2, WLQH, Square(I1, I2), Some(WSRF)),
          (P1, WSRF, Square(I2, I3), Some(WSRH)),
          (P2, WSRH, Square(I0, I2), Some(WSQF)),
          (P1, WSQF, Square(I0, I3), Some(WSQH)),
          (P2, WSQH, Square(I2, I2), None))
    )
  }

  it should "reject when game is already won" in {
    intercept[Exception] {
      takeTurns(Quarto())(List(
        (P1, WLQF, Square(I0, I0), Some(BLQF)),
        (P2, BLQF, Square(I0, I1), Some(BLRH)),
        (P1, BLRH, Square(I0, I2), Some(WLQH)),
        (P2, WLQH, Square(I0, I3), None),
        (P1, BSRH, Square(I0, I0), Some(WSQF)))
      )
    }
  }

  "a Quarto board" should "be invalid if the same piece is placed twice" in {
    intercept[Exception] {
      Board(Map(Square(I1, I2) -> WLQF, Square(I2, I2) -> WLQF))
    }
  }

  "a Quarto piece" should "be equal to a piece with the same attributes" in {
    assert(WLQF == Piece(White, Large, Squar, Flat))
  }

  it should "not be equal to a piece with different attributes" in {
    assert(WLQF != Piece(Black, Large, Squar, Flat))
  }

  //piece declarations TODO fix squar vs square name collision
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
