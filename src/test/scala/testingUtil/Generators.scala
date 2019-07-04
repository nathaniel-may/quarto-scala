package testingUtil

// Scalacheck
import org.scalacheck.Arbitrary, Arbitrary.arbBool
import org.scalacheck.Gen
import org.scalacheck.Gen.{choose, oneOf, pick}

// Scala
import scala.util.{Success, Failure}

// Project
import com.nathanielmay.quarto._
import Pieces._
import Util._

object Generators {

  private val pieces: List[Piece] =
    List(WLQF, WLQH, WLRF, WLRH, WSQF, WSQH, WSRF, WSRH,
         BLQF, BLQH, BLRF, BLRH, BSQF, BSQH, BSRF, BSRH)

  private val indexes = List(I0, I1, I2, I3)
  private val colors  = List(White, Black)
  private val shapes  = List(Round, Square)
  private val sizes   = List(Large, Small)
  private val tops    = List(Flat, Hole)

  private val tiles: List[Tile] = for {
    h <- indexes
    v <- indexes
  } yield Tile(h, v)

  val pieceGen: Gen[Piece] = oneOf(pieces)
  val indexGen: Gen[Index] = oneOf(indexes)
  val colorGen: Gen[Color] = oneOf(colors)
  val shapeGen: Gen[Shape] = oneOf(shapes)
  val sizeGen:  Gen[Size]  = oneOf(sizes)
  val topGen:   Gen[Top]   = oneOf(tops)
  val tileGen:  Gen[Tile]  = oneOf(tiles)

  val genAnySizeGame: Gen[Quarto] =
    choose(0, pieces.size).flatMap(n => genGame(n))

  def genGame(n: Int): Gen[Quarto] = for {
    turns  <- choose(0, n) // takeTurnsAndStop handles n > 16
    tiles  <- pick(turns, tiles) map { _.toList }
    pieces <- pick(turns, pieces) map { _.toList }
    pass   <- arbBool.arbitrary
    raw    =  getTurns(tiles, pieces)
    turns  =  if (pass || raw.isEmpty) raw else raw.init
  } yield takeTurnsAndStop(turns)

  def nextTurns(q: Quarto): Stream[Turn] = {
    q match {
      case passQ: PassQuarto =>
        pieces.toStream
          .filterNot(passQ.board.contains)
          .map { Pass(passQ.player, _) }

      case placeQ: PlaceQuarto =>
        tiles.toStream
          .filterNot(placeQ.board.contains)
          .map { Place(placeQ.player, _) }

      case _: FinalQuarto => Stream.empty
    }
  }

  val genFinalGame: Gen[FinalQuarto] = {
    def go(game: Gen[Quarto]): Gen[FinalQuarto] =
      game.flatMap { start =>
        oneOf(nextTurns(start)
          .map { takeTurn(_, start) }
          .flatMap {
            case Success(g) => Stream(g)
            case Failure(_) => Stream.empty
          }
        ) flatMap {
          case end: FinalQuarto => end
          case g                => go(g)
        }
      }

    go(Gen.const(Quarto.empty))
  }

}


