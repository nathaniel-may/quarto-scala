package testingUtil

import com.nathanielmay.quarto.Piece
import com.nathanielmay.quarto.{White, Black, Large, Small, Square, Round, Flat, Hole}

object Pieces {

  val WLQF = Piece(White, Large, Square, Flat)
  val WLQH = Piece(White, Large, Square, Hole)
  val WLRF = Piece(White, Large, Round,  Flat)
  val WLRH = Piece(White, Large, Round,  Hole)
  val WSQF = Piece(White, Small, Square, Flat)
  val WSQH = Piece(White, Small, Square, Hole)
  val WSRF = Piece(White, Small, Round,  Flat)
  val WSRH = Piece(White, Small, Round,  Hole)
  val BLQF = Piece(Black, Large, Square, Flat)
  val BLQH = Piece(Black, Large, Square, Hole)
  val BLRF = Piece(Black, Large, Round,  Flat)
  val BLRH = Piece(Black, Large, Round,  Hole)
  val BSQF = Piece(Black, Small, Square, Flat)
  val BSQH = Piece(Black, Small, Square, Hole)
  val BSRF = Piece(Black, Small, Round,  Flat)
  val BSRH = Piece(Black, Small, Round,  Hole)

}
