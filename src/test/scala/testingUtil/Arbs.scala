package testingUtil

// Scalacheck
import org.scalacheck.Arbitrary

// Project
import com.nathanielmay.quarto.{Color, FinalQuarto, Piece, Quarto, Shape, Size, Tile, Top}
import testingUtil.Generators.{colorGen, genAnySizeGame, genFinalGame, pieceGen, shapeGen, sizeGen, tileGen, topGen}

object Arbs {
  implicit val aPiece: Arbitrary[Piece] = Arbitrary(pieceGen)
  implicit val aTile:  Arbitrary[Tile]  = Arbitrary(tileGen)
  implicit val aColor: Arbitrary[Color] = Arbitrary(colorGen)
  implicit val aShape: Arbitrary[Shape] = Arbitrary(shapeGen)
  implicit val aSize:  Arbitrary[Size]  = Arbitrary(sizeGen)
  implicit val aTop:   Arbitrary[Top]   = Arbitrary(topGen)

  implicit val aGame:          Arbitrary[Quarto]      = Arbitrary(genAnySizeGame)
  implicit val aCompletedGame: Arbitrary[FinalQuarto] = Arbitrary(genFinalGame)
}
