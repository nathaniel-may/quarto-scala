package com.nathanielmay.quarto.quarto
import com.nathanielmay.quarto.java.Attribute.Color
import com.nathanielmay.quarto.java.Attribute.Size
import com.nathanielmay.quarto.java.Attribute.Shape
import com.nathanielmay.quarto.java.Attribute.Top
import com.nathanielmay.quarto.java.Line


class Quarto (boardId:String) {

  //constructor
  protected val active:Option[Piece] = None
  protected val squares:Map[(Int, Int), Piece] = Map()
  protected val pieces:Map[Piece, Boolean] = Map()
  protected val lines:Map[Line, Int] = Map()

  def takeTurn (toPlace:Piece, square:(Int, Int), active:Piece): Quarto = {

    if(square._1 < 0 || square._1 > 3 || square._2 < 0 || square._2 > 3){
      throw new SquareDoesNotExistError
    }

    //TODO STUB
    return this
  }

}

class Piece(color: Color, size: Size, shape: Shape, top: Top) {}

class QuartoError extends Exception {}

class SquareDoesNotExistError extends QuartoError {}