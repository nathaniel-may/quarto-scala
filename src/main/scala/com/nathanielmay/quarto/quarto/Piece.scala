package com.nathanielmay.quarto.quarto

sealed case class Piece(color: Color, size: Size, shape: Shape, top: Top) {
  val attrs = List(color, size, shape, top)
  override def toString: String = "" + color + size + shape + top
}

sealed trait Attribute

trait Color extends Attribute
case object White extends Color { override def toString = "W" }
case object Black extends Color { override def toString = "B" }

trait Size extends Attribute
case object Large extends Size { override def toString = "L" }
case object Small extends Size { override def toString = "S" }

trait Shape extends Attribute
case object Round extends Shape { override def toString = "R" }
case object Squar extends Shape { override def toString = "Q" }

trait Top extends Attribute
case object Flat extends Top { override def toString = "F" }
case object Hole extends Top { override def toString = "H" }

