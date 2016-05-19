import scala.language.implicitConversions

/**
  * Created by casafta on 16/5/2016.
  */

abstract class Cell {
  def isKnown: Boolean
  val isImpossible: Boolean
  def possibleVals: List[Value]
  def removePossibleVal(v: Value): Cell
  def set(v: Int): Cell = Cell(v)
  def value = if (this.isKnown) this.possibleVals.head else new ValueNotKnown()

  override def toString = possibleVals.foldLeft("")((v1, v2) => v1 + " " + v2)
}

case class KnownCell(v: Value) extends Cell {
  override def isKnown: Boolean = true
  def removePossibleVal(v: Value): Cell = if (this.v == v) ImpossibleCell() else this
  override def possibleVals: List[Value] = List(v)

  override val isImpossible: Boolean = false
}

case class EmptyCell() extends Cell {
  override def isKnown = false
  def removePossibleVal(v: Value): Cell = Cell((1 to 9 toList).map(Value(_)).filter(_ != v))
  override def possibleVals: List[Value] = ( 1 to 9 toList).map(Value(_))

  override val isImpossible: Boolean = false
}

case class CellWithVals(vals: List[Value]) extends Cell{
  override def isKnown = false
  def removePossibleVal(v: Value): Cell = Cell(vals.filter(_ != v))
  override def possibleVals: List[Value] = vals

  override val isImpossible: Boolean = false
}

case class ImpossibleCell() extends Cell {
  override def isKnown = false
  def removePossibleVal(v: Value): Cell = new ImpossibleCell
  override def possibleVals: List[Value] = List()

  override val isImpossible: Boolean = true
}

object Cell{
  def apply() = new EmptyCell()
  def apply(v: Value) = new KnownCell(v)
  def apply(v: Int) = new KnownCell(Value(v))
  def apply(vals: List[Value]) = if (vals.length == 1) new KnownCell(vals.head) else new CellWithVals(vals)
  def apply(vals: Int*) = new CellWithVals(vals.toList.map(v => Value(v)))

}

