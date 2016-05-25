import scala.language.{postfixOps, implicitConversions}

/**
  * Created by casafta on 16/5/2016.
  */

abstract class Cell {
  def isKnown: Boolean
  val isImpossible: Boolean
  val possibleVals: Set[Value]
  def removePossibleVal(v: Value): Cell
  def set(v: Int): Cell = Cell(v)
  def value = if (this.isKnown) this.possibleVals.head else new ValueNotKnown()

  override def toString = possibleVals.foldLeft("")((v1, v2) => v1 + " " + v2)
}

case class KnownCell(v: Value) extends Cell {
  override def isKnown: Boolean = true
  def removePossibleVal(v: Value): Cell = if (this.v == v) ImpossibleCell() else this
  val possibleVals: Set[Value] = Set(v)

  override val isImpossible: Boolean = false
}

case class EmptyCell() extends Cell {
  override def isKnown = false
  def removePossibleVal(v: Value): Cell = Cell( possibleVals - v toList)  //Cell((1 to 9).filter(Value(_) != v).toList.map(Value(_)))
  val possibleVals: Set[Value] = ( 1 to 9 ).map(Value(_)).toSet

  override val isImpossible: Boolean = false
}

case class CellWithVals(vals: List[Value]) extends Cell{
  override def isKnown = false
  def removePossibleVal(v: Value): Cell = Cell(possibleVals - v toList)
  val possibleVals: Set[Value] = vals.toSet

  override val isImpossible: Boolean = false
}

case class ImpossibleCell() extends Cell {
  override def isKnown = false
  def removePossibleVal(v: Value): Cell = new ImpossibleCell
  val possibleVals: Set[Value] = Set()

  override val isImpossible: Boolean = true
}

object Cell{
  def apply() = new EmptyCell()
  def apply(v: Value) = new KnownCell(v)
  def apply(v: Int) = new KnownCell(Value(v))
  def apply(vals: List[Value]) = if (vals.length == 1) new KnownCell(vals.head) else new CellWithVals(vals)
  def apply(vals: Int*) = new CellWithVals(vals.toList.map(v => Value(v)))

}

