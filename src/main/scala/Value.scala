import scala.language.implicitConversions

/**
  * Created by casafta on 16/5/2016.
  */
trait Value

case class KnownValue(v: Int) extends Value {
  require(v >= 1 && v <= 9, s"value: $v should be between 1 and 9")
  override def toString = v.toString
  override def equals(that: Any) = that match {
    case KnownValue(that_val: Int) => v == that_val
    case ValueNotKnown() => false
  }
}
case class ValueNotKnown() extends Value{
  override def toString = "Not known"
  override def equals(that: Any) = that match {
    case KnownValue(that_val: Int) => false
    case ValueNotKnown() => true
  }
}

object Value{
  implicit def intToVal(v: Int): Value = Value(v)
  implicit def valToInt(v: Value): Int = v match {
    case KnownValue(intVal: Int) => intVal
    case ValueNotKnown() => 0
  }
  def apply() = new ValueNotKnown()
  def apply(v: Int) = new KnownValue(v)
}
