/**
  * Created by casafta on 16/5/2016.
  */
case class Loc(val i: Int, val j: Int) {
  require(i>=0 && i <=9)
  require(j>=0 && j <=9)
  override def toString = s"[$i:$j]"
  override def equals(that: Any) = that match {
    case Loc(that_i, that_j) =>
      if (i == that_i && j == that_j) true else false
  }
}
