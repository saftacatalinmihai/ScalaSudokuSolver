import scala.io.Source

val filename = "p067_triangle.txt"
val triangle = for (line <- Source.fromFile(filename).getLines()) yield {
  line.split(" ").map(_.toInt).toList
}
//triangle.reverse.reduce(
//  (below, above) => {
//    above.indices.map(
//      i =>
//        if (below(i) > below(i + 1)) above(i) + below(i)
//        else above(i) + below(i + 1)
//    ).toList
//  }
//).head
/*
class Path(val p: List[Int], val headPos: Int){
  val runningSum: Int = p.sum
  val head: Int = p.head
  def append(value: Int, headPos: Int = this.headPos) = {
    assert(headPos >= this.headPos && headPos <= this.headPos + 1)
    new Path(value::p, headPos)
  }
  override def toString = s"Sum: $runningSum , headPos: $headPos , path: $p"
}

(1 until triangle.length - 1)
  .map(triangle(_))
  .foldLeft(List(new Path(triangle.head, 0)))(
    (paths, nextLine) => {
      paths.flatMap( p => {
        List(
          p.append(nextLine(p.headPos)),
          p.append(nextLine(p.headPos + 1), p.headPos + 1))
      })
    })
  .maxBy(_.runningSum)
  .runningSum
  /*