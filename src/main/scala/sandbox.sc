val triangle =
  """75
    |95 64
    |17 47 82
    |18 35 87 10
    |20 04 82 47 65
    |19 01 23 75 03 34
    |88 02 77 73 07 63 67
    |99 65 04 28 06 16 70 92
    |41 41 26 56 83 40 80 70 33
    |41 48 72 33 47 32 37 16 94 29
    |53 71 44 65 25 43 91 52 97 51 14
    |70 11 33 28 77 73 17 78 39 68 17 57
    |91 71 52 38 17 14 91 43 58 50 27 29 48
    |63 66 04 68 89 53 67 30 73 16 69 87 40 31
    |04 62 98 27 23 09 70 98 73 93 38 53 60 04 23""".stripMargin.split("\\r?\\n").map(_.split(" ").map(_.toInt).toList)

triangle.reverse.reduce(
  (below, above) => {
    above.indices.map(
      i =>
        if (below(i) > below(i + 1)) above(i) + below(i)
        else above(i) + below(i + 1)
    ).toList
  }
).head
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