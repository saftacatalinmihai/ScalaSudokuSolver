def collatzNext(n: Long): Long =
  if ( n % 2 == 0 ) n / 2
  else 3 * n + 1
//
//var strCache = Map[Long, Stream[Long]]()
//
//def makeSeq(n: Long): Stream[Long] =
//  if ( strCache.contains(n)) strCache(n)
//  else n #:: makeSeq(collatzNext(n))
//
//def strSeqCount(start: Long) : Long = {
//  val seq = makeSeq(start).takeWhile(_ != 1L) ++ List(1L)
//  seq.zip(1 to seq.length).foreach {
//    case (n, i) =>
//      if (!strCache.contains(n)) {
//        strCache += (n -> seq.take(i))
//      }
//  }
//  seq.length
//}
//
//println(
//( 1 to 4).foldLeft((0L, 0L))(
//  (max, next) => {
//    val sc = strSeqCount(next)
//    if (sc > max._2) (next, sc) else max
//  }
//)._1
//)

def collatz(n: Long, count: Int = 1): Int =
  if (n == 1) count
  else collatz( collatzNext(n), count + 1 )

(1 to 1000000)
  .map(n => (n, collatz(n)))
  .reduceLeft((x, y)  => if (x._2 > y._2) x else y)
  ._1
//val r = (1 until 1000000).view.map(n => (n, from(n)))
//  .reduceLeft((a, b) => if (a._2 > b._2) a else b)._1

//assert(r == 837799) // 1 s
//strSeqCount(40)

//var cache = Map[Int, List[Int]]()
//def seqCount(start: Int) : Int = {
//  def seqGen(l: List[Int]): List[Int] =
//    if (cache.contains(l.head)) cache(l.head) ++ l.tail
//    else if (l.head == 1) l
//    else seqGen(collatzNext(l.head) :: l)
//
//  val seq = seqGen(List(start))
//  seq.zip(1 to seq.length).foreach {
//    case (n, i) =>
//      if ( ! cache.contains(n)) {
//        cache += (n -> seq.take(i))
//      }
//  }
//  seq.length
//}
//seqCount(20)
//println(cache)
//seqCount(13)
//memoSeqCount(20)
//println(cache)
