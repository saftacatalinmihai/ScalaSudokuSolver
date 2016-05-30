import scala.language.postfixOps

def time[R](block: => R): R = {
  val t0 = System.nanoTime()
  val result = block    // call-by-name
  val t1 = System.nanoTime()
  println("Elapsed time: " + (t1 - t0) + " ns")
  result
}

def sumToN(n: Int) = n * (n + 1) / 2
def triangleSum(n: Int) = ( 1 to n).map(sumToN).reduceLeft(_ + _)

def makeFactStream(n: Int, f: Long): Stream[(Int, Long)] = (n,f) #:: makeFactStream(n + 1, f * (n + 1))
def factStream = makeFactStream(1,1)
def fact(n: Int): Long = factStream.take(n).last._2

val squareLen = 20

def makeSirp(aboveRow: List[Long]): Stream[List[Long]] =
  aboveRow #:: makeSirp(1L :: aboveRow.sliding(2).map(_.sum).toList ::: List(1L))

val sirpRow = makeSirp(List(1L,1L)).take(squareLen * 2).last
sirpRow(sirpRow.length / 2)

val n = 20
fact(2 * n) / ( fact(n) * 2 )