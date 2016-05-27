/**
  * Created by casafta on 26/5/2016.
  */

object ProjectEuler {

  def projectEuler1 = {
    Stream.from(1).filter(i => i % 3 == 0 || i % 5 == 0).takeWhile(_ < 1000).sum
  }

  def projectEuler2 = {

    def fibSeq(maxTerm: Int, _fibSeq: List[Int] = List(1,1)): List[Int] = {
      val nextTerm =  _fibSeq.take(2).sum
      if ( nextTerm >= maxTerm ) _fibSeq
      else fibSeq(maxTerm, nextTerm :: _fibSeq)
    }

    fibSeq(4000000 - 1).filter(_ % 2 == 0).sum
  }

  def nextPrimeSieve(primes: List[Int]): Int = {
    if (primes.isEmpty) 2
    else {
      def _nextPrime(candidate: Int): Int = {
        if (primes.forall(candidate % _ != 0)) candidate
        else _nextPrime(candidate + 1)
      }
      _nextPrime(primes.head + 1)
    }
  }

  def getPrimes(until: Int) = {

    lazy val primes: Stream[Int] = 2 #:: primes.map(a => {
        Stream.from(a + 1)
          .find(b => primes.takeWhile(c => c * c <= b).forall(b % _ != 0)).get
      }
    )

    primes.takeWhile(_ < until)
  }

  def projectEuler3 = {

    def divide(n: Long, p: Int): Long =
      if (n % p != 0) n
      else divide(n / p, p)

    def largestPrimeFactor(n: Long): Int = {
      def iterate(n: Long, primes: List[Int]): Int = {
         if (n <= primes.head) primes.head
         else if (n % primes.head == 0) iterate(divide(n, primes.head),  nextPrimeSieve(primes) :: primes)
         else iterate(n, nextPrimeSieve(primes) :: primes)
      }

      iterate(n, List(2))
    }
    largestPrimeFactor( 600851475143L )
  }

  def projectEuler4 = {

    def findPal(): Int = {
      val palList = for {
        i <- (100 to 999).reverse
        j <- (100 to 999).reverse
        prod = i * j
        if prod.toString.reverse == prod.toString
      } yield prod

      palList.max
    }

    findPal()
  }

  def projectEuler5 = {
    Stream
      .from(20)
      .filter(n => (1 to 20).forall(n % _ == 0))
      .take(1)
      .head
  }

  def projectEuler6 = {
    val sumOfSq = (1 to 100).map(x => x * x).sum
    val sum = (1 to 100).sum
    val sqOfSum = sum * sum
    sqOfSum - sumOfSq
  }

  def projectEuler7 = {
    (1 to 10000).foldLeft(List(2)){
      (primeList, idx) => nextPrimeSieve(primeList) :: primeList
    }.head
  }

  def projectEuler8 = {
    val longNum = "73167176531330624919225119674426574742355349194934\n96983520312774506326239578318016984801869478851843\n85861560789112949495459501737958331952853208805511\n12540698747158523863050715693290963295227443043557\n66896648950445244523161731856403098711121722383113\n62229893423380308135336276614282806444486645238749\n30358907296290491560440772390713810515859307960866\n70172427121883998797908792274921901699720888093776\n65727333001053367881220235421809751254540594752243\n52584907711670556013604839586446706324415722155397\n53697817977846174064955149290862569321978468622482\n83972241375657056057490261407972968652414535100474\n82166370484403199890008895243450658541227588666881\n16427171479924442928230863465674813919123162824586\n17866458359124566529476545682848912883142607690042\n24219022671055626321111109370544217506941658960408\n07198403850962455444362981230987879927244284909188\n84580156166097919133875499200524063689912560717606\n05886116467109405077541002256983155200055935729725\n71636269561882670428252483600823257530420752963450"
    val chars = longNum.replace("\n","").split("""|\n""").map(_.toLong)
    val slices = chars.sliding(13)
    slices.map(a => a.product ).toList.max
  }

  def projectEuler9 = {
    val m = 1000
    val found = for {
      a <- 0 to m
      b <- a + 1 to m
      c <- b + 1 to m
      if ((a * a) + (b * b)) == c * c && a + b + c == 1000
    } yield (a, b, c)
    found.head._1 * found.head._2 * found.head._3
  }

  def projectEuler10 = {
    getPrimes(2000000).foldLeft(0L)(_+_)
  }

  def projectEuler11 = {

    val grid =
      """08 02 22 97 38 15 00 40 00 75 04 05 07 78 52 12 50 77 91 08
        |49 49 99 40 17 81 18 57 60 87 17 40 98 43 69 48 04 56 62 00
        |81 49 31 73 55 79 14 29 93 71 40 67 53 88 30 03 49 13 36 65
        |52 70 95 23 04 60 11 42 69 24 68 56 01 32 56 71 37 02 36 91
        |22 31 16 71 51 67 63 89 41 92 36 54 22 40 40 28 66 33 13 80
        |24 47 32 60 99 03 45 02 44 75 33 53 78 36 84 20 35 17 12 50
        |32 98 81 28 64 23 67 10 26 38 40 67 59 54 70 66 18 38 64 70
        |67 26 20 68 02 62 12 20 95 63 94 39 63 08 40 91 66 49 94 21
        |24 55 58 05 66 73 99 26 97 17 78 78 96 83 14 88 34 89 63 72
        |21 36 23 09 75 00 76 44 20 45 35 14 00 61 33 97 34 31 33 95
        |78 17 53 28 22 75 31 67 15 94 03 80 04 62 16 14 09 53 56 92
        |16 39 05 42 96 35 31 47 55 58 88 24 00 17 54 24 36 29 85 57
        |86 56 00 48 35 71 89 07 05 44 44 37 44 60 21 58 51 54 17 58
        |19 80 81 68 05 94 47 69 28 73 92 13 86 52 17 77 04 89 55 40
        |04 52 08 83 97 35 99 16 07 97 57 32 16 26 26 79 33 27 98 66
        |88 36 68 87 57 62 20 72 03 46 33 67 46 55 12 32 63 93 53 69
        |04 42 16 73 38 25 39 11 24 94 72 18 08 46 29 32 40 62 76 36
        |20 69 36 41 72 30 23 88 34 62 99 69 82 67 59 85 74 04 36 16
        |20 73 35 29 78 31 90 01 74 31 49 71 48 86 81 16 23 57 05 54
        |01 70 54 71 83 51 54 69 16 92 33 48 61 43 52 01 89 19 67 48""".stripMargin

    val intGrid = grid.split("\\r?\\n").map( l => l.split(" ").map(_.toInt).toList).toList
    val rowLen = intGrid.head.length
    val rowNr = intGrid.length

    val prodsDiagRight = for {
      row <- 0 to rowNr - 4
      col <- 0 to rowLen - 4
      (n1, n2, n3, n4) = ( intGrid(row)(col), intGrid(row + 1)(col + 1), intGrid(row + 2)(col + 2), intGrid(row + 3)(col + 3))
    } yield List(n1, n2, n3, n4).product

    val prodsDiagLeft = for {
      row <- 0 to rowNr - 4
      col <- 0 to rowLen - 4
      (n1, n2, n3, n4) = ( intGrid(row)(col + 3), intGrid(row + 1)(col + 2), intGrid(row + 2)(col + 1), intGrid(row + 3)(col))
    } yield List(n1, n2, n3, n4).product

    val prodsLine = for {
      row <- 0 until rowNr
      col <- 0 to rowLen - 4
      (n1, n2, n3, n4) = ( intGrid(row)(col + 3), intGrid(row)(col + 2), intGrid(row)(col + 1), intGrid(row)(col))
    } yield List(n1, n2, n3, n4).product

    val prodsCols = for {
      row <- 0 to rowNr - 4
      col <- 0 until rowLen
      (n1, n2, n3, n4) = ( intGrid(row)(col), intGrid(row +1 )(col), intGrid(row + 2)(col), intGrid(row + 3)(col))
    } yield List(n1, n2, n3, n4).product

    List(prodsDiagRight.max, prodsDiagLeft.max, prodsLine.max, prodsCols.max)
      .max
  }

  def main(args: Array[String]) {
//    assert(projectEuler1 == 233168)
//    assert(projectEuler2 == 4613732)
//    assert(projectEuler3 == 6857)
//    assert(projectEuler4 == 906609)
//    assert(projectEuler5 == 232792560)
//    assert(projectEuler6 == 25164150)
//    assert(projectEuler7 == 104743)
//    assert(projectEuler8 == 23514624000L)
//    assert(projectEuler9 == 31875000)
//    assert(projectEuler10 == 142913828922L)
//    assert(projectEuler11 == 70600674)

  }
}