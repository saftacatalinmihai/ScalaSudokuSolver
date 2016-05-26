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

  def nextPrime(primes: List[Long] = List()): Long = {
    if (primes.isEmpty) 2
    else {
      def _nextPrime(candidate: Long): Long = {
        if (primes.forall(candidate % _ != 0)) candidate
        else _nextPrime(candidate + 1)
      }
      _nextPrime(primes.head + 1)
    }
  }

  def getPrimes(until: Long) = {
    def sieve(s: Stream[Int]): Stream[Int] =
      s.head #:: sieve(s.tail filter (_ % s.head != 0))

    val primes = sieve(Stream.from(2))

    primes.takeWhile(_ < until).toList
  }

  def projectEuler3 = {

    def divide(n: Long, p: Long): Long =
      if (n % p != 0) n
      else divide(n / p, p)

    def largestPrimeFactor(n: Long): Long = {
      def iterate(n: Long, primes: List[Long]): Long = {
         if (n <= primes.head) primes.head
         else if (n % primes.head == 0) iterate(divide(n, primes.head),  nextPrime(primes) :: primes)
         else iterate(n, nextPrime(primes) :: primes)
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
    (1 to 10000).foldLeft(List(2L)){
      (primeList, idx) => nextPrime(primeList) :: primeList
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
    getPrimes(2000000).sum
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

    println(projectEuler10)


  }
}