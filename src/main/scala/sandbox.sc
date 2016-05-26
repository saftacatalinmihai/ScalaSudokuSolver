def sieve(s: Stream[Int]): Stream[Int] =
  s.head #:: sieve(s.tail filter (_ % s.head != 0))

val primes = sieve(Stream.from(2))                     //> primes  : Stream[Int] = Stream(2, ?)

primes.take(10).toList                          //> res1: List[Int] = List(2, 3, 5, 7, 11, 13, 17, 19, 23, 29)