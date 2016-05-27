
def factors( n: Int) : List[Int] = {
  n :: ( 1 to n / 2 reverse).filter(n % _ == 0).toList
}

def factorsLong( n: Long) : List[Long] = {
  n :: ( 1L to n / 2 reverse).filter(n % _ == 0).toList
}

Stream
  .from(1)                          // ints
  .scan(0)(_ + _)                   // triangles
  .map(t => t -> factorsLong(t))    // triangles -> factors
  .map( t2 => {println(t2._2.length); t2})
  .filter(_._2.length > 500)
  .take(1)