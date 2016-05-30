var numToStr: Map[Int, String] = Map(
  1 -> "one",
  2 -> "two",
  3 -> "three",
  4 -> "for",
  5 -> "five",
  6 -> "six",
  7 -> "seven",
  8 -> "eight",
  9 -> "nine",
  10 -> "ten",
  11 -> "eleven",
  12 -> "twelve",
  13 -> "thirteen",
  14 -> "fourteen",
  15 -> "fifteen",
  16 -> "sixteen",
  17 -> "seventeen",
  18 -> "eighteen",
  19 -> "nineteen",
  20 -> "twenty",
  30 -> "thirty",
  40 -> "forty",
  50 -> "fifty",
  60 -> "sixty",
  70 -> "seventy",
  80 -> "eighty",
  90 -> "ninety",
  1000 -> "one thousand"
)

( 21 to 29).foreach( i => numToStr += (i -> (numToStr(20) + " " + numToStr(i - 20) )))
( 31 to 39).foreach( i => numToStr += (i -> (numToStr(30) + " " + numToStr(i - 30) )))
( 41 to 49).foreach( i => numToStr += (i -> (numToStr(40) + " " + numToStr(i - 40) )))
( 51 to 59).foreach( i => numToStr += (i -> (numToStr(50) + " " + numToStr(i - 50) )))
( 61 to 69).foreach( i => numToStr += (i -> (numToStr(60) + " " + numToStr(i - 60) )))
( 71 to 79).foreach( i => numToStr += (i -> (numToStr(70) + " " + numToStr(i - 70) )))
( 81 to 89).foreach( i => numToStr += (i -> (numToStr(80) + " " + numToStr(i - 80) )))
( 91 to 99).foreach( i => numToStr += (i -> (numToStr(90) + " " + numToStr(i - 90) )))

( 1 to 9 ).foreach( i => numToStr += ( i * 100 -> (numToStr(i) + " hundred")))

(101 to 999).filter(_ % 100 != 0).foreach( i => numToStr += ( i -> (numToStr(i / 100 * 100 ) + " and " + numToStr(i % 100))))

(1 to 1000).map(numToStr(_)).map(_.filter(_ != " ")).map(_.length).sum
numToStr