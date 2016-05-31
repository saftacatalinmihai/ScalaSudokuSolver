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

// 20 - 99
(2 to 9).foreach { dec =>
  ( (dec * 10 + 1) to (dec * 10 + 9)).foreach( i => numToStr += (i -> (numToStr(dec * 10) + " " + numToStr(i - dec * 10) )))
}

// 100 - 900 (hundreds)
( 1 to 9 ).foreach( i => numToStr += ( i * 100 -> (numToStr(i) + " hundred")))

// 101 - 999 (hundreds decimals)
(101 to 999).filter(_ % 100 != 0).foreach( i => numToStr += ( i -> (numToStr(i / 100 * 100 ) + " and " + numToStr(i % 100))))

// Calc
(1 to 1000)
  .map(numToStr)
  .map(_.replace(" ", ""))
  .map(_.length)
//  .sum
  .foreach(println)
//numToStr
//( 1 to 1000).map(numToStr).foreach(println)