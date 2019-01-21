object Solution {
  def containsADivisor(i: Long, primes: Vector[Long]): Boolean = {
    val end = math.sqrt(i) + 1
    @annotation.tailrec
    def iter(l: Int): Boolean = {
      if (l >= primes.length) true
      else {
        val x = primes(l)
        if (x > end) true else (if (i % x == 0) false else iter(l + 1))
      }
    }
    iter(0)
  }

  def seive(end: Long): Seq[Long] = {
    val primes = new Array[Int](end.toInt)
    primes.update(0, -1)
    1 until end.toInt foreach {
      i =>
      if (primes(i) != -1) {
        i until end.toInt by i + 1 foreach {
          j => primes.update(j, -1)
        }
        primes.update(i, i + 1)
      }
    }
    (primes filter (_ != -1) map (_.toLong)).toSeq
  }
  
  def isABackwardPrime(primes: Set[Long])(i: Long): Boolean = {
    primes find (_ == i.toString.reverse.toLong) exists (_ != i)
  }

  def backwardsPrime(start: Long, end: Long): String = {
    val primes = seive(Math.pow(10, end.toString.length + 1).toInt)
    (primes dropWhile (_ < start) takeWhile (_ <= end)) filter isABackwardPrime(Set.empty ++ primes) mkString ","
  }
}
