object Thirteen {
  val xs = Seq(1, 10, 9, 12, 3, 4)

  def go(n: Long): Long = {
    (n.toString.reverse.zipWithIndex map {
      case (c, i) => {
        val digit = c.toString.toInt
        digit * xs(i % xs.length)
      }
    }).sum
  }

  def thirt(n: Long): Long = {
    val next = go(n)
    if (n == next) n
    else thirt(next)
  }
}
