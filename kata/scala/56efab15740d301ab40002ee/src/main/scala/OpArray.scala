object Operarray {
  def gcdi(xx: Long, yy: Long): Long = (for {
    i <- 1L to Math.abs(mini(xx, yy)).toLong
    if (xx % i == 0 && yy % i == 0)
  } yield i).last
  val som = (_:Long) + (_:Long)
  def lcmu(a: Long, b: Long): Long = (Math.abs(a * b)).toLong / gcdi(a, b)
  def maxi(a: Long, b: Long): Long = if (a > b) a else b
  def mini(a: Long, b: Long): Long = if (a < b) a else b
  def operArray(f: (Long, Long) => Long, arr: List[Long], init: Long): List[Long] = arr match {
    case x::xs => {
      val next = f(init, x)
      next :: operArray(f, xs, next)
    }
    case Nil => Nil
  }
}
