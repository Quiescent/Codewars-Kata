object DirReduction {
  def dirReduc(arr: Array[String]): Array[String] = {
    def iter(xs: List[String]): List[String] = xs match {
      case "NORTH" :: "SOUTH" :: xt => iter(xt)
      case "SOUTH" :: "NORTH" :: xt => iter(xt)
      case "WEST"  :: "EAST"  :: xt => iter(xt)
      case "EAST"  :: "WEST"  :: xt => iter(xt)
      case x :: xt                  => x :: iter(xt)
      case Nil                      => Nil
    }

    def iterWhileChange(xs: List[String]): List[String] = {
      val next = iter(xs)
      if (next.length == xs.length) xs
      else iterWhileChange(next)
    }

    iterWhileChange(arr.toList).toArray
  }
}

object DirReduction_wrong_interpretation {
    // I misinterpreted the problem :/ I thought that it was total
  // direction reduction, but it's just subsequent useless moves.
  def dirReduc(arr: Array[String]): Array[String] = {
    def followDirections(x: Int, y: Int, xs: List[String]): (Int, Int) = xs match {
      case Nil => (x, y)
      case "NORTH" :: xt => followDirections(x,     y + 1, xt)
      case "SOUTH" :: xt => followDirections(x,     y - 1, xt)
      case "WEST"  :: xt => followDirections(x - 1, y,     xt)
      case "EAST"  :: xt => followDirections(x + 1, y,     xt)
    }

    val (x, y) = followDirections(0, 0, arr.toList)

    val horizantalDirection = if (x < 0) "WEST"  else "EAST"
    val verticalDirection   = if (y < 0) "SOUTH" else "NORTH"

    ((0 until (Math.abs(x).toInt) map (_ => horizantalDirection)) ++
      (0 until (Math.abs(y).toInt) map (_ => verticalDirection))).toArray
  }
}
