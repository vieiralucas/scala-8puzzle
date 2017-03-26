package vieirandes

/**
  * Created by lucas on 3/24/17.
  */
case class Position(x: Int, y: Int) {
  def manhattan(pos: Position): Int = Math.abs(pos.x - x) + Math.abs(pos.y - y)

  override def equals(that: Any): Boolean =
    that match {
      case that: Position => that.x == x && that.y == y
      case _ => false
    }


  override def toString: String = "x: " + x + " - y: " + y
}

