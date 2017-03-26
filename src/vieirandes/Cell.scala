package vieirandes

/**
  * Created by lucas on 3/24/17.
  */
case class Cell(pos: Position, value: Int) {
  override def toString: String = Integer.toString(value)

  def manhattan(position: Position): Int = position.manhattan(pos)

  def correctPos(size: Int): Position =
    if (value == 0) Position(size - 1, size -1 )
    else Position((value - 1) % size, (value - 1) / size)

  def belongsTo(size: Int, row: Int, col: Int): (Boolean, Boolean) = {
    val cPos = correctPos(size)
    (cPos.y == row, cPos.x == col)
  }

  def belongsTo(size: Int): (Boolean, Boolean) = belongsTo(size, pos.y, pos.x)
}

