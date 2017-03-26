package vieirandes

import vieirandes.Board.Matrix

/**
  * Created by lucas on 3/24/17.
  */

case class Board(matrix: Matrix, blank: Position) {
  def applySolution(solution: List[Position]): Board = solution.foldLeft(this)((b, step) => b.move(step))

  private def rowToString(row: List[Cell]): String = row.foldLeft("")((str, cell) => str + cell.toString + "|")

  override def toString: String = matrix.foldLeft("")((str, row) => str + rowToString(row) + "\n") + "blank: " + blank + "\n"

  def neighbours(position: Position): List[Cell] = {
    val top = matrix.lift(position.y - 1).flatMap(_.lift(position.x))
    val bot = matrix.lift(position.y + 1).flatMap(_.lift(position.x))
    val left = matrix.lift(position.y).flatMap(_.lift(position.x - 1))
    val right = matrix.lift(position.y).flatMap(_.lift(position.x + 1))

    List(top, bot, left, right).flatten[Cell]
  }

  def getCell(position: Position): Option[Cell] = matrix.lift(position.y).flatMap(_.lift(position.x))

  def swap(a: Cell, b: Cell): Board = {
    val newMatrix = matrix.map(row => row.map(col => {
      if (col.pos == a.pos) {
        a.copy(value = b.value)
      } else if (col.pos == b.pos) {
        b.copy(value = a.value)
      } else {
        col.copy()
      }
    }))

    this.copy(newMatrix)
  }

  def move(position: Position): Board = {
    val n = neighbours(position)
    val blank = n.find(_.value == 0)

    blank match {
      case Some(b) => {
        getCell(position) match {
          case Some(cell) => swap(b, cell).copy(blank = position)
          case None => this
        }
      }
      case None => this
    }
  }

  def move(x: Int, y: Int): Board = move(Position(x, y))

  def win: Boolean = {
    if (blank.equals(Position(2, 2))) {
      val ints = matrix.flatten[Cell].map(_.value)
      val aslist = ints.filterNot(i => i == 0)
      val sorted = ints.filterNot(i => i == 0).sorted

      aslist == sorted
    } else {
      false
    }
  }

  def manhattan(): Int =
    matrix.foldLeft(0)((tVR, rows) => tVR + rows.foldLeft(tVR)((tVC, cell) => {
      val correctPos = cell.correctPos(matrix.size)

      tVC + cell.manhattan(correctPos)
    }))

  def toList: List[Int] = matrix.foldLeft(List[Int]())((list, rows) => rows.foldLeft(list)((inList, col) => inList ++ List(col.value)))

  def linearConflict: Int = {
    matrix.foldLeft(0)((rowAcc, row) => {
      row.foldLeft(rowAcc)((acc, cell) => {
        def walkRight(current: Cell, accConflict: Int): Int = {
          val right = getCell(current.pos.copy(x = current.pos.x + 1))

          right match {
            case None => accConflict
            case Some(r) => {
              val (rBelongsToRow, _) = r.belongsTo(matrix.length)

              val newConflict = if (rBelongsToRow) accConflict + 2 else accConflict

              walkRight(r, newConflict)
            }
          }
        }

        def walkDown(current: Cell, accConflict: Int): Int = {
          val bottom = getCell(current.pos.copy(y = current.pos.y + 1))

          bottom match {
            case None => accConflict
            case Some(b) => {
              val (_, bBelongsToCol) = b.belongsTo(matrix.length)

              val newConflict = if (bBelongsToCol) accConflict + 2 else accConflict

              walkDown(b, newConflict)
            }
          }
        }

        if (cell.correctPos(matrix.length) == cell.pos) acc
        else cell.belongsTo(matrix.length) match {
          case (true, true) => acc + walkRight(cell, 0) + walkDown(cell, 0)
          case (true, false) => acc + walkRight(cell, 0)
          case (false, true) => acc + walkDown(cell, 0)
          case _ => acc
        }
      })
    })
  }
}

object Board {
  type Matrix = List[List[Cell]]
  def reversedBoard: Board = {
    val matrix = List(
      List(Cell(Position(0, 0), 8),Cell(Position(1, 0), 7),Cell(Position(2, 0), 6)),
      List(Cell(Position(0, 1), 5),Cell(Position(1, 1), 4),Cell(Position(2, 1), 3)),
      List(Cell(Position(0, 2), 2),Cell(Position(1, 2), 1),Cell(Position(2, 2), 0))
    )

    Board(matrix, Position(2, 2))
  }
}
