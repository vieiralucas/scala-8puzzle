package vieirandes

/**
  * Created by lucas on 3/24/17.
  */
object Main extends App {
  val matrix = List(
    List(Cell(Position(0, 0), 8),Cell(Position(1, 0), 6),Cell(Position(2, 0), 7)),
    List(Cell(Position(0, 1), 2),Cell(Position(1, 1), 5),Cell(Position(2, 1), 4)),
    List(Cell(Position(0, 2), 3),Cell(Position(1, 2), 0),Cell(Position(2, 2), 1))
  )


  /*
  3, 2, 1
  4, 5, 6
  7, 8, 0
  */

  val board = Board(matrix, Position(1, 2))

  var t0 = System.nanoTime()
  val solutionWithConflict = ManhattanWithLinearConflict.search(board)
  var t1 = System.nanoTime()
  println("Elapsed time: " + (t1 - t0) + "ns")

  println(solutionWithConflict.size)
  var solvedConf = board.applySolution(solutionWithConflict)
  println(solvedConf)

  t0 = System.nanoTime()
  val solution = Manhattan.search(board)
  t1 = System.nanoTime()
  println("Elapsed time: " + (t1 - t0) + "ns")

  println(solutionWithConflict.size)
  solvedConf = board.applySolution(solutionWithConflict)
  println(solvedConf)
}
