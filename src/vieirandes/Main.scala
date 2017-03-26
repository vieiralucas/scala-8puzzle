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

  val board = Board.reversedBoard
 // println(board)

  val solution = Manhattan.search(board)
  println(solution.size)
  val solved = board.applySolution(solution)
  println(solved)

  val solutionWithConflict = ManhattanWithLinearConflict.search(board)
  println(solutionWithConflict.size)
  val solvedConf = board.applySolution(solutionWithConflict)
  println(solvedConf)

 // val depthSolution = DepthFirst.search(board)
 // println(depthSolution.size)
  //val depthSolved = board.applySolution(depthSolution)
  //println(depthSolved)
}
