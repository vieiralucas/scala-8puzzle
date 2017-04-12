package vieirandes

/**
  * Created by lucas on 3/24/17.
  */
object Main extends App {
  val matrix = List(
    List(Cell(Position(0, 0), 4),Cell(Position(1, 0), 7),Cell(Position(2, 0), 5)),
    List(Cell(Position(0, 1), 0),Cell(Position(1, 1), 2),Cell(Position(2, 1), 1)),
    List(Cell(Position(0, 2), 3),Cell(Position(1, 2), 6),Cell(Position(2, 2), 8))
  )

  // 27 passos


  /*
  3, 2, 1
  4, 5, 6
  7, 8, 0
  */

  val board = Board(matrix, Position(0, 1))

  println("Heurística: Apenas Manhattan Distance")
  println("Tabuleiro inicial:\n" + board)
  var t0 = System.nanoTime()
  val (solution, stats) = Manhattan.search(board)
  var t1 = System.nanoTime()
  println("Tempo decorrido: " + (t1 - t0) + "ns")
  println("Tamanho da solução: " + solution.size)
  println("Quantidade total de estados avaliados: " + stats.evaluations)
  println("Tamanho máximo da fronteira: " + stats.biggestFrontier)
  println("Sequência de jogadas:")
  solution.foreach(println)
  println("\nTabuleiros intermediarios: ")
  var interBoard = board
  solution.foreach(s => {
    println(interBoard)
    interBoard = interBoard.move(s)
  })
  println(interBoard)
  println("###########################")

  println("Heurística: Manhattan Distance + Linear Conflict")
  println("Tabuleiro inicial:\n" + board)
  t0 = System.nanoTime()
  val (solutionWithConflict, conflictStats) = ManhattanWithLinearConflict.search(board)
  t1 = System.nanoTime()
  println("Tempo decorrido: " + (t1 - t0) + "ns")
  println("Tamanho da solução: " + solutionWithConflict.size)
  println("Quantidade total de estados avaliados: " + conflictStats.evaluations)
  println("Tamanho máximo da fronteira: " + conflictStats.biggestFrontier)
  println("Sequência de jogadas:")
  solutionWithConflict.foreach(println)
  println("\nTabuleiros intermediarios: ")
  interBoard = board
  solutionWithConflict.foreach(s => {
    println(interBoard)
    interBoard = interBoard.move(s)
  })
  println(interBoard)

}
