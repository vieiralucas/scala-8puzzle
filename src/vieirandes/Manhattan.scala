package vieirandes

import scala.annotation.tailrec
import scala.collection.mutable.{PriorityQueue}

/**
  * Created by lucas on 3/24/17.
  */
object Manhattan extends Search {
  case class ManhattanExpansion(steps: List[Position], board: Board, penalty: Int) extends Expansion
  override def search(board: Board): (List[Position], Stats) = {
    @tailrec
    def searchLoop(current: ManhattanExpansion,
                   frontier: PriorityQueue[ManhattanExpansion],
                   visiteds: Map[Board, Int],
                   bestWin: Option[ManhattanExpansion],
                   stats: Stats): (List[Position], Stats) = {
      val bestWinSize = bestWin.map(_.steps.size).getOrElse(-1)
      val newBestWin = if (current.board.win && (bestWinSize == -1 || bestWinSize > current.steps.size))
        Some(current)
      else bestWin

      val visitedSize = visiteds.get(current.board).getOrElse(-1)
      val newVisiteds = if (visitedSize == -1 || visitedSize > current.steps.size)
        visiteds + ((current.board, current.steps.size))
      else visiteds

      val expansions = expand(current)
        .map(e => ManhattanExpansion(e.steps, e.board, current.penalty + e.board.manhattan))
        .filter(e => {
          val visitedSize = newVisiteds.get(e.board).getOrElse(-1)
          visitedSize == -1 || (visitedSize > e.steps.size)
        })

      val newFrontier = frontier ++ expansions

      val newBiggestFrontier = if (stats.biggestFrontier < newFrontier.size) newFrontier.size else stats.biggestFrontier
      val newStats = Stats(evaluations = stats.evaluations + 1, biggestFrontier = newBiggestFrontier)

      if (newFrontier.size <= 0) {
        (newBestWin.map(_.steps).getOrElse(List.empty).reverse, stats)
      } else {
        val nextCurrent = newFrontier.dequeue
        searchLoop(nextCurrent, newFrontier, newVisiteds, newBestWin, newStats)
      }
    }

    searchLoop(
      ManhattanExpansion(List.empty, board, 0),
      PriorityQueue.empty[ManhattanExpansion](Ordering.by((_: ManhattanExpansion).penalty).reverse),
      Map.empty,
      None,
      Stats(0, 0)
    )
  }
}

