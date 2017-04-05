package vieirandes

import vieirandes.Manhattan.ManhattanExpansion

import scala.annotation.tailrec
import scala.collection.immutable.{HashSet, SortedSet}
import scala.collection.mutable
import scala.collection.mutable.PriorityQueue

/**
  * Created by lucas on 3/24/17.
  */
object ManhattanWithLinearConflict extends Search {
  override def search(board: Board): List[Position] = {
    @tailrec
    def searchLoop(current: ManhattanExpansion,
                   frontier: PriorityQueue[ManhattanExpansion],
                   visiteds: Map[Board, Int],
                   bestWin: Option[ManhattanExpansion]): List[Position] = {
      if (current.board.win && current.steps.size < 40) {
        return current.steps.reverse
      }

      val bestWinSize = bestWin.map(_.steps.size).getOrElse(-1)
      val newBestWin = if (current.board.win && (bestWinSize == -1 || bestWinSize > current.steps.size))
        Some(current)
      else bestWin

      val visitedSize = visiteds.get(current.board).getOrElse(-1)
      val newVisiteds = if (visitedSize == -1 || visitedSize > current.steps.size)
        visiteds + ((current.board, current.steps.size))
      else visiteds

      val expansions = expand(current)
        .map(e => ManhattanExpansion(e.steps, e.board, current.penalty + e.board.manhattan + e.board.linearConflict))
        .filter(e => {
          val visitedSize = newVisiteds.get(e.board).getOrElse(-1)
          visitedSize == -1 || (visitedSize > e.steps.size)
        })

      val newFrontier = frontier ++ expansions

      if (newFrontier.size <= 0) {
        newBestWin.map(_.steps).getOrElse(List.empty).reverse
      } else {
        val nextCurrent = newFrontier.dequeue
        searchLoop(nextCurrent, newFrontier, newVisiteds, newBestWin)
      }
    }

    searchLoop(ManhattanExpansion(List.empty, board, 0), PriorityQueue.empty[ManhattanExpansion](Ordering.by((_: ManhattanExpansion).penalty).reverse), Map.empty, None)
  }
}

