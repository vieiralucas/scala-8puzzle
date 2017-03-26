package vieirandes

import vieirandes.Manhattan.ManhattanExpansion

import scala.annotation.tailrec
import scala.collection.immutable.{HashSet, SortedSet}

/**
  * Created by lucas on 3/24/17.
  */
object ManhattanWithLinearConflict extends Search {
  override def search(board: Board): List[Position] = {
    @tailrec
    def searchLoop(current: ManhattanExpansion,
                   frontier: SortedSet[ManhattanExpansion],
                   visiteds: HashSet[Board]): List[Position] = {
      if (current.board.win) {
        current.steps.reverse
      } else {
        val newVisiteds = visiteds + current.board
        val expansions = expand(current)
          .map(e => ManhattanExpansion(e.steps, e.board, current.penalty + e.board.manhattan + e.board.linearConflict))
          .sortBy(_.penalty)

        val unvisitedExpansions = expansions.filterNot(e => newVisiteds.contains(e.board))
        val newFrontier = frontier.drop(1) ++ unvisitedExpansions

        val nextCurrent = newFrontier.headOption

        nextCurrent match {
          case Some(newCurrent) => searchLoop(newCurrent, newFrontier, newVisiteds)
          case None => List.empty
        }
      }
    }

    implicit val ord = ManhattanExpansion.ordering
    searchLoop(ManhattanExpansion(List.empty, board, 0), SortedSet.empty[ManhattanExpansion], HashSet.empty)
  }
}

