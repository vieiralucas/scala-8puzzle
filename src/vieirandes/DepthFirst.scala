package vieirandes

import scala.annotation.tailrec
import scala.collection.immutable.HashSet

/**
  * Created by lucas on 3/24/17.
  */
object DepthFirst extends Search {
  override def search(board: Board): List[Position] = {
    @tailrec
    def searchLoop(current: BaseExpansion, frontier: List[BaseExpansion], visiteds: HashSet[Board]): List[Position] = {
      if (current.board.win) {
        current.steps.reverse
      } else {
        val newVisiteds = visiteds + current.board
        val expansions = expand(current)
        val unvisitedExpansions = expansions.filterNot(e => newVisiteds.contains(e.board)).map(BaseExpansion.fromExpansion)
        val newFrontier = unvisitedExpansions ++ frontier.drop(1)

        val nextCurrent = newFrontier.headOption

        nextCurrent match {
          case Some(newCurrent) => searchLoop(newCurrent, newFrontier, newVisiteds)
          case None => List.empty
        }
      }
    }

    searchLoop(BaseExpansion(List.empty, board), List.empty, HashSet.empty)
  }
}
