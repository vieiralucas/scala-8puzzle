package vieirandes

import scala.annotation.tailrec
import scala.collection.immutable.{HashSet, SortedSet}

/**
  * Created by lucas on 3/24/17.
  */
object Manhattan extends Search {
  case class ManhattanExpansion(steps: List[Position], board: Board, penalty: Int) extends Expansion
  object ManhattanExpansion {
    val ordering = Ordering.by { foo: ManhattanExpansion => foo.penalty }
  }


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
          .map(e => ManhattanExpansion(e.steps, e.board, current.penalty + e.board.manhattan))
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

