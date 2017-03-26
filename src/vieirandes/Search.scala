package vieirandes

/**
  * Created by lucas on 3/24/17.
  */
trait Search {
  def search(board: Board): List[Position]

  def expand(expansion: Expansion): List[Expansion] = {
    val board = expansion.board
    val n = board.neighbours(board.blank)
    val positions = n.map(_.pos)
    val expansions = positions.map(p => BaseExpansion(p :: expansion.steps, board.move(p)))

    expansions
  }

  def expand(board: Board): List[Expansion] = expand(BaseExpansion(List.empty, board))
}

case class BaseExpansion(steps: List[Position], board: Board) extends Expansion
object BaseExpansion {
  def fromExpansion(expansion: Expansion): BaseExpansion = BaseExpansion(expansion.steps, expansion.board)
}

trait Expansion extends Ordering[Expansion] {
  def steps: List[Position]
  def board: Board

  def compare(e1: Expansion, e2: Expansion) = e1.steps.size compare e2.steps.size

  override def toString: String = steps.toString + "\n" + board.toString
}


