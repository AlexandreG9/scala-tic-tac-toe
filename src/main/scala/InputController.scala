import scala.io.StdIn.readLine
import scala.util.{Failure, Success, Try}

case class Input(row: Int, col: Int)

object InputController {

  def readInput(player: Player): Try[Input] = {
    print(s"${player.getSign()} - It's up to you (type '<row> <col>')\n")
    val input = readLine().split(" ")

    Try(Input(input(0).toInt, input(1).toInt))
  }

  def updateGrid(grid: Grid, input: Try[Input], player: Player): Option[Grid] = {
    input match {
      case Failure(exception) => {
        println(s"oops ${exception}")
        None
      }
      case Success(value) => updateGridSafe(grid, value, player)
    }
  }


  private def updateGridSafe(grid: Grid, input: Input, player: Player): Option[Grid] =
    if (spaceAvailable(grid, input)) Some(insertInGrid(grid, input, player))
    else None

  private def spaceAvailable(grid: Grid, input: Input): Boolean = grid.cells(input.row - 1)(input.col - 1).maybePlayer.isEmpty

  private def insertInGrid(grid: Grid, input: Input, player: Player): Grid = {
    val cells = insertAt(grid.cells, input.row, insertAt(grid.cells(input.row - 1), input.col, Cell(Some(player))))
    Grid(cells)
  }

  private def insertAt[A](list: List[A], position: Int, value: A): List[A] = {
    val (front, back) = list.splitAt(position - 1)
    front ++ List(value) ++ back.tail
  }

  private def upCursor(): Unit = print("\u001B[A")

}
