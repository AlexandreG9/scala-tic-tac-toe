import scala.annotation.tailrec

object Main extends App {
  val playerA = new PlayerA
  val playerB = new PlayerB

  val grid = Grid(List(
    List(Cell(Some(playerA)), Cell(Some(playerA)), Cell(Some(playerB))),
    List(Cell(Some(playerA)), Cell(None), Cell(Some(playerB))),
    List(Cell(Some(playerA)), Cell(None), Cell(Some(playerA)))
  ))

  printGrid(grid)

  println(checkGrid(grid))


  /**
   * A list of row
   * Each row have 3 cells
   *
   * @param cells
   */
  case class Grid(cells: List[List[Cell]])

  def getRowFromColumn[A](list: List[List[A]], acc: List[List[A]] = List.empty): List[List[A]] = {
    if (list.isEmpty || list.head.isEmpty) acc
    else getRowFromColumn(list.map(r => r.tail), acc :+ list.map(r => r.head))
  }

  def getRowFromDiag[A](list: List[List[A]], acc: List[A] = List.empty): List[A] = {
    if (list.isEmpty || list.head.isEmpty) acc
    else getRowFromDiag(list.map(r => r.tail).tail, acc :+ list.head.head)
  }


  def play(grid: Grid): Unit = {
    checkGrid(grid) match {
      case Some(value) => println(s"$value win !!")
      case None => {
        // Ask input
        val input = ???
        val newGrid = ???
        play(newGrid)
      }
    }
  }

  def checkGrid(grid: Grid): Option[Player] = {
    val resultByLine = for {
      horizontalLine <- grid.cells
      verticalLine <- getRowFromColumn(grid.cells)
      diagLine <- List(getRowFromDiag(grid.cells))
      antiDiag <- List(getRowFromDiag(grid.cells.reverse))
      allLine <- List(horizontalLine, verticalLine, diagLine, antiDiag)
    } yield checkRowWin(allLine)

    resultByLine.flatten.headOption
  }

  def checkRowWin(row: List[Cell]): Option[Player] = {
    def loop(newItem: List[Cell], acc: Option[Player] = None): Option[Player] = {
      if (acc.isEmpty || newItem.isEmpty) acc
      else {
        newItem.head.maybePlayer match {
          case Some(value) => acc match {
            case Some(accValue) =>
              if (accValue.equals(value))
                loop(newItem.tail, Some(value))
              else None
            case None => None
          }
          case None => None
        }
      }
    }

    if (row.head.maybePlayer.isEmpty) None
    else loop(row.tail, row.head.maybePlayer)
  }

  /**
   * Game section
   */
  def printGrid(grid: Grid): Unit = grid.cells.map(getRow).foreach(print)

  def getRow(row: List[Cell]): String = row.mkString(" | ") + System.lineSeparator()
}