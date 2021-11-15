/**
 * A list of row
 * Each row have 3 cells
 *
 * @param cells
 */
case class Grid(cells: List[List[Cell]])

object Grid {
  def newGrid(): Grid = Grid(List(
    List(Cell(None), Cell(None), Cell(None)),
    List(Cell(None), Cell(None), Cell(None)),
    List(Cell(None), Cell(None), Cell(None))
  ))
}