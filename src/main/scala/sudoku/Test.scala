

package sudoku

object Test {
  def main(args:Array[String]) {
    val solver = new Puzzle()
    println("Generating 3x3 puzzle with 50 holes")
    val puzz = "3 3"+solver.generateSudoku(3, 3, 50)
    println(puzz)
    println("generating csv for solver")
    val grid = solver.stringFromGrid(puzz)
    println(grid)
    println("Solving puzzle")
    println(solver.solveSudoku(grid._1, grid._2))  
  }
}