

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
    
    
    println("Generating 4x4 puzzle with 200 holes")
    val puzz2 = "4 4"+solver.generateSudoku(4, 4, 200)
    println(puzz2)
    println("generating csv for solver")
    val grid2 = solver.stringFromGrid(puzz2)
    println(grid2._1)
    println("Solving puzzle")
    println(solver.solveSudoku(grid2._1, grid2._2))  
  }
}