package sudoku

object Solve {
  def main(args:Array[String]) {
    if (args.size != 3) {
      println("Invalid arg length")
    } else {
      val solver = new Puzzle()
      println(solver.solveSudoku(args(0), (args(1).toInt, args(2).toInt)))
    }
  }
}