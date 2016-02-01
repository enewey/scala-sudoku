package sudoku

object Generate { 
  def main(args:Array[String]) {
    if (args.size != 3) {
      println("Invalid arg length")
    } else {
      val solver = new Puzzle()
      println(args(0).toInt +" "+ args(1).toInt + "\n" + solver.generateSudoku(args(0).toInt, args(1).toInt, args(2).toInt))
    }
  }
}