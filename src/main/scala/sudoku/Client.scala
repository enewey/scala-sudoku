package sudoku

import org.http4s.Http4s._
import org.http4s.client.blaze.defaultClient
import scalaz.concurrent.Task
import org.http4s.Uri

object Client {
  
  def main(args:Array[String]) {
    if (args.length != 1) {
      println("invalid args length")
      return
    } 
    
    val url = Uri.fromString(args(0)).toOption.get
    val client = defaultClient
    val task: Task[String] = client.getAs[String](url)
    val board = task.run //the sudoku board
    
    println("Puzzle received:\n"+ board)
    println("Solving puzzle...")
    val solver = new Puzzle()
    val grid = solver.stringFromGrid(board)
    println(solver.solveSudoku(grid._1, grid._2))
    
  }
  
  
}