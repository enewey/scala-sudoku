package sudoku

//import org.http4s.headers.{`Content-Type`, `Content-Length`}
import org.http4s._
import org.http4s.dsl._
import org.http4s.server.blaze.BlazeBuilder
import scalaz.concurrent.Task


object Server {
  
  def main(args:Array[String]) {
    
    val route = HttpService {
      
      //uri pattern -- ' http://domain:port/generate?params=m-n-holes.txt '
      case req @ GET -> Root / "generate" => {
        
        //println(req.uri.toString())
        val str = req.uri.toString().split("params=")
        if (str.length < 2) {
          Ok("Params not valid")
        } else {
          val params = str(1).substring(0,str(1).length-4).split('-')
          if (params.length < 3) {
            Ok("Invalid param length") 
          } else {
            val m = params(0).toInt
            val n = params(1).toInt
            val holes = params(2).toInt
            println("Generating "+m+"x"+n+" puzzle with "+holes+" blank spots..")            
            val solver = new Puzzle()
            val res = m +" "+ n + solver.generateSudoku(m, n, holes)
            println("Puzzle generated:" + res)
            Ok(res)  
          }
        }
      }
      
      case GET -> Root => {
        Ok("i am root")
      }
    }
    
    println("Starting server on port 8080..")
    BlazeBuilder.bindHttp(8080)
      .withWebSockets(true)
      .mountService(route, "/")
      .run
      .awaitShutdown()
      
    
  }
}