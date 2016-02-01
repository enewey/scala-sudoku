package sudoku
import scala.util.Random

class Puzzle {  
  
  /**
   * Generates a mxn sudoku puzzle, where holes is the number of blank spots in the puzzle.
   */
  def generateSudoku(rows:Int, cols:Int, holes:Int): String = {
    def poke(model:Map[String, Set[Int]], times:Int, max:Int): Map[String, Set[Int]] = {
      if (times == 0 || times > max*max) return model
      val rand = new Random()
      val shuf = rand.shuffle(model.keys.filter(a => model.get(a).get.size == 1))
      poke(model.updated(shuf.toList(rand.nextInt(shuf.size-1)), (0 to max).toSet), times-1, max)
    }
    
    val nums = Random.shuffle((1 to (rows*cols))).mkString(",") //generate the first row
    val zeroes = (",0"*((rows*cols*rows*cols)-(rows*cols))) //make the rest empty, let the solver figure it out
    val m = initModel(nums+zeroes, rows, cols)
    val soln = poke(getSolution((m._1,true), m._2)._1, holes, rows*cols)
    return stringGrid(stringModel(soln), (rows, cols)) + "\n"
  }
  
  /**
   * Solves a sudoku puzzle, represented as a single string of comma-separated numbers, 
   *  and a tuple describing the mxn dimensions
   */
  def solveSudoku(in:String, dims:(Int,Int)):String = {
    val m = initModel(in, dims._1, dims._2)
    return stringGrid(stringModel(getSolution((m._1,true), m._2)._1), (dims._1, dims._2)) + "\n"
  }
  
  /**
   * Formats a comma-separated string of numbers into a grid of the given dimensions.
   * Will fail if the string of numbers does not contain m*n numbers.
   */
  def stringGrid(in:String, dims:(Int,Int)): String = {
    val maxNumLen = (dims._1*dims._2).toString().length()
    val grid = in.split(',').map(c => c.toInt).toSeq
    
    def recur(grid:Seq[Int], max:Int, it:Int, ret:String): String = {
      if (grid.size == 0) return ret
      val pad = (maxNumLen - grid(0).toString().length()) + 1
      return recur(grid.drop(1), max, it+1, ret + (if (it % (dims._1*dims._2) == 0) "\n" else "") + (if (grid(0) == 0) "_" else grid(0)) + (" " * pad))
    }
    
    recur(grid, maxNumLen, 0, "")
  }
  
  /**
   * Utility to convert from a grid to a comma-separated string of numbers, along with a tuple describing the dimensions.
   */
  def stringFromGrid(in:String): (String, (Int,Int)) = {
    def recur(str:String, lines:List[String]): String = {
      if (lines.size == 0) return str
      val line = lines(0).replace('_', '0').trim().split(" ").mkString(",")
      return recur((if (!str.isEmpty()) str + "," else str) + lines(0).replace('_', '0').trim().split(" ").mkString(","), lines.drop(1))
    }
    val lines = in.split("\n").toList
    val dims = lines(0).trim().split(" ")
    return ( recur("", lines.drop(1)), (dims(0).toInt, dims(1).toInt) )
  }
  
  /**
   * Gives a long comma-separated string, like is passed in at the command line
   */
  def stringModel(model:Map[String, Set[Int]]): String = {
    val mseq = model.toSeq.sortBy(_._1)
    def recur(m:Seq[(String, Set[Int])], acc:String): String = {
      if (m.size == 0) return acc
      else {
        val next = if (m(0)._2.size == 1) m(0)._2.head else "0"
        recur(m.drop(1), acc+","+next)
      }
    }
    recur(mseq.drop(1), if (mseq(0)._2.size == 1) mseq(0)._2.head.toString() else "0")
  }
  
  /**
   * Model the solution as a mxn dictionary of strings to sets
   * rows are A-*, columns are 1-n, so the strings are A1, A2, ... etc.
   * The sets are integer sets that detail what number each space could possibly contain
   * Without applying constraints, each set starts out with all numbers 1-n
   */
  def initModel(in:String, rows:Int, cols:Int): (Map[String, Set[Int]], (Int,Int)) = {
    def convert(it:Int, dim:Int):String = getStringCoords(Math.floor(it / dim).toInt, (it % dim).toInt)
    def recur(grid:Seq[Int], model:Map[String, Set[Int]], i:Int): Map[String, Set[Int]] = {
      if (grid.size == 0) return model
      else {
        val placement = place(convert(i, rows*cols), grid.head, model, (rows,cols))
        if (placement._2) recur(grid.drop(1), placement._1, i+1)
        else model
      }
    }
    
    val grid = in.split(',').map(c => c.toInt).toSeq
    val dim = rows*cols
    val m = (for (i <- 0 to grid.length-1) yield {
      convert(i, rows*cols) -> (1 to dim).toSet
    }).groupBy(_._1).map(p => (p._1, p._2.map(_._2).flatten.toSet))
    
    (recur(grid, m, 0), (rows,cols))
  }
  
  /**
   * Attempt to place a value in the model, then check for contradictions.
   * The Boolean will be true if there were no contradictions. (ie a successful placement)
   */
  def place(coord:String, value:Int, model:Map[String, Set[Int]], dims:(Int,Int)): (Map[String, Set[Int]], Boolean) = {
    def recur(removes:List[Int], m:Map[String, Set[Int]]): (Map[String, Set[Int]], Boolean) = {
      if (removes.size == 0) 
        return (m, true)
      
      val rm = removal(coord, removes.head, m, dims)
      if (!rm._2) 
        (m, false)
      else recur(removes.drop(1), rm._1)
    }
    
    if (value == 0) //if trying to place a 0, just return the model as it was sent in, but consider it successful.
      return (model, true)

    recur(model.get(coord).get.-(value).toList, model)
  }
  
  /**
   * Called by the 'place' function; will attempt to remove the value placed in the square from its peers,
   *  and report any contradictions.
   * There are three contradictions to consider:
   * 		1. Trying to remove the final value possible for a square.
   * 		2. After removal, there is only one value remaining in a square, and that value can't be removed from peers.
   * 		3. The removed value cannot be placed in any other square
   */
  def removal(coord:String, value:Int, model:Map[String, Set[Int]], dims:(Int,Int)): (Map[String, Set[Int]], Boolean) = {
    //Helper method 1 - searches for two contradictions, based on what values are already "solved"
    def removal2(coord:String, model:Map[String, Set[Int]], m:Map[String, Set[Int]]): (Map[String, Set[Int]], Boolean) = {
      def recur(keySet:List[String], v:Int, contraModel:Map[String, Set[Int]]): (Map[String, Set[Int]], Boolean) = {
        if (keySet.size == 0) (contraModel, true)  
        else {
          val rem = removal(keySet.head, v, contraModel, dims) 
          if (!rem._2) (contraModel, false)
          else recur(keySet.drop(1), v, rem._1)
        }
      }
      
      if (m.get(coord).get.size == 0) 
        return (model, false) //Contradiction 1: can't remove the final value of a square..
      else if (m.get(coord).get.size == 1) {
        val c2 = recur(getPeerKeys(m, coord, dims).toList, m.get(coord).get.head, m)
        if (!c2._2)
          return (model,false) //Contradiction 2: If the only value left can't be removed from peers, fail
        else
          return (c2._1, true)
      }
      
      return (m, true)
    }
    
    //Helper method 2 - searches for contradiction based on what possible values are unique to each square
    def removal3(coord:String, value:Int, model:Map[String, Set[Int]], m:Map[String, Set[Int]]): (Map[String, Set[Int]], Boolean) = {
      for (keyList <- getUnitKeys(model, coord, dims)) {
        val places = keyList.filter(p => m.get(p).get.contains(value))
        if (places.size == 0) 
          return (model, false) //Contradiction 3: no place for the removed value to go.
        else if (places.size == 1) {
          val pl = place(places.head, value, m, dims)
          if (!pl._2)
            return (model, false)
          else
            return (pl._1, true)
        }
      }
      
      return (m, true)
    }
    
    //Begin removal of value and checking for contradictions against the rest of the board
    if (!model.get(coord).get.contains(value)) 
      return (model, true) //value already removed

    val r2 = removal2(coord, model, model.updated(coord, model.get(coord).get.-(value))) //remove the value from the given square's possibilities
    if (r2._2) {
      val r3 = removal3(coord, value, model, r2._1)
      if (r3._2) {
        return (r3._1, true)
      } else {
        return (model, false)
      }
    } else {
      return (model, false)
    }
  }
  
  /**
   * Returns a solved sudoku puzzle. The boolean is used to say if the puzzle is actually solved. (used in recursive call)
   */
  def getSolution(model:(Map[String, Set[Int]], Boolean), dims:(Int,Int)): (Map[String, Set[Int]], Boolean) = {
    if (!model._2) return model //a previous place call failed... return
    if (isSolved(model._1, dims)) return (model._1, true) //success case
   
    val kv =  model._1.filter(a => a._2.size > 1).toSeq.sortBy(_._2.size).head
    for(n <- Random.shuffle(kv._2.toSeq)) { //the randomization here helps with generation
      val p = getSolution(place(kv._1, n, model._1, dims), dims)
      if (p._2) return p
    }
    (model._1, false) //if we can't find a solution, just spit it back out.
  }
  
  /**
   * Helper methods: Group-getting methods for the sudoku puzzle.
   */
  def getRowKeys(model:Map[String, Set[Int]], coord:String): List[String] = { model.keys.filter(s => s.contains(coord.charAt(0))).toList }
  def getColKeys(model:Map[String, Set[Int]], coord:String): List[String] = { model.keys.filter(s => s.substring(1).equals(coord.substring(1))).toList }
  def getSquareKeys(model:Map[String, Set[Int]], coord:String, dims:(Int,Int)): List[String] = { (getSquareTuples(dims, model, coord).map(a => getStringCoords(a._1, a._2))) }
  
  /**
   * Helpers for the group-getters
   */
  def getSquareTuples(dims:(Int,Int), model:Map[String, Set[Int]], coord:String): List[(Int,Int)] = {
    def getOffset(dims:(Int,Int), t:(Int, Int)): (Int,Int) = {
      ((Math.floor(t._1 / dims._2).toInt * dims._2), 
        (Math.floor(t._2 / dims._1).toInt * dims._1))
    }
    
    val offset = getOffset(dims, getIntCoords(coord))
    val rows = List.range(offset._1, offset._1+dims._2)
    val cols = List.range(offset._2, offset._2+dims._1)
    (for (r <- rows) yield for (c <- cols) yield (r,c)).flatMap(x => x)
  }  
  
  /**
   * Peer keys: Returns a set of all keys that share a row, column, or square-group with a single space.
   */
  def getPeerKeys(model:Map[String, Set[Int]], coord:String, dims:(Int,Int)): Set[String] = {
    (getRowKeys(model,coord)++getColKeys(model,coord)++getSquareKeys(model,coord, dims)).filter(s => !s.equals(coord)).toSet
  }
  /**
   * Unit keys: Returns three lists of keys related to a space: The row, the column, and the square-group.
   */
  def getUnitKeys(model:Map[String, Set[Int]], coord:String, dims:(Int,Int)): List[List[String]] = {
    List(getRowKeys(model,coord),getColKeys(model,coord),getSquareKeys(model,coord, dims))
  }
 
  /**
   * Methods used to determine if a sudoku puzzle is solved.
   */
  def isSolved(model:Map[String, Set[Int]], dims:(Int,Int)): Boolean = { model.forall(p => verifySquare(model, p, dims)) }
  def verifySquare(model:Map[String, Set[Int]], square:(String, Set[Int]), dims:(Int,Int)): Boolean = {
    if (square._2.size != 1) return false
    val keySets = getUnitKeys(model, square._1, dims).map(r => r.filter(p => !p.equals(square._1)))
    val v = square._2.head
    keySets.forall(keySet => model.filter(a => keySet.contains(a._1)).forall(p => !p._2.contains(v)))
  }
  
  /**
   * Convert row/col coords from ints to a String, and back.. etc. "A0" = (0,0), "A1" = (0,1)...
   */
  def getIntCoords(coords:String): (Int,Int) = { (coords.charAt(0).toInt-65, coords.substring(1).toInt) }
  def getStringCoords(row:Int, col:Int): String = { (((row+65).toChar).toString+col.toString) }
}