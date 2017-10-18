import Solver.Preferences

import scala.io.Source

object Main {

  def main(args: Array[String]): Unit = {
    val c = readConstraintsFromFile("assets/input4.txt")
    Solver.solve(c.preferences, c.numColors) match {
      case None => println("No solution exists")
      case Some(sol) => println(sol)
    }
  }

  def readConstraintsFromFile(filePath : String): Constraints ={
    var numColors = 0
    var listLines: List[String] = List()
    for ((line,count) <- Source.fromFile(filePath).getLines().zipWithIndex) {
      if(count==0) {
        numColors = line.split(" ")(0).toInt
      }
      else
        listLines ::= line
    }
    val prefs = listLines.map(x => x.split(" ").grouped(2).toList.map(x => (x(0).toInt,Symbol(x(1))) ))
    Constraints(numColors, prefs)
  }

}


