package cilib
package research

import java.io._
import scalaz._
import Scalaz._

object SubswarmsGen {
  def main(args:Array[String]) = {
    def genSubswarms(objectives: Int, swarmSize: Int): Stream[List[Int]] = {
      val range: Stream[Int] = Stream.range(1, swarmSize + 1, 1)
      val generators: Stream[List[Int]] = range.replicateM(objectives)

      generators.filter(_.suml == swarmSize)
    }
    if(args.size == 3)
      if(args(0).toInt > 0 && args(1).toInt > 0) {
        val file = args(2)
        val writer = new PrintWriter(new FileWriter(file))
        genSubswarms(args(0).toInt, args(1).toInt).map(_.mkString(",") + "\n").foreach(writer.write) //.foreach(println)
        writer.close()
      }
      else
        print("#objectives and populationSize command line arguments must be > 0.\n")
    else
      print("Pass command line arguments #objectives, populationSize, and fileName.\n")
  }
}
