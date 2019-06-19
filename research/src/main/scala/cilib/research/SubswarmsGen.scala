package cilib
package research

import scalaz._
import Scalaz._

object SubswarmsGen {
  def main(args:Array[String]) = {
    def genSubswarms(objectives: Int, swarmSize: Int): Stream[List[Int]] = {
      val range: Stream[Int] = Stream.range(1, swarmSize + 1, 1)
      val generators: Stream[List[Int]] = range.replicateM(objectives)

      generators.filter(_.suml == swarmSize)
    }
    genSubswarms(5, 126).map(_.mkString(",")).foreach(println)
  }
}
