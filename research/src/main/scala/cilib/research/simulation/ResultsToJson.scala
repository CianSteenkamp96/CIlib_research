package cilib.research.simulation
import cilib.research.MGArchive
import scalaz.Scalaz._

object ResultsToJson {

  def finalArchive(run: Int, iteration: Int, archive: MGArchive): String = {
    if (iteration === 2000) {
      "{ \"archive\": [" +
        archive.values
          .map(x => "[" + x.pos.fitness.toList.mkString(",") + "]")
          .mkString(",") +
        "], \"run\": " + run + " }\n"
    } else ""
  }

}
