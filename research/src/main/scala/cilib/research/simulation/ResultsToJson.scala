package cilib.research.simulation
import cilib.research.MGArchive
import cilib.research.mgpso.MGParticle
import scalaz.NonEmptyList
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

  def particlesJson(particles: NonEmptyList[MGParticle]): String = {
    "\"swarm\": [" + particles.map(x => "{\"swarmID\": " + x.swarmID + ", \"fitness\": [" + x.pos.fitness.toList.mkString(",") + "]}").toList.mkString(",") + "]"
  }

  def archiveWithParticles(run: Int, iteration: Int, archive: MGArchive, particles: NonEmptyList[MGParticle]): String = {
    "{ \"archive\": [" +
      archive.values
        .map(x => "[" + x.pos.fitness.toList.mkString(",") + "]")
        .mkString(",") +
      "], " + particlesJson(particles) + ",\"run\": " + run + ",\"iteration\": " + iteration + " }\n"
  }

}
