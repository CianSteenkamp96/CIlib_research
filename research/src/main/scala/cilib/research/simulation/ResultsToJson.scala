package cilib.research.simulation

import cilib.research.MGArchive
import cilib.research.mgpso.MGParticle
import scalaz.NonEmptyList
import scalaz.Scalaz._

object ResultsToJson {

  def finalArchive(run: Int, iteration: Int, archive: MGArchive, maxIterations: Int): String =
//    if (iteration == maxIterations) { // save more (or less) data to file. Saving only final iteration here. Use % (modulus) for other iters.
//      "{\"archive\": [" +
      "{ \"prev_ratio\": " + archive.get_pr_prKPs2ND._1 + ", \"prev_ratio_KPs_2_ND_sols\": " + archive.get_pr_prKPs2ND._2 + ", \"archive\": [" +
        archive.values
          .map(x => "[" + x.pos.fitness.toList.mkString(",") + "]")
          .mkString(",") +
        "], \"iteration\": " + iteration +
        ", \"run\": " + run + " }\n"
//    } else ""

  def particlesJson(particles: NonEmptyList[MGParticle]): String =
    "\"swarm\": [" + particles
      .map(x =>
        "{\"swarmID\": " + x.swarmID + ", \"fitness\": [" + x.pos.fitness.toList
          .mkString(",") + "]}")
      .toList
      .mkString(",") + "]"

  def archiveWithParticles(run: Int,
                           iteration: Int,
                           archive: MGArchive,
                           particles: NonEmptyList[MGParticle]): String =
    "{ \"archive\": [" +
      archive.values
        .map(x => "[" + x.pos.fitness.toList.mkString(",") + "]")
        .mkString(",") +
      "], " + particlesJson(particles) + ",\"run\": " + run + ",\"iteration\": " + iteration + " }\n"
}
