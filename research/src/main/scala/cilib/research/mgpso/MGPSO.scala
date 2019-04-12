package cilib.research.mgpso
import cilib.research.MGArchive
import cilib.research.core.{Benchmark, Position}
import cilib.{Dist, RVar, Step, StepS}
import scalaz.Scalaz._
import scalaz._

object MGPSO {

  private def multiEval(envX: Benchmark)(particle: MGParticle) = MGStep.stepPure[Double, MGParticle] {
    val fitness = if (particle.pos.isInbounds) {
      envX.f(particle.pos.pos)
    } else {
      particle.pos.fitness.map(_ => Double.MaxValue)
    }
    particle.updateFitness(fitness)
  }

  private def gbest(envX: Benchmark)(particle: MGParticle, collection: NonEmptyList[MGParticle]) =
    MGStep.stepPure[Double, Position] {
      val x = collection.toList
        .filter(x => x.swarmID == particle.swarmID)
        .sortWith((x, y) => envX.compareAtIndex(x.pb.fitness, y.pb.fitness, x.swarmID))
        .head
        .pb
      x
    }

  private def pbest(particle: MGParticle) = MGStep.stepPure[Double, Position] {
    particle.pb
  }

  private def updatePBest(envX: Benchmark)(particle: MGParticle) = MGStep.stepPure[Double, MGParticle] {
    if (envX.compare(particle.pos.fitness, particle.pb.fitness)) {
      particle.updatePB
    } else {
      particle
    }
  }

  private def updatePBestBounds(envX: Benchmark)(p: MGParticle): StepS[Double, MGArchive, MGParticle] =
    if (p.pos.isInbounds) updatePBest(envX)(p) else MGStep.stepPure[Double, MGParticle](p)

  private def calcVelocity(particle: MGParticle,
                   social: Position,
                   cognitive: Position,
                   w: Double,
                   c1: Double,
                   c2: Double,
                   c3: Double) =
    MGStep.withArchive[Double, Position](archive => {
      if (archive.isEmpty) {
        Step.liftR(
          for {
            cog <- (cognitive - particle.pos).traverse(x => Dist.stdUniform.map(_ * x))
            soc <- (social - particle.pos).traverse(x => Dist.stdUniform.map(_ * x))
          } yield (w *: particle.velocity) + (c1 *: cog) + (c2 *: soc)
        )
      } else {
        Step.liftR(
          RVar
            .shuffle(archive.values.toNel.get)
            .flatMap(archiveList => {
              val tournament = archiveList.toList.take(3)
              val archiveGuide = CrowdingDistance.leastCrowded(tournament)
              for {
                cog <- (cognitive - particle.pos).traverse(x => Dist.stdUniform.map(_ * x))
                soc <- (social - particle.pos).traverse(x => Dist.stdUniform.map(_ * x))
                arc <- (archiveGuide.pos - particle.pos).traverse(x => Dist.stdUniform.map(_ * x))
                lambda <- particle.lambda.value
              } yield {
                (w *: particle.velocity) + (c1 *: cog) + (lambda *>: (c2 *: soc)) + (lambda.map(x =>
                  1 - x) *>: (c3 *: arc))
              }
            }))
      }
    })

  private def updateVelocity(particle: MGParticle, v: Position) = MGStep.stepPure[Double, MGParticle] {
    particle.updateVelocity(v)
  }

  private def updateLambda(particle: MGParticle) = MGStep.stepPure[Double, MGParticle] {
    particle.updateLambda
  }

  private def stdPosition(particle: MGParticle, v: Position) = MGStep.stepPure[Double, MGParticle] {
    particle.updatePos(particle.pos + v)
  }

  private def insertIntoArchive(particle: MGParticle) =
    MGStep.modifyArchive { archive =>
      archive.insert(particle)
    }

  def mgpso(envX: Benchmark)
    : NonEmptyList[MGParticle] => MGParticle => StepS[Double, MGArchive, MGParticle] =
    collection =>
      x =>
        for {
          _ <- insertIntoArchive(x)
          cog <- pbest(x)
          soc <- gbest(envX)(x, collection)
          v <- calcVelocity(x, soc, cog, envX.controlParameters.w, envX.controlParameters.c1, envX.controlParameters.c2, envX.controlParameters.c3)
          p <- stdPosition(x, v)
          p2 <- multiEval(envX)(p)
          p3 <- updateVelocity(p2, v)
          p4 <- updateLambda(p3)
          updated <- updatePBestBounds(envX)(p4)
        } yield updated

}
