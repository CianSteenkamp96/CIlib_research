package cilib.research.mgpso
import cilib.research.MGArchive
import cilib.research.core.{Benchmark, Position}
import cilib.{Dist, RNG, RVar, Step, StepS} // RNG new
import scalaz._
import Scalaz._
import spire.implicits._ // NEW
import spire.math.Interval // NEW

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

//  private def calcVelocity(particle: MGParticle,
//                   social: Position,
//                   cognitive: Position,
//                   w: Double,
//                   c1: Double,
//                   c2: Double,
//                   c3: Double) =
//    MGStep.withArchive[Double, Position](archive => {
//      if (archive.isEmpty) {
//        Step.liftR(
//          for {
//            cog <- (cognitive - particle.pos).traverse(x => Dist.stdUniform.map(_ * x))
//            soc <- (social - particle.pos).traverse(x => Dist.stdUniform.map(_ * x))
//          } yield (w *: particle.velocity) + (c1 *: cog) + (c2 *: soc)
//        )
//      } else {
//        Step.liftR(
//          RVar
//            .shuffle(archive.values.toNel.get)
//            .flatMap(archiveList => {
//              val tournament = archiveList.toList.take(3)
//              val archiveGuide = CrowdingDistance.leastCrowded(tournament)
//              for {
//                cog <- (cognitive - particle.pos).traverse(x => Dist.stdUniform.map(_ * x))
//                soc <- (social - particle.pos).traverse(x => Dist.stdUniform.map(_ * x))
//                arc <- (archiveGuide.pos - particle.pos).traverse(x => Dist.stdUniform.map(_ * x))
//                lambda <- particle.lambda.value
//              } yield {
//                (w *: particle.velocity) + (c1 *: cog) + (lambda *>: (c2 *: soc)) + (lambda.map(x =>
//                  1 - x) *>: (c3 *: arc))
//              }
//            }))
//      }
//    })

  /////////////////////////////////////////////////// NEW ///////////////////////////////////////////////////
  // Calc velocity with random control params w, c1, c2, c3 which satisfies mgpso convergence criteria
  // w (0, 1)
  // c_1,c_2,c_3 (0, 2)
  private def calcVelocity(particle: MGParticle,
                           social: Position,
                           cognitive: Position) = {

    val wc123 = satisfyStabilityCriteria(particle.lambda.value.eval(RNG.fromTime).head) // Correct way to access lambda double value? Also, why RVar[NEL[Double]] and not just Rvar[Double] ????????

    val w = wc123._1
    val c1 = wc123._2
    val c2 = wc123._3
    val c3 = wc123._4

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
  }

  /////////////////////////////////////////////////// NEW ///////////////////////////////////////////////////
  /////////////////////////////////////////////////// !!!!!!! VARS !!!!!!! ///////////////////////////////////////////////////
  private def satisfyStabilityCriteria(lambda: Double) = {
    var satisfied = false
    var w = RVar.doubles(1).run(RNG.fromTime)._2.head // gen num between 0 and 1
    var c1 = Dist.uniform(Interval(0.05, 1.95)).run(RNG.fromTime)._2 // gen num between 0 and 2
    var c2 = Dist.uniform(Interval(0.05, 1.95)).run(RNG.fromTime)._2 // gen num between 0 and 2
    var c3 = Dist.uniform(Interval(0.05, 1.95)).run(RNG.fromTime)._2 // gen num between 0 and 2

    if(((c1 + (lambda * c2) + ((1 - lambda) * c3)) > 0) &&
      ((c1 + (lambda * c2) + ((1 - lambda) * c3)) < ((4 * (1 - Math.pow(w, 2))) / (1 - w + ((Math.pow(c1, 2) + (Math.pow(lambda, 2) * Math.pow(c2, 2)) + ((Math.pow((1 - lambda), 2) * Math.pow(c3, 2)) * (1 + w))) / (3 * Math.pow((c1 + (lambda * c2) + ((1 - lambda) * c3)), 2)))))))
      (w, c1, c2, c3)
    else {
      while (!satisfied) {
        w = RVar.doubles(1).run(RNG.fromTime)._2.head // gen num between 0 and 1
        c1 = Dist.uniform(Interval(0.05, 1.95)).run(RNG.fromTime)._2 // gen num between 0 and 2
        c2 = Dist.uniform(Interval(0.05, 1.95)).run(RNG.fromTime)._2 // gen num between 0 and 2
        c3 = Dist.uniform(Interval(0.05, 1.95)).run(RNG.fromTime)._2 // gen num between 0 and 2

        if(((c1 + (lambda * c2) + ((1 - lambda) * c3)) > 0) &&
          ((c1 + (lambda * c2) + ((1 - lambda) * c3)) < ((4 * (1 - Math.pow(w, 2))) / (1 - w + ((Math.pow(c1, 2) + (Math.pow(lambda, 2) * Math.pow(c2, 2)) + ((Math.pow((1 - lambda), 2) * Math.pow(c3, 2)) * (1 + w))) / (3 * Math.pow((c1 + (lambda * c2) + ((1 - lambda) * c3)), 2)))))))
          satisfied = true
      }
      (w, c1, c2, c3)
    }
  }
  // Partial Gary solution - no vars
//  def satisfyStabilityCriteria(lambda: Double) = {
//    val generator =
//      for {
//        w  <- RVar.doubles(1) // gen num between 0 and 1
//        c1 <- Dist.uniform(Interval(0.05, 1.95)) // gen num between 0 and 2
//        c2 <- Dist.uniform(Interval(0.05, 1.95)) // gen num between 0 and 2
//        c3 <- Dist.uniform(Interval(0.05, 1.95)) // gen num between 0 and 2
//      } yield
//
//        generator.flatMap { case (w, c1, c2, c3) =>
//          if(((c1 + (lambda * c2) + ((1 - lambda) * c3)) > 0) &&
//            ((c1 + (lambda * c2) + ((1 - lambda) * c3)) < ((4 * (1 - Math.pow(w, 2))) / (1 - w + ((Math.pow(c1, 2) + (Math.pow(lambda, 2) * Math.pow(c2, 2)) + ((Math.pow((1 - lambda), 2) * Math.pow(c3, 2)) * (1 + w))) / (3 * Math.pow((c1 + (lambda * c2) + ((1 - lambda) * c3)), 2))))))) {
//            RVar.pure((w, c1, c2, c3))
//          } else {
//            generator
//          }
//        }
//
//  }

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
//          v <- calcVelocity(x, soc, cog, envX.controlParameters.w, envX.controlParameters.c1, envX.controlParameters.c2, envX.controlParameters.c3)
          v <- calcVelocity(x, soc, cog)
          p <- stdPosition(x, v)
          p2 <- multiEval(envX)(p)
          p3 <- updateVelocity(p2, v)
          p4 <- updateLambda(p3)
          updated <- updatePBestBounds(envX)(p4)
        } yield updated

}
