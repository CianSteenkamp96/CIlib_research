package cilib.research.mgpso
import cilib.research.MGArchive
import cilib.research.core.{Benchmark, Position}
import cilib.{Dist, RVar, Step, StepS}
import scalaz._
import Scalaz._
import spire.implicits._
import spire.math.Interval

// MGPSO, PMGPSO, and KnMGPSO
object MGPSO {

  private def multiEval(envX: Benchmark)(particle: MGParticle) =
    MGStep.stepPure[Double, MGParticle] {
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

  private def updatePBest(envX: Benchmark)(particle: MGParticle) =
    MGStep.stepPure[Double, MGParticle] {
      if (envX.compare(particle.pos.fitness, particle.pb.fitness)) {
        particle.updatePB
      } else {
        particle
      }
    }

  private def updatePBestBounds(envX: Benchmark)(
      p: MGParticle): StepS[Double, MGArchive, MGParticle] =
    if (p.pos.isInbounds) updatePBest(envX)(p) else MGStep.stepPure[Double, MGParticle](p)

  private def calcVelocity(
      particle: MGParticle,
      social: Position,
      cognitive: Position,
      R: NonEmptyList[Double] = NonEmptyList[Double](-1)): StepS[Double, MGArchive, Position] =
    MGStep.withArchive[Double, Position](
      archive => { // See Gitter chat with Kyle Erwin explaining this code block ...
        if (archive.isEmpty) {
          Step.liftR( // ... and this code block.
            for {
              wc123 <- particle.lambda.value.map(list => satisfyStabilityCriteria(list.head))
              weights <- wc123
              (w, c1, c2, _) = weights
                .getOrElse((0.356, 1.222, 1.3, 1.517)) // if finding suitable params took too long then return 'default' ctrl param vals.
              // These were calculated as the average value per ctrl param over all 9 WFG problems taken from the optimal WFG 3 objective parameters.
              // A.k.a => 3 objective WFG 1-9 w vals / 9, 3 objective WFG 1-9 c1 vals / 9, etc...
              // These values satisfy the convergence condition for MGPSO (lambda vals were chosen (0, 1) and still satisfied the stability criteria regardless of lambda).
              cog <- (cognitive - particle.pos).traverse(x => Dist.stdUniform.map(_ * x))
              soc <- (social - particle.pos).traverse(x => Dist.stdUniform.map(_ * x))
            } yield (w *: particle.velocity) + (c1 *: cog) + (c2 *: soc))
        } else {
          // NOTE !!! FOR KNMGPSO THE ARCHIVE AND ARCHIVE MANAGEMENT IS THE SAME BUT THE ARCHIVE GUIDE IS CHOSEN DIFFERENTLY!!!
          // if MGPSO or PMGPSO
          if (R.head == -1 && R.size == 1) {
            Step.liftR(
              RVar
                .shuffle(archive.values.toNel.get)
                .flatMap(archiveList => {
                  val tournament = archiveList.toList.take(3)
                  val archiveGuide: MGParticle = CrowdingDistance.leastCrowded(tournament)

                  for {
                    wc123 <- particle.lambda.value.map(list => satisfyStabilityCriteria(list.head))
                    weights <- wc123
                    (w, c1, c2, c3) = weights
                      .getOrElse((0.356, 1.222, 1.3, 1.517)) // if finding suitable params took too long then return 'default' ctrl param vals.
                    // These were calculated as the average value per ctrl param over all 9 WFG problems taken from the optimal WFG 3 objective parameters.
                    // A.k.a => 3 objective WFG 1-9 w vals / 9, 3 objective WFG 1-9 c1 vals / 9, etc...
                    // These values satisfy the convergence condition for MGPSO (lambda vals were chosen (0, 1) and still satisfied the stability criteria regardless of lambda).
                    cog <- (cognitive - particle.pos).traverse(x => Dist.stdUniform.map(_ * x))
                    soc <- (social - particle.pos).traverse(x => Dist.stdUniform.map(_ * x))
                    arc <- (archiveGuide.pos - particle.pos).traverse(x =>
                      Dist.stdUniform.map(_ * x))
                    lambda <- particle.lambda.value
                  } yield {
                    (w *: particle.velocity) + (c1 *: cog) + (lambda *>: (c2 *: soc)) + (lambda.map(
                      x => 1 - x) *>: (c3 *: arc))
                  }
                }))
          } else { // if KnMGPSO
            Step.liftR(
              RVar
                .shuffle(archive.values.toNel.get)
                .flatMap(archiveList => {
                  // if neither = KP and same max distance to extremal hyperplane, return least crowded of the two particles.
                  // Note that due to the way that '.take' works and the fact that we 'shuffle' the archive before this is executed in MGPSO.scala, therefore, choosing 2 'new' sols for the crowding distance tournament will actually use the same tournament participants as taken here.
                  val archiveGuide: MGParticle = KneePoint
                    .kneePoint(archiveList, R)
                    .getOrElse(CrowdingDistance.leastCrowded(archiveList.toList.take(2)))
                  for {
                    wc123 <- particle.lambda.value.map(list => satisfyStabilityCriteria(list.head))
                    weights <- wc123
                    (w, c1, c2, c3) = weights
                      .getOrElse((0.356, 1.222, 1.3, 1.517)) // if finding suitable params took too long then return 'default' ctrl param vals.
                    // These were calculated as the average value per ctrl param over all 9 WFG problems taken from the optimal WFG 3 objective parameters.
                    // A.k.a => 3 objective WFG 1-9 w vals / 9, 3 objective WFG 1-9 c1 vals / 9, etc...
                    // These values satisfy the convergence condition for MGPSO (lambda vals were chosen (0, 1) and still satisfied the stability criteria regardless of lambda).
                    cog <- (cognitive - particle.pos).traverse(x => Dist.stdUniform.map(_ * x))
                    soc <- (social - particle.pos).traverse(x => Dist.stdUniform.map(_ * x))
                    arc <- (archiveGuide.pos - particle.pos).traverse(x =>
                      Dist.stdUniform.map(_ * x))
                    lambda <- particle.lambda.value
                  } yield {
                    (w *: particle.velocity) + (c1 *: cog) + (lambda *>: (c2 *: soc)) + (lambda.map(
                      x => 1 - x) *>: (c3 *: arc))
                  }
                }))
          }
        }
      })

  // Thanks, Gary Pampara for the help with this method.
  def satisfyStabilityCriteria(lambda: Double): RVar[Option[(Double, Double, Double, Double)]] = {
    def generator(counter: Int): RVar[Option[(Int, (Double, Double, Double, Double))]] =
      for {
        w <- RVar.next[Double] // gen num between 0 and 1
        c1 <- Dist.uniform(Interval.open(0.0, 2.0)) // gen num between 0 and 2
        c2 <- Dist.uniform(Interval.open(0.0, 2.0)) // gen num between 0 and 2
        c3 <- Dist.uniform(Interval.open(0.0, 2.0)) // gen num between 0 and 2
      } yield Some((counter, (w, c1, c2, c3)))

    val result: RVar[Option[(Int, (Double, Double, Double, Double))]] =
      generator(0).flatMap {
        case Some((counter, (w, c1, c2, c3))) =>
          // MGPSO convergence/stability criteria
          if (((c1 + (lambda * c2) + ((1 - lambda) * c3)) > 0) &&
              ((c1 + (lambda * c2) + ((1 - lambda) * c3)) < ((4 * (1 - Math.pow(w, 2))) / (1 - w + ((Math
                .pow(c1, 2) + (Math.pow(lambda, 2) * Math.pow(c2, 2)) + ((Math.pow((1 - lambda), 2) * Math
                .pow(c3, 2)) * (1 + w))) / (3 * Math.pow((c1 + (lambda * c2) + ((1 - lambda) * c3)),
                                                         2)))))))
            RVar.pure(Some((counter, (w, c1, c2, c3))))
          else if (counter > 10) RVar.pure(None)
          else generator(counter + 1) // If generating random values that satisfy the stability criteria takes to long then return None
        case None =>
          sys.error("impossible")
      }
    //    result.map(option => option.map(Tuple2 => Tuple2._2))
    result.map(_.map(_._2))
  }

  private def updateVelocity(particle: MGParticle, v: Position) =
    MGStep.stepPure[Double, MGParticle] {
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

  // desired_ratio_KPs_2_ND_sols => user defined parameter > 0 and < 1; represents the desired ratio of knee points to non-dominated solutions
  def mgpso_pmgpso_knmgpso(envX: Benchmark, desired_ratio_KPs_2_ND_sols: Double = -1.0)
    : NonEmptyList[MGParticle] => MGParticle => StepS[Double, MGArchive, MGParticle] =
    collection => {
      if (desired_ratio_KPs_2_ND_sols != -1.0) { // KnMGPSO
        val fitnessValues: List[List[Double]] = collection.toList.map(x => x.pos.fitness.toList)
        val numObjectives: Int = fitnessValues.head.size
        // adaptive neighbourhood strategy

        ////////////////////// FIX HERE !!! ///////////////////////////////////////////////////////
        // Move to parameters and persist ???
        // ratio of knee points to non-dominated solutions at iteration t − 1
        // HOW TO UPDATE THIS ??? ALL KPS AT ITERATION T / ARCHIVE.SIZE ???
        val prev_ratio_KPs_2_ND_sols: Int = 0
        // ratio of the neighbourhood size to the range spanned by objective m at iteration t - 1
        val prev_ratio = 1
        //////////////////////////////////////////////////////////////////////////////////////////

        // ratio of the neighbourhood size to the range spanned by objective m at iteration t
        val ratio: Double = prev_ratio * math.pow(
          math.E,
          -(1 - (prev_ratio_KPs_2_ND_sols / desired_ratio_KPs_2_ND_sols)) / numObjectives)
        // max for each objective
        val maxes: List[Double] = fitnessValues.transpose.map(_.max)
        // min for each objective
        val mins: List[Double] = fitnessValues.transpose.map(_.min)
        // size of neighbourhood for each objective
        val R: NonEmptyList[Double] = (maxes, mins).zipped.map(_ - _).map(_ * ratio).toNel.get

        assert(R.size == numObjectives)

        x: MGParticle =>
          for {
            _ <- insertIntoArchive(x)
            cog <- pbest(x)
            soc <- gbest(envX)(x, collection)
            v <- calcVelocity(x, soc, cog, R) // New calcVelocity which generates random ctrl params satisfying the MGPSO stability criteria.
            p <- stdPosition(x, v)
            p2 <- multiEval(envX)(p)
            p3 <- updateVelocity(p2, v)
            p4 <- updateLambda(p3)
            updated <- updatePBestBounds(envX)(p4)
          } yield updated
      } else { x => // MGPSO or PMGPSO
        for {
          _ <- insertIntoArchive(x)
          cog <- pbest(x)
          soc <- gbest(envX)(x, collection)
          v <- calcVelocity(x, soc, cog) // New calcVelocity which generates random ctrl params satisfying the MGPSO stability criteria.
          p <- stdPosition(x, v)
          p2 <- multiEval(envX)(p)
          p3 <- updateVelocity(p2, v)
          p4 <- updateLambda(p3)
          updated <- updatePBestBounds(envX)(p4)
        } yield updated
      }
    }
}
