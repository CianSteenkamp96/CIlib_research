// Cian Steenkamp

package cilib.research.mgpso

import scalaz.NonEmptyList
import scalaz.Scalaz._
// http://ejml.org/javadoc/org/ejml/simple/SimpleMatrix.html
import org.ejml.simple.SimpleMatrix // for nullspace calc

object KneePoint {
  def apply(archiveCollection: NonEmptyList[MGParticle],
            R: NonEmptyList[Double]): Option[MGParticle] = {
    // if archive only contains a single solution - note a check for an empty archive is done before this is even called
    if (archiveCollection.size == 1) return Some(archiveCollection.head)

    // archive guide selected as winner of binary tournie (whether KP or not)
    val centres: List[(List[Double], Int)] =
      archiveCollection.toList.zipWithIndex.take(2).map(tmp => (tmp._1.pos.fitness.toList, tmp._2))

    val fitnessValues: List[List[Double]] = archiveCollection.toList.map(x => x.pos.fitness.toList)
    val numObjectives: Int = fitnessValues.head.size

    // Create a list of extreme points (i.e. the least fit solutions for each objective) E
    val E: List[List[Double]] =
      fitnessValues.transpose.map(_.zipWithIndex.maxBy(_._1)._2).map(i => fitnessValues(i))

    assert(E.size == numObjectives)

    // KP Condition #0
    // if either of the tournament solutions are extremal then return that one; i.e., it is considered a kp
    // if both of the tournament solutions are extremal then return either randomly
    val kp_cond0 =
      if (E.indexOf(centres(0)._1) != -1 && E.indexOf(centres(1)._1) == -1)
        Some(archiveCollection.toList(centres(0)._2))
      else if (E.indexOf(centres(0)._1) == -1 && E.indexOf(centres(1)._1) != -1)
        Some(archiveCollection.toList(centres(1)._2))
      else if (E.indexOf(centres(0)._1) != -1 && E.indexOf(centres(1)._1) != -1)
        Some(archiveCollection.toList(centres(0 + scala.util.Random.nextInt((1 - 0) + 1))._2))
      else None
    if (kp_cond0 != None) return kp_cond0

    // Construct hyperplane H (PQR), of the form ax + by + cz = d (a(x-x0) + b(y-y0) + c(z-z0) = 0), as follows:
    // (where (a, b, c) is the normal vector, _n, perpendicular to the (hyper)plane and (x0, y0, z0) is a point on the (hyper)plane)
    // Create M - 1 distinct VECTORS using E
    // That is, create a matrix G (where each row of G is a calculated vector)
    val G: List[List[Double]] = E.tail.map(el => {
      (0 to (el.size - 1)).map(i => el(i) - E.head(i)).toList
    })

    // Compute the null space of G, using the result to determine the constants of the equation to the hyperplane H.
    // See MSc HD/...1/reading_material/scala/SVD_nullspace.pdf
    val nullspace: SimpleMatrix =
      new SimpleMatrix(G.size, G.head.size, true, G.flatten.toArray).svd.getV;
    val consts: List[Double] =
      (0 to (nullspace.numRows() - 1)).map(i => nullspace.get(i, G.head.size - 1)).toList
    val d: Double = (consts, E.head).zipped.map(_ * _) sum // Subs point (E.head) to get d
    // ax + by + cz + ... = d
    // H = List(a, b, c, ..., d)
    val H = d :: consts.reverse reverse // last element is d

    assert(H.size == numObjectives + 1)

    // Check which particles of the archive are inside neighbourhood of the centre particles (tournament solutions) and then determine archive guide ...
    // R
    //   |
    //   * -
    // http://mathcentral.uregina.ca/QQ/database/QQ.09.10/h/ali1.html
    val insideIndices: List[List[Int]] = centres.map(
      ci =>
        fitnessValues.zipWithIndex
          .map(ai =>
            (0 to ai._1.size - 1).toList.map(i =>
              if (math.abs(ai._1(i) - ci._1(i)) < R.toList(i)) ai._2 else -1))
          .filterNot(_.contains(-1))
          .map(_.head)
          .filterNot(centres.map(_._2).contains))

    assert(insideIndices.size == 2)

    // isolated solutions considered KPs
    if (insideIndices(0).size == 0 && insideIndices(1).size >= 0)
      Some(archiveCollection.toList(centres(0)._2))
    // isolated solutions considered KPs
    else if (insideIndices(0).size >= 0 && insideIndices(1).size == 0)
      Some(archiveCollection.toList(centres(1)._2))
    // if either solution = KP, return KP;
    // if neither or both = KP, return sol with max distance to extremal hyperplane;
    // if both = KP and same max distance to extremal hyperplane, return one sol randomly;
    // if neither = KP and same max distance to extremal hyperplane, return least crowded of the two particles.
    val N1: List[List[Double]] = centres(0)._1 :: insideIndices(0).map(i => fitnessValues(i))
    val N2: List[List[Double]] = centres(1)._1 :: insideIndices(1).map(i => fitnessValues(i))

    // returns index of KPcandidates for candidate with max distance to H
    def distance2H(KPcandidates: List[List[Double]], H: List[Double]) = {
      val denom: Double = math.sqrt(H.reverse.tail.map(x => x * x).sum)
      // return distances to H
      KPcandidates.map(
        candidate =>
          math.abs(
            (H.reverse.tail.reverse
              .zip(candidate)
              .map(el => el._1 * el._2) sum) - H.reverse.head) / denom)
    }

    val N1ds = distance2H(N1, H)
    assert(N1.size == N1ds.size)
    val N1max = N1ds.max

    val N2ds = distance2H(N2, H)
    assert(N2.size == N2ds.size)
    val N2max = N2ds.max

    // KP Condition #1
    // if either solution max distance to H, return sol as KP;
    val kp_cond1: Option[MGParticle] =
      if (N1ds.head == N1max && N2ds.head != N2max) Some(archiveCollection.toList(centres(0)._2))
      else if (N1ds.head != N1max && N2ds.head == N2max)
        Some(archiveCollection.toList(centres(1)._2))
      else None
    if (kp_cond1 != None) return kp_cond1
    // if neither or both = KP, return sol with max distance to extremal hyperplane;
    // if both = KP and same max distance to extremal hyperplane, return one sol randomly;
    val kp_cond2: Option[MGParticle] =
      if ((N1ds.head == N1max && N2ds.head == N2max) || (N1ds.head != N1max && N2ds.head != N2max))
        if (N1ds.head > N2ds.head) Some(archiveCollection.toList(centres(0)._2))
        else if (N1ds.head < N2ds.head) Some(archiveCollection.toList(centres(1)._2))
        else if (N1ds.head == N2ds.head)
          Some(archiveCollection.toList(centres(0 + scala.util.Random.nextInt((1 - 0) + 1))._2))
        else None
      else None
    if (kp_cond2 != None) return kp_cond2
    // if neither = max distance to H and same max distance to extremal hyperplane H, return least crowded of the two particles.
    None // Most crowded calced if None returned
    // Note that due to the way that '.take' works and the fact that we 'shuffle' the archive before this is executed in MGPSO.scala, therefore, choosing 2 'new' sols for the crowding distance tournament will actually use the same tournament participants as taken here.
  }

  def kneePoint(collection: NonEmptyList[MGParticle], R: NonEmptyList[Double]) =
    apply(collection, R)
}
