// Cian Steenkamp

package cilib.research.mgpso

import scalaz.NonEmptyList
import scalaz.Scalaz._
// for nullspace calc
// http://ejml.org/javadoc/org/ejml/simple/SimpleMatrix.html
import org.ejml.simple.SimpleMatrix

object KneePoint {
  // returns a kp or the number of kps if all == true
  def apply(archiveCollection: NonEmptyList[MGParticle],
            R: NonEmptyList[Double],
            all: Boolean): (Option[MGParticle], Option[Int]) = {
    // NB Note: extreme solutions are not counted when calculating the num KPs
    if (all) { // return num KPs
      if (archiveCollection.size == 1) return (None, Some(1))

      // consider all in order to count number of KPs
      val centres: List[(List[Double], Int)] =
        archiveCollection.toList.zipWithIndex.map(x => (x._1.pos.fitness.toList, x._2))

      val fitnessValues: List[List[Double]] =
        archiveCollection.toList.map(x => x.pos.fitness.toList)
      val numObjectives: Int = fitnessValues.head.size

      // Create a list of extreme points (i.e. the least fit solutions for each objective) E
      val E: List[List[Double]] =
        fitnessValues.transpose.map(_.zipWithIndex.maxBy(_._1)._2).map(i => fitnessValues(i))

      val G: List[List[Double]] = E.tail.map(el => {
        el.indices.map(i => el(i) - E.head(i)).toList
      })

      // Compute the null space of G, using the result to determine the constants of the equation to the hyperplane H.
      // See MSc HD/...1/reading_material/scala/SVD_nullspace.pdf
      val nullspace: SimpleMatrix =
        new SimpleMatrix(G.size, G.head.size, true, G.flatten.toArray).svd.getV
      val consts: List[Double] =
        (0 until nullspace.numRows()).map(i => nullspace.get(i, G.head.size - 1)).toList
      val d: Double = (consts, E.head).zipped.map(_ * _) sum // Subs point (E.head) to get d
      // ax + by + cz + ... = d
      // H = List(a, b, c, ..., d)
      val H = d :: consts.reverse reverse // last element is d

      assert(H.size == numObjectives + 1)

      val tmp: List[List[Int]] = centres.map(
        ci =>
          centres
            .map(ai =>
              ai._1.indices.toList.map(i =>
                if (math.abs(ai._1(i) - ci._1(i)) < R.toList(i)) ai._2 else -1))
            .filterNot(_.contains(-1))
            .map(_.head)
      ) // indices of neighbours for all solutions

      // remove each considered solution from its own neighbourhood (prepended again later)
      val insideIndices = tmp.indices.toList.map(i => tmp(i).filter(_ != i))

      assert(insideIndices.size == centres.size)

      // return distances to H
      def distance2H(KPcandidates: List[List[Double]], H: List[Double]): List[Double] = {
        val denom: Double = math.sqrt(H.reverse.tail.map(x => x * x).sum)
        KPcandidates.map(
          candidate =>
            math.abs(
              (H.reverse.tail.reverse
                .zip(candidate)
                .map(el => el._1 * el._2) sum) - H.reverse.head) / denom)
      }

      // combine (put in list) each solution with solutions also in its neighbourhood
      val Ns: List[List[List[Double]]] = centres.indices.toList.map(i =>
        centres(i)._1 :: insideIndices(i).map(j => fitnessValues(j)))
      assert(Ns.size == fitnessValues.size)

      // neighbourhood distances (archive distances since all are considered - fitnessValues)
      val NsDs: List[List[Double]] = Ns.map(n => distance2H(n, H))
      assert(NsDs.size == fitnessValues.size)

      val NsMaxes: List[Double] = NsDs.map(n => n.max)
      assert(NsMaxes.size == fitnessValues.size)
      val NsDs_NsMaxes: List[(List[Double], Double)] = NsDs.zip(NsMaxes)

      // if solution has max distance to extremal hyperplane within its neighbourhood count as KP
      val numKPs: Int =
        NsDs_NsMaxes.foldLeft(0)((acc, d_m) => if (d_m._1.head == d_m._2) acc + 1 else acc)

      (None, Some(numKPs))
    } else { // return a KP
      // if archive only contains a single solution - note a check for an empty archive is done before this is even called
      if (archiveCollection.size == 1) return (Some(archiveCollection.head), None)

      // archive guide selected as winner of binary tournie (whether KP or not)
      val centres: List[(List[Double], Int)] =
        archiveCollection.toList.zipWithIndex
          .take(2)
          .map(tmp => (tmp._1.pos.fitness.toList, tmp._2))

      val fitnessValues: List[List[Double]] =
        archiveCollection.toList.map(x => x.pos.fitness.toList)
      val numObjectives: Int = fitnessValues.head.size

      // Create a list of extreme points (i.e. the least fit solutions for each objective) E
      val E: List[List[Double]] =
        fitnessValues.transpose.map(_.zipWithIndex.maxBy(_._1)._2).map(i => fitnessValues(i))

      // assert same number of extreme solutions as the number of objectives (one extreme sol per obj)
      assert(E.size == numObjectives)

      // KP Condition #0_0
      // if either of the tournament solutions are extremal, then return that extreme solution; i.e., it is considered a kp
      // if both of the tournament solutions are extremal then return either randomly
      val kp_cond0: Option[MGParticle] =
        if (E.indexOf(centres.head._1) != -1 && E.indexOf(centres(1)._1) == -1)
          Some(archiveCollection.toList(centres.head._2))
        else if (E.indexOf(centres.head._1) == -1 && E.indexOf(centres(1)._1) != -1)
          Some(archiveCollection.toList(centres(1)._2))
        else if (E.indexOf(centres.head._1) != -1 && E.indexOf(centres(1)._1) != -1)
          Some(archiveCollection.toList(centres(scala.util.Random.nextInt(2))._2))
        else None
      if (kp_cond0.isDefined) return (kp_cond0, None)

      // Construct hyperplane H (PQR), of the form ax + by + cz = d (i.e. a(x-x0) + b(y-y0) + c(z-z0) = 0), as follows:
      // (where (a, b, c) is the normal vector, _n, perpendicular to the (hyper)plane and (x0, y0, z0) is a point on the (hyper)plane)
      // Create M - 1 distinct VECTORS using E
      // That is, create a matrix G (where each row of G is a calculated vector)
      val G: List[List[Double]] = E.tail.map(el => {
        el.indices.map(i => el(i) - E.head(i)).toList
      })

      // Compute the null space of G, using the result to determine the constants of the equation to the hyperplane H.
      // See MSc HD/...1/reading_material/scala/SVD_nullspace.pdf
      val nullspace: SimpleMatrix =
        new SimpleMatrix(G.size, G.head.size, true, G.flatten.toArray).svd.getV
      val consts: List[Double] =
        (0 until nullspace.numRows()).map(i => nullspace.get(i, G.head.size - 1)).toList
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
              ai._1.indices.toList.map(i =>
                if (math.abs(ai._1(i) - ci._1(i)) < R.toList(i)) ai._2 else -1))
            .filterNot(_.contains(-1))
            .map(_.head)
            .filterNot(centres.map(_._2).contains))

      assert(insideIndices.size == centres.size) // centres.size => 2

      // KP Condition #0_1
      // isolated solutions considered KPs
      if (insideIndices.head.isEmpty && insideIndices(1).size >= 0)
        (Some(archiveCollection.toList(centres.head._2)), None)
      // isolated solutions considered KPs
      else if (insideIndices.head.size >= 0 && insideIndices(1).isEmpty)
        (Some(archiveCollection.toList(centres(1)._2)), None)

      val N1: List[List[Double]] = centres.head._1 :: insideIndices.head.map(i => fitnessValues(i))
      val N2: List[List[Double]] = centres(1)._1 :: insideIndices(1).map(i => fitnessValues(i))

      // return distances to H
      def distance2H(KPcandidates: List[List[Double]], H: List[Double]): List[Double] = {
        val denom: Double = math.sqrt(H.reverse.tail.map(x => x * x).sum)
        KPcandidates.map(
          candidate =>
            math.abs(
              (H.reverse.tail.reverse
                .zip(candidate)
                .map(el => el._1 * el._2) sum) - H.reverse.head) / denom)
      }

      val N1ds: List[Double] = distance2H(N1, H)
      assert(N1.size == N1ds.size)
      val N1max: Double = N1ds.max

      val N2ds: List[Double] = distance2H(N2, H)
      assert(N2.size == N2ds.size)
      val N2max: Double = N2ds.max

      // KP Condition #1
      // if either solution max distance to H, return sol as KP;
      val kp_cond1: Option[MGParticle] =
        if (N1ds.head == N1max && N2ds.head != N2max)
          Some(archiveCollection.toList(centres.head._2))
        else if (N1ds.head != N1max && N2ds.head == N2max)
          Some(archiveCollection.toList(centres(1)._2))
        else None
      if (kp_cond1.isDefined) return (kp_cond1, None)

      // KP Condition #2
      // if neither or both = KP, return sol with max distance to extremal hyperplane;
      // if both = KP and same max distance to extremal hyperplane, return one sol randomly;
      val kp_cond2: Option[MGParticle] =
        if ((N1ds.head == N1max && N2ds.head == N2max) || (N1ds.head != N1max && N2ds.head != N2max))
          if (N1ds.head > N2ds.head) Some(archiveCollection.toList(centres.head._2))
          else if (N1ds.head < N2ds.head) Some(archiveCollection.toList(centres(1)._2))
          else if (N1ds.head == N2ds.head)
            Some(archiveCollection.toList(centres(scala.util.Random.nextInt(2))._2))
          else None
        else None
      if (kp_cond2.isDefined) return (kp_cond2, None)

      // KP Condition #3
      // if neither = max distance to H and same max distance to extremal hyperplane H, return None (actually least crowded of the two particles returned after function return for KnMGPSO).
      (None, None) // Most crowded calced if None returned
      // Note that due to the way that '.take' works and the fact that we 'shuffle' the archive before this is executed in MGPSO.scala, therefore, choosing 2 'new' sols for the crowding distance tournament will actually use the same tournament participants as taken here.
    }
  }

  def kneePoint(collection: NonEmptyList[MGParticle],
                R: NonEmptyList[Double],
                all: Boolean = false): (Option[MGParticle], Option[Int]) =
    apply(collection, R, all)
}

///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
//// Cian Steenkamp
//// Knee Point Calculation
//// Intellij Scratch File Test

//import org.ejml.simple.SimpleMatrix
//import scalaz._
//import Scalaz._
//
//val R: NonEmptyList[Double] = NonEmptyList(10.0, 10.0, 10.0)
////val R: NonEmptyList[Double] = NonEmptyList(30.0, 30.0, 30.1)
////val R: NonEmptyList[Double] = NonEmptyList(30.0, 30.0, 0.1)
//
//val fitnessValues: List[List[Double]] = List(List(1.0, 2.0, 3.0),
//List(11.0, 4.0, 5.0),
//List(6.0, 12.0, 7.0),
//List(8.0, 9.0, 13.0),
//List(10.0, 10.0, 10.0))
//val numObjectives: Int = fitnessValues.head.size
//val E: List[List[Double]] =
//fitnessValues.transpose.map(_.zipWithIndex.maxBy(_._1)._2).map(i => fitnessValues(i))
//
//assert(E.size == numObjectives)
//
//val centres: List[(List[Double], Int)] = fitnessValues.zipWithIndex.take(2)
//
//val G: List[List[Double]] = E.tail.map(el => {
//el.indices.map(i => el(i) - E.head(i)).toList
//})
//
//val nullspace: SimpleMatrix =
//new SimpleMatrix(G.size, G.head.size, true, G.flatten.toArray).svd.getV
//val consts: List[Double] =
//(0 until nullspace.numRows()).map(i => nullspace.get(i, G.head.size - 1)).toList
//val d: Double = (consts, E.head).zipped.map(_ * _) sum
//
//val H = d :: consts.reverse reverse
//
//// comp H: eq2: 0.84613 x+0.53275 y-0.01567 z=11.36006
//// with answer of
//// https://onlinemschool.com/math/assistance/cartesian_coordinate/plane/ when passing E: eq1: 54 x+34 y-z-725=0
//// on https://onlinemschool.com/math/assistance/cartesian_coordinate/plane/
//// planes (should) lie on each other - that is, the same plane., the plane equations are identical.
//
//val insideIndices: List[List[Int]] = centres.map(
//ci =>
//fitnessValues.zipWithIndex
//.map(ai =>
//ai._1.indices.toList.map(i =>
//if (math.abs(ai._1(i) - ci._1(i)) < R.toList(i)) ai._2 else -1))
//.filterNot(_.contains(-1))
//.map(_.head)
//.filterNot(centres.map(_._2).contains)
//)
//
//assert(insideIndices.size == 2)
//
//val N1: List[List[Double]] = centres.head._1 :: insideIndices.head.map(i => fitnessValues(i))
//val N2: List[List[Double]] = centres(1)._1 :: insideIndices(1).map(i => fitnessValues(i))
//
//// return distances to H
//def distance2H(KPcandidates: List[List[Double]], H: List[Double]): List[Double] = {
//val denom: Double = math.sqrt(H.reverse.tail.map(x => x * x).sum)
//KPcandidates.map(
//candidate =>
//math.abs(
//(H.reverse.tail.reverse
//.zip(candidate)
//.map(el => el._1 * el._2) sum) - H.reverse.head) / denom)
//}
//
//val N1ds: List[Double] = distance2H(N1, H)
//assert(N1.size == N1ds.size)
//// comp dist fro point to plane https://onlinemschool.com/math/assistance/cartesian_coordinate/p_plane/
//val N1max: Double = N1ds.max
//
//val N2ds: List[Double] = distance2H(N2, H)
//assert(N2.size == N2ds.size)
//val N2max: Double = N2ds.max/
///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
//// Cian Steenkamp
//// Number of Knee Points Calculation
//// Intellij Scratch File Test

//import org.ejml.simple.SimpleMatrix
//import scalaz._
//import Scalaz._
//
////val R: NonEmptyList[Double] = NonEmptyList(10.0, 10.0, 10.0)
////val R: NonEmptyList[Double] = NonEmptyList(30.0, 30.0, 30.1)
////  val R: NonEmptyList[Double] = NonEmptyList(30.0, 30.0, 0.1)
//val R: NonEmptyList[Double] = NonEmptyList(1.5, 1.5, 1.5)
//
//val fitnessValues: List[List[Double]] = List(
//List(0.0, 0.0, 0.0), // 0
//List(0.0, 0.0, 1.0), // 1
//List(0.0, 1.0, 0.0), // 2
//List(0.0, 1.0, 1.0), // 3
//List(1.0, 0.0, 0.0), // 4
//List(1.0, 0.0, 1.0), // 5
//List(1.0, 1.0, 0.0), // 6
//List(1.0, 1.0, 1.0), // 7
//List(1.0, 1.0, 1.5), // 8
//List(1.0, 2.0, 3.0), // 9
//List(2.0, 2.0, 2.0), // 10
//List(1.0, 1.0, 2.0), // 11
//List(1.0, 1.0, 1.0), // 12
//List(11.0, 4.0, 5.0), // 13
//List(6.0, 12.0, 7.0), // 14
//List(8.0, 9.0, 13.0), // 15
//List(10.0, 10.0, 10.0) // 16
//)
//
////if (archiveCollection.size == 1) return (None, Some(1))
//
//// consider all in order to count number of KPs
//val centres: List[(List[Double], Int)] =
////    List((List(0.0, 0.0, 0.0), 0), (List(0.0, 0.0, 1.0), 1))
//fitnessValues.toList.zipWithIndex
////	archiveCollection.toList.zipWithIndex.map(x => (x._1.pos.fitness.toList, x._2))
//
////val fitnessValues: List[List[Double]] =
////	archiveCollection.toList.map(x => x.pos.fitness.toList)
//val numObjectives: Int = fitnessValues.head.size
//
//// Create a list of extreme points (i.e. the least fit solutions for each objective) E
//val E: List[List[Double]] =
//fitnessValues.transpose.map(_.zipWithIndex.maxBy(_._1)._2).map(i => fitnessValues(i))
//
//val G: List[List[Double]] = E.tail.map(el => {
//el.indices.map(i => el(i) - E.head(i)).toList
//})
//
//// Compute the null space of G, using the result to determine the constants of the equation to the hyperplane H.
//// See MSc HD/...1/reading_material/scala/SVD_nullspace.pdf
//val nullspace: SimpleMatrix =
//new SimpleMatrix(G.size, G.head.size, true, G.flatten.toArray).svd.getV
//val consts: List[Double] =
//(0 until nullspace.numRows()).map(i => nullspace.get(i, G.head.size - 1)).toList
//val d: Double = (consts, E.head).zipped.map(_ * _) sum // Subs point (E.head) to get d
//// ax + by + cz + ... = d
//// H = List(a, b, c, ..., d)
//val H = d :: consts.reverse reverse // last element is d
//
//assert(H.size == numObjectives + 1)
//
//// Check which particles of the archive are inside neighbourhood of the centre particles (tournament solutions) and then determine archive guide ...
//// R
////   |
////   * -
//// http://mathcentral.uregina.ca/QQ/database/QQ.09.10/h/ali1.html
//val tmp: List[List[Int]] = centres.map(
//ci =>
//fitnessValues.zipWithIndex
//.map(ai =>
//ai._1.indices.toList.map(i =>
//if (math.abs(ai._1(i) - ci._1(i)) < R.toList(i)) ai._2 else -1))
//.filterNot(_.contains(-1))
//.map(_.head)
//) // indices of neighbours for all solutions
//
//// remove each considered solution from its own neighbourhood (prepended again later)
//val insideIndices = tmp.indices.toList.map(i => tmp(i).filter(_ != i))
//
//assert(insideIndices.size == centres.size)
//
//// return distances to H
//def distance2H(KPcandidates: List[List[Double]], H: List[Double]): List[Double] = {
//val denom: Double = math.sqrt(H.reverse.tail.map(x => x * x).sum)
//KPcandidates.map(
//candidate =>
//math.abs(
//(H.reverse.tail.reverse
//.zip(candidate)
//.map(el => el._1 * el._2) sum) - H.reverse.head) / denom)
//}
//
//// combine (put in list) each solution with solutions also in its neighbourhood
//val Ns: List[List[List[Double]]] =
//centres.indices.toList.map(i => centres(i)._1 :: insideIndices(i).map(j => fitnessValues(j)))
//assert(Ns.size == fitnessValues.size)
//
//// neighbourhood distances (archive distances since all are considered i.e. all fitnessValues)
//val NsDs: List[List[Double]] = Ns.map(n => distance2H(n, H))
//assert(NsDs.size == fitnessValues.size)
//
//val NsMaxes: List[Double] = NsDs.map(n => n.max)
//assert(NsMaxes.size == fitnessValues.size)
//val NsDs_NsMaxes: List[(List[Double], Double)] = NsDs.zip(NsMaxes)
//
//// if solution has max distance to extremal hyperplane within its neighbourhood count as KP
//val numKPs: Int =
//NsDs_NsMaxes.foldLeft(0)((acc, d_m) => if (d_m._1.head == d_m._2) acc + 1 else acc)
//
//numKPs
///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
