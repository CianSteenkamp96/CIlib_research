package cilib.research.core

import cilib.{RNG, RVar}
import eu.timepit.refined.api.Refined
import eu.timepit.refined.numeric.Positive
import scalaz.Scalaz._
import scalaz._

sealed trait ArchiveBound
final case class Bounded[A](limit: Int Refined Positive, deletePolicy: List[A] => A)
    extends ArchiveBound
final case class Unbounded() extends ArchiveBound

// In our case
// l is all our solutions
// b is our capacity of 50 with a crowding distance delete policy
// c is a our Dominates insert policy so
//  c(v, x) means "does v dominate x"
//  !c(v, x) means "does v not dominate x"

sealed abstract class Archive[A] {
  import Archive._
  def values: List[A] =
    this match {
      case Empty(_, _)       => List()
      case NonEmpty(l, _, _) => l
    }

  def bound: ArchiveBound =
    this match {
      case Empty(b, _)       => b
      case NonEmpty(_, b, _) => b
    }

// Unnecessary according to Gary
//  def insertCondition: (A, A) => Boolean =
//    this match {
//      case Empty(_, c)       => c
//      case NonEmpty(_, _, c) => c
//    }

//  def insert(v: A): Archive[A] =
//    this match {
//      case Empty(b, c) => NonEmpty[A](List(v), b, c)
//      case NonEmpty(l, b, c) =>
//        b match {
//          case Bounded(limit, deletePolicy) =>
//            // l.forall(x => !c(x, v)) means that there is no element in the list that dominates v
//            if (l.size < limit.value && l.forall(x => !c(x, v))) {
//              removeDominatedAndInsert(v)
//            } else if (l.size == limit.value && l.forall(x => !c(x, v))) {
//              val selected = deletePolicy(l)
//              NonEmpty[A](l.filterNot(x => x.equals(selected)), b, c).removeDominatedAndInsert(v)
//            } else
//              NonEmpty[A](l, b, c)
//          case Unbounded() =>
//            if (l.forall(x => !c(x, v)))
//              removeDominatedAndInsert(v)
//            else
//              NonEmpty[A](l, b, c)
//        }
//    }

// !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! USED BY MGPSO !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
//  protected def removeDominatedAndInsert(v: A): Archive[A] =
//    this match {
//      case Empty(b, c) => NonEmpty[A](List(v), b, c)
//      case NonEmpty(l, b, c) =>
//        val dominated = l.foldLeft(List[A]())((acc, current) => if (c(v, current)) current :: acc else acc)
//        NonEmpty[A](v :: l.filterNot(dominated.contains), b, c)
//    }
// !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! USED BY MGPSO !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
//  def removeDominated(v: A, l: List[A]): List[A] =
//    this match {
//      case Empty(_, _) => List[A]()
//      case NonEmpty(_, _, c) =>
//        val dominated =
//          l.foldLeft(List[A]())((acc, current) => if (c(v, current)) current :: acc else acc)
//        l.filterNot(dominated.contains)
//    }

// !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! PMGPSO insert !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  def insert(v: A): Archive[A] = {
    val rng = RNG.fromTime
    // !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! Need to be changed for each problem's number of objectives !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
//    val shuffled = RVar.shuffle((0 to (3 - 1)).toList.toNel.get).eval(rng).toList
    val shuffled = RVar.shuffle((0 to (5 - 1)).toList.toNel.get).eval(rng).toList
    //    val shuffled = RVar.shuffle((0 to (8 - 1)).toList.toNel.get).eval(rng).toList
    //    val shuffled = RVar.shuffle((0 to (10 - 1)).toList.toNel.get).eval(rng).toList
    //    val shuffled = RVar.shuffle((0 to (15 - 1)).toList.toNel.get).eval(rng).toList
    val random_indices = NonEmptyList(shuffled(0), shuffled(1), shuffled(2))

    this match {
      case Empty(b, c) => NonEmpty[A](List(v), b, c)
      case NonEmpty(l, b, c) =>
        b match {
          case Bounded(limit, deletePolicy) =>
            if (l.size < limit.value && l.forall(current => !c(current, v, random_indices))) {
              NonEmpty[A](v :: l, b, c)
            } else if (l.size == limit.value && l.forall(current => !c(current, v, random_indices))) {
              val selected = deletePolicy(l)
              NonEmpty[A](v :: l.filterNot(x => x.equals(selected)), b, c)
            } else
              NonEmpty[A](l, b, c)
          case Unbounded() =>
            if (l.forall(current => !c(current, v, random_indices)))
              NonEmpty[A](v :: l, b, c)
            else
              NonEmpty[A](l, b, c)
        }
    }
  }

  def empty: Archive[A] =
    this match {
      case Empty(_, _)       => this
      case NonEmpty(_, b, c) => Empty(b, c)
    }

  def isEmpty: Boolean =
    this match {
      case Empty(_, _)       => true
      case NonEmpty(_, _, _) => false
    }

  def size: Int =
    this match {
      case Empty(_, _)       => 0
      case NonEmpty(l, _, _) => l.size
    }
}
object Archive {
    // !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! MGPSO !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
//    private final case class Empty[A](b: ArchiveBound, insertPolicy: (A, A) => Boolean)
    // !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! PMGPSO !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    private final case class Empty[A](b: ArchiveBound, insertPolicy: (A, A, NonEmptyList[Int]) => Boolean)
    extends Archive[A]
    // !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! MGPSO !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
//    private final case class NonEmpty[A](l: List[A], b: ArchiveBound, insertPolicy: (A, A) => Boolean)
//     !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! PMGPSO !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    private final case class NonEmpty[A](l: List[A], b: ArchiveBound, insertPolicy: (A, A, NonEmptyList[Int]) => Boolean)
    extends Archive[A]

  // Let user create bounded instance - Gary ?
  def bounded[A](limit: Int Refined Positive,
                 // !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! MGPSO !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
//                 insertPolicy: (A, A) => Boolean,
                 // !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! For PMGPSO !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
                 insertPolicy: (A, A, NonEmptyList[Int]) => Boolean,
                 deletePolicy: List[A] => A): Archive[A] =
    Empty[A](Bounded(limit, deletePolicy), insertPolicy)

  // !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! MGPSO !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
//  def unbounded[A](insertPolicy: (A, A) => Boolean): Archive[A] =
  // !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! For PMGPSO !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  def unbounded[A](insertPolicy: (A, A, NonEmptyList[Int]) => Boolean): Archive[A] =
    Empty[A](Unbounded(), insertPolicy)
  def boundedNonEmpty[A](seeds: NonEmptyList[A],
                         limit: Int Refined Positive,
                         // !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! MGPSO !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
//                          insertPolicy: (A, A) => Boolean,
                         // !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! For PMGPSO !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
                         insertPolicy: (A, A, NonEmptyList[Int]) => Boolean,
                         deletePolicy: List[A] => A): Archive[A] = {
    val emptyArchive: Archive[A] = bounded(limit, insertPolicy, deletePolicy)
    seeds.foldLeft(emptyArchive)((archive, seed) => archive.insert(seed))
  }
  // !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! MGPSO !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
//  def unboundedNonEmpty[A](seeds: NonEmptyList[A], insertPolicy: (A, A) => Boolean): Archive[A] = {
  // !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! PMGPSO !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  def unboundedNonEmpty[A](seeds: NonEmptyList[A], insertPolicy: (A, A, NonEmptyList[Int]) => Boolean): Archive[A] = {
    val emptyArchive: Archive[A] = unbounded(insertPolicy)
    seeds.foldLeft(emptyArchive)((archive, seed) => archive.insert(seed))
  }
}