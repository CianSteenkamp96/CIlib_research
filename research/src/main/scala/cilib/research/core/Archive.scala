package cilib.research.core

import eu.timepit.refined.api.Refined
import eu.timepit.refined.numeric.Positive
import scalaz.Scalaz._
import scalaz._

sealed trait ArchiveBound
final case class Bounded[A](limit: Int Refined Positive, deletePolicy: List[A] => A)
    extends ArchiveBound
final case class Unbounded() extends ArchiveBound

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

  def insertCondition: (A, A) => Boolean =
    this match {
      case Empty(_, c)       => c
      case NonEmpty(_, _, c) => c
    }

  def insert(v: A): Archive[A] =
    this match {
      case Empty(b, c) => NonEmpty[A](List(v), b, c)
      case NonEmpty(l, b, c) =>
        b match {
          case Bounded(limit, deletePolicy) =>
            if (l.size < limit.value && l.foldLeft(true)((a, current) => a && (!c(current, v)))) {
              NonEmpty[A](v :: l, b, c)
            } else if (l.size == limit.value && l.foldLeft(true)((a, current) =>
                         a && (!c(current, v)))) {
              val selected = deletePolicy(l)
              NonEmpty[A](v :: l.filterNot(x => x.equals(selected)), b, c)
            } else
              NonEmpty[A](l, b, c)
          case Unbounded() =>
            if (l.foldLeft(true)((a, current) => a && (!c(current, v))))
              NonEmpty[A](v :: l, b, c)
            else
              NonEmpty[A](l, b, c)
        }
    }

  def removeDominated(v: A, l: List[A]): List[A] =
    this match {
      case Empty(_, _) => List[A]()
      case NonEmpty(_, _, c) => {
        val dominated =
          l.foldLeft(List[A]())((acc, current) => if (c(v, current)) current :: acc else acc)
        l.filterNot(dominated.contains)
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
  private final case class Empty[A](b: ArchiveBound, insertPolicy: (A, A) => Boolean)
      extends Archive[A]
  private final case class NonEmpty[A](l: List[A], b: ArchiveBound, insertPolicy: (A, A) => Boolean)
      extends Archive[A]
  def bounded[A](limit: Int Refined Positive,
                 insertPolicy: (A, A) => Boolean,
                 deletePolicy: List[A] => A): Archive[A] =
    Empty[A](Bounded(limit, deletePolicy), insertPolicy)
  def unbounded[A](insertPolicy: (A, A) => Boolean): Archive[A] =
    Empty[A](Unbounded(), insertPolicy)
  def boundedNonEmpty[A](seeds: NonEmptyList[A],
                         limit: Int Refined Positive,
                         insertPolicy: (A, A) => Boolean,
                         deletePolicy: List[A] => A): Archive[A] = {
    val emptyArchive: Archive[A] = bounded(limit, insertPolicy, deletePolicy)
    seeds.foldLeft(emptyArchive)((archive, seed) => archive.insert(seed))
  }
  def unboundedNonEmpty[A](seeds: NonEmptyList[A], insertPolicy: (A, A) => Boolean): Archive[A] = {
    val emptyArchive: Archive[A] = unbounded(insertPolicy)
    seeds.foldLeft(emptyArchive)((archive, seed) => archive.insert(seed))
  }
}
