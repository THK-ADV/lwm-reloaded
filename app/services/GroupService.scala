package services

import java.util.UUID

import models.labwork.Group
import utils.PreferenceSort

import scala.util.Try
import scalaz.StreamT._

sealed trait Strategy {
  def apply(people: Vector[UUID]) = this match {
    case Count(value) =>
      Try(value.toInt) map (count => (people.size / count) + 1)
    case Range(min, max) =>
      def range(min: Int, max: Int, s: Int): Int = (min to max) reduce { (prev, curr) =>
        if (prev % s < curr % s) curr else prev
      }

      for {
        min <- Try(min.toInt)
        max <- Try(max.toInt) if min <= max
      } yield range(min, max, people.size)
  }
}

case class Count(value: String) extends Strategy
case class Range(min: String, max: String) extends Strategy

trait GroupServiceLike {

  def alphabeticalOrdering(amount: Int): List[String] = orderingWith('A')(char => Some((char.toString, (char + 1).toChar)))(amount % 27)

  def orderingWith[A, B](a: A)(v: A => Option[(B, A)]): Int => List[B] = amount => unfold(a)(v).take(amount).toStream.toList

  // THIS RESULT FROM THIS SHOULD `NEVER` BE TRANSFORMED INTO A SET. ORDERING IS CRUCIAL!
  def sortApplicantsFor(labwork: UUID): Try[Vector[UUID]]

  def groupBy(labwork: UUID, strategy: Strategy): Try[Set[Group]]
}

class GroupService(private val applicationService: LabworkApplicationServiceLike) extends GroupServiceLike {

  override def sortApplicantsFor(labwork: UUID): Try[Vector[UUID]] = {
    applicationService.applicationsFor(labwork) map { v =>
      val nodes = v map (app => (app.applicant, app.friends))
      PreferenceSort.sort(nodes)
    }
  }

  override def groupBy(labwork: UUID, strategy: Strategy): Try[Set[Group]] = {
    for {
      people <- sortApplicantsFor(labwork) if people.nonEmpty
      groupSize <- strategy.apply(people)
      grouped = people.grouped(groupSize).toList
      zipped = alphabeticalOrdering(grouped.size) zip grouped
      mapped = zipped map (t => Group(t._1, labwork, t._2.toSet))
    } yield mapped.toSet
  }
}