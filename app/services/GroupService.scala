package services

import java.util.UUID

import models.{PostgresGroup, PostgresLabworkApplication, SesameGroup}
import utils.PreferenceSort

import scala.util.{Failure, Try}
import scalaz.StreamT._

sealed trait GroupingStrategy {

  @scala.annotation.tailrec
  private def go(min: Int, max: Int, limit: Option[Int], p: Vector[UUID]): List[Vector[UUID]] = {
    (p.size % min, p.size / min) match {
      case (m, d) if m == d =>
        p.grouped(min + 1).toList
      case (m, d) if m < d =>
        val g = p.grouped(min).toList

        if (limit.contains(g.size)) {
          g
        } else {
          g.take(g.size - 1).zipAll(g.last.map(Option.apply), Vector.empty, None).map {
            case (list, Some(toAdd)) => list.+:(toAdd)
            case (list, None) => list
          }
        }
      case (m, d) if m > d =>
        go(min + 1, max, limit, p)
      case _ =>
        List.empty
    }
  }

  def apply(people: Vector[UUID]): List[Vector[UUID]] = this match {
    case CountGrouping(value) =>
      val count = value.toInt
      val min = people.size / count
      val max = min + 1

      go(min, max, Some(count), people)

    case RangeGrouping(min, max) => go(min.toInt, max.toInt, None, people)
  }
}

case class CountGrouping(value: String) extends GroupingStrategy
case class RangeGrouping(min: String, max: String) extends GroupingStrategy

trait GroupServiceLike {

  def alphabeticalOrdering(amount: Int): List[String] = orderingWith('A')(char => Some((char.toString, (char + 1).toChar)))(amount % 27)

  def orderingWith[A, B](a: A)(v: A => Option[(B, A)]): Int => List[B] = amount => unfold(a)(v).take(amount).toStream.toList

  // THIS RESULT FROM THIS SHOULD `NEVER` BE TRANSFORMED INTO A SET. ORDERING IS CRUCIAL!
  def sortApplicantsFor(labwork: UUID): Try[Vector[UUID]]

  def groupBy(labwork: UUID, strategy: GroupingStrategy): Try[Set[SesameGroup]]
}

class GroupService(private val applicationService: LabworkApplicationServiceLike) extends GroupServiceLike {

  override def sortApplicantsFor(labwork: UUID): Try[Vector[UUID]] = {
    applicationService.applicationsFor(labwork) map { v =>
      val nodes = v map (app => (app.applicant, app.friends))
      PreferenceSort.sort(nodes)
    }
  }

  override def groupBy(labwork: UUID, strategy: GroupingStrategy): Try[Set[SesameGroup]] = {
    /*for {
      people <- sortApplicantsFor(labwork) if people.nonEmpty
      groupSize <- strategy.apply(people)
      grouped = people.grouped(groupSize).toList
      zipped = alphabeticalOrdering(grouped.size) zip grouped
      mapped = zipped map (t => SesameGroup(t._1, labwork, t._2.toSet))
    } yield mapped.toSet*/

    Failure(new Exception("not implemented"))
  }
}

object GroupService {

  def alphabeticalOrdering(amount: Int): List[String] = orderingWith('A')(char => Some((char.toString, (char + 1).toChar)))(amount % 27)

  def orderingWith[A, B](a: A)(v: A => Option[(B, A)]): Int => List[B] = amount => unfold(a)(v).take(amount).toStream.toList

  // THIS RESULT FROM THIS SHOULD `NEVER` BE TRANSFORMED INTO A SET. ORDERING IS CRUCIAL!
  def sort(applicants: Vector[PostgresLabworkApplication]): Vector[UUID] = {
    val nodes = applicants map (app => (app.applicant, app.friends))
    PreferenceSort.sort(nodes)
  }

  def groupApplicantsBy(strategy: GroupingStrategy, applicants: Vector[PostgresLabworkApplication], labwork: UUID): Vector[PostgresGroup] = {
    val people = sort(applicants)
    val grouped = strategy.apply(people)
    val zipped = alphabeticalOrdering(grouped.size) zip grouped

    zipped.map(t => PostgresGroup(t._1, labwork, t._2.toSet)).toVector
  }
}