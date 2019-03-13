package services

import java.util.UUID

import models.{Group, LabworkApplication}
import utils.PreferenceSort

import scalaz.StreamT._

object GroupService { // TODO DI

  def alphabeticalOrdering(amount: Int): List[String] = orderingWith('A')(char => Some((char.toString, (char + 1).toChar)))(amount % 27)

  def orderingWith[A, B](a: A)(v: A => Option[(B, A)]): Int => List[B] = amount => unfold(a)(v).take(amount).toStream.toList

  // THIS RESULT FROM THIS SHOULD `NEVER` BE TRANSFORMED INTO A SET. ORDERING IS CRUCIAL!
  def sort(applicants: Vector[LabworkApplication]): Vector[UUID] = {
    val nodes = applicants map (app => (app.applicant, app.friends))
    PreferenceSort.sort(nodes)
  }

  def groupApplicantsBy(strategy: GroupingStrategy, applicants: Vector[LabworkApplication], labwork: UUID): Vector[Group] = {
    val people = sort(applicants)
    val grouped = strategy.apply(people)
    val zipped = alphabeticalOrdering(grouped.size) zip grouped

    zipped.map(t => Group(t._1, labwork, t._2.toSet)).toVector
  }
}