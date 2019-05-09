package service

import java.util.UUID

import models.{Group, LabworkApplication}
import utils.PreferenceSort

object GroupService {

  private val alphabetLetters = 26

  private def alphabet: List[String] = 'A' to 'Z' map (_.toString) toList

  private def zipWithAlphabet(people: List[Vector[UUID]]) = alphabeticalOrdering(people.size) zip people

  def alphabeticalOrdering(amount: Int): List[String] = {
    def go(amount: Int, suffixLevel: Int): List[String] = {
      val letters = alphabet take amount
      val maybeSuffixed = if (suffixLevel == 0) letters else letters map (c => s"$c-$suffixLevel")
      val remaining = amount - alphabetLetters

      if (remaining > 0) maybeSuffixed ++ go(remaining, suffixLevel + 1) else maybeSuffixed
    }

    go(amount, 0)
  }

  // THIS RESULT FROM THIS SHOULD `NEVER` BE TRANSFORMED INTO A SET. ORDERING IS CRUCIAL!
  def sort(applicants: Vector[LabworkApplication]): Vector[UUID] = {
    val nodes = applicants map (app => (app.applicant, app.friends))
    PreferenceSort.sort(nodes)
  }

  def groupApplicantsBy(strategy: GroupingStrategy)(applicants: Vector[LabworkApplication], labwork: UUID): Vector[Group] = {
    (sort _ andThen strategy.group andThen zipWithAlphabet) (applicants)
      .map(t => Group(t._1, labwork, t._2.toSet)).toVector
  }
}