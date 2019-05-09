package service

import java.util.UUID

import models.{Group, LabworkApplication}
import utils.PreferenceSort

object GroupService {

  private val alphabetLetters = 26

  private def alphabet: List[String] = 'A' to 'Z' map (_.toString) toList

  def alphabeticalOrdering(amount: Int): List[String] = amount match {
    case letters if (1 to alphabetLetters) contains letters => alphabet take letters
    case lettersSuffixed =>
      val suffixed = for {
        (seq, i) <- (0 until lettersSuffixed).toList.grouped(alphabetLetters).zipWithIndex
        letter <- if (i == 0) alphabet else seq.zip(alphabet).map(t => s"${t._2}-$i")
      } yield letter

      suffixed toList
  }

  // THIS RESULT FROM THIS SHOULD `NEVER` BE TRANSFORMED INTO A SET. ORDERING IS CRUCIAL!
  def sort(applicants: Vector[LabworkApplication]): Vector[UUID] = {
    val nodes = applicants map (app => (app.applicant, app.friends))
    PreferenceSort.sort(nodes)
  }

  def groupApplicantsBy(strategy: GroupingStrategy)(applicants: Vector[LabworkApplication], labwork: UUID): Vector[Group] = {
    val people = sort(applicants)
    val grouped = strategy.group(people)
    val groups = alphabeticalOrdering(grouped.size) zip grouped

    groups.map(t => Group(t._1, labwork, t._2.toSet)).toVector
  }
}