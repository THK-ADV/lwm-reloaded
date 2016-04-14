package services

import java.util.UUID
import java.util.concurrent.Executors
import utils.PreferenceSort._

import scala.concurrent.{ExecutionContext, Future}
import scala.util.Try
import scalaz.StreamT._

trait GroupServiceLike {

  def alphabeticalOrdering(amount: Int): List[String] = orderingWith('A')(char => Some((char.toString, (char + 1).toChar)))(amount % 27)

  def orderingWith[A, B](a: A)(v: A => Option[(B, A)]): Int => List[B] = amount => unfold(a)(v).take(amount).toStream.toList

  //THIS RESULT FROM THIS SHOULD `NEVER` BE TRANSFORMED INTO A SET. ORDERING IS CRUCIAL!
  def sortApplicantsFor(labwork: UUID): Try[Vector[UUID]]
}

class GroupService(private val applicationService: LabworkApplicationServiceLike) extends GroupServiceLike {

  override def sortApplicantsFor(labwork: UUID): Try[Vector[UUID]] = {
    applicationService.applicationsFor(labwork) map { v =>
      val nodes = v map (app => (app.applicant, app.friends))
      sort(nodes)
    }
  }
}