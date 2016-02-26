package services

import java.util.UUID
import java.util.concurrent.Executors
import utils.Ops.MonadInstances._
import models.Group
import org.w3.banana.RDFPrefix
import store.Prefixes.LWMPrefix
import store.SesameRepository
import store.bind.Bindings
import utils.PTree._

import scala.concurrent.{ExecutionContext, Future}
import scala.util.Try
import scalaz.StreamT._

trait GroupServiceLike {

  def alphabeticalOrdering(amount: Int): List[String] = orderingWith('A')(char => Some((char.toString, (char + 1).toChar)))(amount % 27)

  def orderingWith[A, B](a: A)(v: A => Option[(B, A)]): Int => List[B] = amount => unfold(a)(v).take(amount).toStream.toList

  //THIS MUST NEVER BE A SET. ORDERING IS CRUCIAL!
  def sortApplicantsFor(labwork: UUID): Option[Vector[UUID]]
}

class GroupService(private val applicationService: LabworkApplicationServiceLike) extends GroupServiceLike {

  private implicit val exec = ExecutionContext.fromExecutor(Executors.newCachedThreadPool())

  override def sortApplicantsFor(labwork: UUID): Option[Vector[UUID]] = {
    applicationService.applicationsFor(labwork) map { v =>
      val nodes = v map (app => (app.applicant, app.friends.toList))
      sortWithPairs(nodes)
    }
  }
}