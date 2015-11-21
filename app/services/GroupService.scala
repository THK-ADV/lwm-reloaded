package services

import java.util.UUID
import java.util.concurrent.Executors

import org.w3.banana.RDFPrefix
import store.Prefixes.LWMPrefix
import store.SesameRepository
import utils.PTree._

import scala.concurrent.{ExecutionContext, Future}
import scalaz.StreamT._

trait GroupServiceLike {

  def alphabeticalOrdering(amount: Int): List[String] = orderingWith('A')(char => Some((char.toString, (char + 1).toChar)))(amount % 27)

  def orderingWith[A, B](a: A)(v: A => Option[(B, A)]): Int => List[B] = amount => unfold(a)(v).take(amount).toStream.toList

  def sortApplicantsFor(labwork: UUID): Option[Vector[UUID]]

  def sortApplicantsForMany(labworks: TraversableOnce[UUID]): Future[Option[Map[UUID, Vector[UUID]]]]

}

class GroupService(private val repository: SesameRepository, private val applicationService: LabworkApplicationServiceLike) extends GroupServiceLike {

  import repository._

  private val lwm = LWMPrefix[Rdf]
  private val rdf = RDFPrefix[Rdf]
  private implicit val exec = ExecutionContext.fromExecutor(Executors.newCachedThreadPool())


  override def sortApplicantsFor(labwork: UUID): Option[Vector[UUID]] = {
    applicationService.applicationsFor(labwork) map { v =>
      val nodes = v map (app => (app.applicant, app.friends.toList))
      sortWithPairs(nodes)
    }
  }

  override def sortApplicantsForMany(labworks: TraversableOnce[UUID]): Future[Option[Map[UUID, Vector[UUID]]]] = {
    import utils.Ops._
    val concurrent = Future.traverse(labworks) { labwork =>
      Future {
        for {
          application <- applicationService.applicationsFor(labwork)
          paired = application map (app => (app.applicant, app.friends.toList))
        } yield (labwork, sortWithPairs(paired))
      }
    }

    concurrent.map(_.sequence.map(_.toMap))
  }
}
