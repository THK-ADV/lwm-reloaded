package service.actor

import akka.actor.{Actor, ActorLogging, Props}
import javax.inject.{Inject, Singleton}
import service.SemesterService

import scala.concurrent.{ExecutionContextExecutor, Future}
import scala.util.{Failure, Success}

object SemesterCreationActor {

  def props(semesterService: SemesterService, year: Option[NaturalDescribableYear]) =
    Props(new SemesterCreationActor(semesterService, year))

  case object CreationRequestAsync

  case object CreationRequestSync

}

@Singleton
final class SemesterCreationActor @Inject()(
  private val semesterService: SemesterService,
  private val year: Option[NaturalDescribableYear]
) extends Actor with ActorLogging {

  import service.actor.SemesterCreationActor._

  private implicit val dispatcher: ExecutionContextExecutor = context.system.dispatcher

  override def receive = {
    case CreationRequestAsync =>
      log.info("beginning creation request")

      createSemester onComplete {
        case Success(result) =>
          log.info(s"result of creation request is $result")
        case Failure(throwable) =>
          log.error(s"failed creation request with throwable: ${throwable.getLocalizedMessage}")
      }

    case CreationRequestSync =>
      log.info("beginning creation request")

      val requester = sender()

      createSemester onComplete {
        case Success(result) => requester ! result
        case Failure(error) => requester ! error
      }
  }

  private def createSemester = year match {
    case Some(y) => semesterService.createSemester(y)
    case None => Future.failed(new Throwable("no year provided to create semester"))
  }
}
