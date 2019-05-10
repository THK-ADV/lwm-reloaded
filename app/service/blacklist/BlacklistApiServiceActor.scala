package service.blacklist

import akka.actor.{Actor, ActorLogging, Props}
import dao.BlacklistDao
import javax.inject.Inject
import service.actor.NaturalDescribableYear

import scala.concurrent.{ExecutionContextExecutor, Future}
import scala.util.{Failure, Success}

object BlacklistApiServiceActor {
  def props(blacklistApiService: BlacklistApiService, blacklistDao: BlacklistDao, year: Option[NaturalDescribableYear]) =
    Props(new BlacklistApiServiceActor(blacklistApiService, blacklistDao, year))

  case object BlacklistDownloadRequestAsync

  case object BlacklistDownloadRequestSync

}

final class BlacklistApiServiceActor @Inject()(
  private val blacklistApiService: BlacklistApiService,
  private val blacklistDao: BlacklistDao,
  private val year: Option[NaturalDescribableYear]
) extends Actor with ActorLogging {

  import BlacklistApiServiceActor._

  private implicit val dispatcher: ExecutionContextExecutor = context.system.dispatcher

  override def receive = {
    case BlacklistDownloadRequestAsync =>
      log.info("beginning blacklist download request")

      download onComplete {
        case Success(blacklists) =>
          log.info(s"result of blacklist download request is $blacklists")
        case Failure(throwable) =>
          log.error(s"failed blacklist download request with throwable: ${throwable.getLocalizedMessage}")
      }
    case BlacklistDownloadRequestSync =>
      log.info("beginning blacklist download request")

      val requester = sender()

      download onComplete {
        case Success(blacklists) => requester ! blacklists
        case Failure(throwable) => requester ! throwable
      }
  }

  private def download = year match {
    case Some(y) => blacklistApiService.fetchLegalHolidays(y.year).flatMap(blacklistDao.createManyPartial)
    case None => Future.failed(new Throwable("no year provided to download blacklists"))
  }
}
