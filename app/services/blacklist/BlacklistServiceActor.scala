package services.blacklist

import akka.actor.{Actor, ActorLogging, Props}
import dao.BlacklistDao
import services.NaturalDescribableYear

import scala.concurrent.ExecutionContextExecutor
import scala.util.{Failure, Success}

object BlacklistServiceActor {
  def props(blacklistService: BlacklistService, blacklistDao: BlacklistDao, year: NaturalDescribableYear) = Props(new BlacklistServiceActor(blacklistService, blacklistDao, year))

  case object BlacklistDownloadRequest
}

final class BlacklistServiceActor(private val blacklistService: BlacklistService, private val blacklistDao: BlacklistDao, private val year: NaturalDescribableYear) extends Actor with ActorLogging {

  import services.blacklist.BlacklistServiceActor._

  private implicit val dispatcher: ExecutionContextExecutor = context.system.dispatcher

  override def receive = {
    case BlacklistDownloadRequest =>
      log.info("beginning blacklist download request")

      blacklistService.fetchLegalHolidays(year.number).flatMap(blacklistDao.createManyPartial) onComplete {
        case Success(result) =>
          log.info(s"result of blacklist download request is $result")
        case Failure(error) =>
          log.error(s"failed blacklist download request with exception: ${error.getLocalizedMessage}")
      }
  }
}
