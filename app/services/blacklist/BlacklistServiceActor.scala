package services.blacklist

import akka.actor.{Actor, ActorLogging, Props}
import dao.BlacklistDao

import scala.concurrent.ExecutionContextExecutor
import scala.util.{Failure, Success}

object BlacklistServiceActor {
  def props(blacklistService: BlacklistService, blacklistDao: BlacklistDao, blacklistYear: BlacklistYear) = Props(new BlacklistServiceActor(blacklistService, blacklistDao, blacklistYear))

  case object BlacklistDownloadRequest

}

final class BlacklistServiceActor(private val blacklistService: BlacklistService, private val blacklistDao: BlacklistDao, private val blacklistYear: BlacklistYear) extends Actor with ActorLogging {

  import services.blacklist.BlacklistServiceActor._
  import utils.Ops.unwrapTrys

  private implicit val dispatcher: ExecutionContextExecutor = context.system.dispatcher

  override def receive = {
    case BlacklistDownloadRequest =>
      log.info("beginning blacklist download request")

      (for {
        blacklists <- blacklistService.fetchLegalHolidays(blacklistYear.year)
        partialCreated <- blacklistDao.createManyPartial(blacklists)
        (succeeded, failed) = unwrapTrys(partialCreated)
      } yield (blacklists.map(_.toLwmModel), succeeded.map(_.toLwmModel), failed)) onComplete {
        case Success((attempted, created, throwable)) =>
          log.info(
            s"""
               | attempted to to download ${attempted.size} blacklists ($attempted)
               | successfully created ${created.size} of those ($created)
               | failed to create ${throwable.size} of those, because $throwable
            """.stripMargin)
        case Failure(error) =>
          log.error(s"failed blacklist download request with exception: ${error.getLocalizedMessage}")
      }
  }
}
