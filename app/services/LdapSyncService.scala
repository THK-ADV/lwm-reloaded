package services

import akka.actor.{Actor, ActorLogging, ActorSystem, Props}
import dao.UserDao
import services.LdapSyncServiceActor.SyncRequest
import us.theatr.akka.quartz.{AddCronSchedule, QuartzActor}

import scala.concurrent.ExecutionContextExecutor
import scala.util.{Failure, Success}

trait LdapSyncService {

  def cronExpression: String

  def ldapService: LdapService

  def userDao: UserDao
}

final class ActorBasedLdapSyncService(val system: ActorSystem, val cronExpression: String, val ldapService: LdapService, val userDao: UserDao) extends LdapSyncService {

  val quartzActor = system.actorOf(Props[QuartzActor])
  val destinationActorRef = system.actorOf(LdapSyncServiceActor.props(ldapService, userDao))

  quartzActor ! AddCronSchedule(destinationActorRef, cronExpression, SyncRequest)
}

object LdapSyncServiceActor {

  def props(ldapService: LdapService, userDao: UserDao) = Props(new LdapSyncServiceActor(ldapService, userDao))

  case object SyncRequest

}

final class LdapSyncServiceActor(val ldapService: LdapService, val userDao: UserDao) extends Actor with ActorLogging {

  implicit val dispatcher: ExecutionContextExecutor = context.system.dispatcher

  import utils.Ops.{FutureOps, unwrapTrys}

  override def receive: Receive = {
    case SyncRequest =>
      log.info("log.info starting sync request")

      val result = for {
        lwmUsers <- userDao.get(atomic = false)
        ldapUsers <- ldapService.users2(lwmUsers.map(_.systemId.toLowerCase).toSet)
        partialUpdated <- ldapUsers.map(userDao.createOrUpdate).toList.asTrys // TODO createOrUpdate could be optimized for syncing purpose since delta updates are not considered, authorities can't be created, users are already in scope, etc...
        (succeeded, failed) = unwrapTrys(partialUpdated)
      } yield (succeeded, failed)

      result onComplete {
        case Success((succeeded, failed)) =>
          log.info(s"successfully created or updated ${succeeded.size} users")

          if (failed.nonEmpty)
            log.info(s"failed to create or update some users, see: ${failed.map(_.getLocalizedMessage)} ")
        case Failure(e) =>
          log.error(s"ldap sync request failed with exception: ${e.getLocalizedMessage}")
      }
  }
}