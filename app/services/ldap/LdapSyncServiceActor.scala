package services.ldap

import akka.actor.{Actor, ActorLogging, Props}
import dao.UserDao
import services.ldap.LdapSyncServiceActor.SyncRequest

import scala.concurrent.ExecutionContextExecutor
import scala.util.{Failure, Success}

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
        ldapUsers <- ldapService.users(lwmUsers.map(_.systemId.toLowerCase).toSet)
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