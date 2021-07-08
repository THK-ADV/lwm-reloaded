package controllers

import akka.actor.ActorSystem
import controllers.helper.{
  AttributeFilter,
  ResultOps,
  SecureControllerContext,
  Secured
}
import dao.{AuthorityDao, UserDao}
import logger.AccessLoggingAction.log
import play.api.libs.json.Json
import play.api.mvc.{AbstractController, ControllerComponents}
import security.LWMRole.{Admin, God}
import security.SecurityActionChain
import service.UserSyncService
import service.actor.UserSyncActor

import java.util.UUID
import javax.inject.{Inject, Singleton}
import scala.concurrent.ExecutionContext

@Singleton
class UserSyncController @Inject() (
    cc: ControllerComponents,
    val authorityDao: AuthorityDao,
    val securedAction: SecurityActionChain,
    val syncService: UserSyncService,
    val userDao: UserDao,
    val system: ActorSystem,
    implicit val context: ExecutionContext
) extends AbstractController(cc)
    with Secured
    with SecureControllerContext
    with ResultOps
    with AttributeFilter {

  def sync(id: String) = contextFrom(Update) asyncAction log { r =>
    val userId = UUID.fromString(id)
    val preview = boolOf(r.queryString)("preview") getOrElse false
    val user = for {
      user <- userDao.getSingle(userId, atomic = false) if user.isDefined
      res <-
        if (preview) syncService.fetchUpdatedUser(user.get)
        else syncService.fetchAndUpdateUser(user.get)
    } yield res

    user.jsonResult { user =>
      Ok(
        Json.obj(
          "previous" -> user.previous,
          "updated" -> user.updated
        )
      )
    }
  }

  def syncAll = contextFrom(Update) asyncAction log { _ =>
    userDao
      .get(atomic = false)
      .map(xs => UserSyncActor.sync(system, syncService, xs))
      .jsonResult { _ =>
        Ok("sync started")
      }
  }

  override protected def contextFrom = {
    case Update => PartialSecureBlock(List(Admin))
    case _      => PartialSecureBlock(List(God))
  }

  override protected def restrictedContext(restrictionId: String) =
    forbiddenAction()
}
