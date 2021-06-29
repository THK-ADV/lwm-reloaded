package controllers

import controllers.helper.{AttributeFilter, ResultOps, SecureControllerContext, Secured}
import dao.AuthorityDao
import logger.AccessLoggingAction.log
import play.api.libs.json.Json
import play.api.mvc.{AbstractController, ControllerComponents}
import security.LWMRole.{Admin, God}
import security.SecurityActionChain
import service.UserSyncService

import java.util.UUID
import javax.inject.{Inject, Singleton}
import scala.concurrent.ExecutionContext

@Singleton
class UserSyncController @Inject() (
    cc: ControllerComponents,
    val authorityDao: AuthorityDao,
    val securedAction: SecurityActionChain,
    val syncService: UserSyncService,
    implicit val context: ExecutionContext
) extends AbstractController(cc)
    with Secured
    with SecureControllerContext
    with ResultOps
    with AttributeFilter {

  def sync(id: String) = contextFrom(Update) asyncAction log { r =>
    val userId = UUID.fromString(id)
    val preview = boolOf(r.queryString)("preview") getOrElse false
    val res =
      if (preview) syncService.fetchUpdatedUser(userId)
      else syncService.fetchAndUpdateUser(userId)
    res.jsonResult { user =>
      Ok(
        Json.obj(
          "previous" -> user.previous,
          "updated" -> user.updated
        )
      )
    }
  }

  override protected def contextFrom = {
    case Update => PartialSecureBlock(List(Admin))
    case _      => PartialSecureBlock(List(God))
  }

  override protected def restrictedContext(restrictionId: String) =
    forbiddenAction()
}
