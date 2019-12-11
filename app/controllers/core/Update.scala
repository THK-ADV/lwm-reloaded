package controllers.core

import controllers.helper.{ResultOps, SecureControllerContext}
import database.UniqueTable
import models.{UniqueDbEntity, UniqueEntity}
import play.api.mvc.{Action, AnyContent}
import slick.jdbc.PostgresProfile.api.Table

trait Update[Protocol, T <: Table[DbModel] with UniqueTable, DbModel <: UniqueDbEntity, LwmModel <: UniqueEntity] {
  self: Core[Protocol, T, DbModel, LwmModel]
    with SecureControllerContext
    with ResultOps
    with JsonWrites[LwmModel]
    with JsonBodyParser[Protocol, T, DbModel, LwmModel] =>

  import DBFilterOps._

  def update(id: String, secureContext: SecureContext = contextFrom(Update)): Action[AnyContent] = secureContext asyncAction { implicit request =>
    id.uuidF
      .flatMap(uuid => parsed(Some(uuid), abstractDao.update))
      .jsonResult
  }
}
