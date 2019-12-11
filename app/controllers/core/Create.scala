package controllers.core

import controllers.helper.{AttributeFilter, ResultOps, SecureControllerContext}
import database.UniqueTable
import models.{UniqueDbEntity, UniqueEntity}
import play.api.mvc.{Action, AnyContent}
import slick.jdbc.PostgresProfile.api.Table

trait Create[Protocol, T <: Table[DbModel] with UniqueTable, DbModel <: UniqueDbEntity, LwmModel <: UniqueEntity] {
  self: Core[Protocol, T, DbModel, LwmModel]
    with SecureControllerContext
    with ResultOps
    with JsonBodyParser[Protocol, T, DbModel, LwmModel]
    with JsonWrites[LwmModel]
    with AttributeFilter =>

  def create(secureContext: SecureContext = contextFrom(Create)): Action[AnyContent] = secureContext asyncAction { implicit request =>
    parsed(None, abstractDao.create).created
  }
}
