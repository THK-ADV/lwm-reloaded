package controllers.core

import java.util.UUID

import controllers.helper.{ResultOps, SecureControllerContext}
import database.UniqueTable
import models.{UniqueDbEntity, UniqueEntity}
import slick.jdbc.PostgresProfile.api.Table

import scala.concurrent.Future

trait Delete[Protocol, T <: Table[DbModel] with UniqueTable, DbModel <: UniqueDbEntity, LwmModel <: UniqueEntity] {
  self: Core[Protocol, T, DbModel, LwmModel]
    with SecureControllerContext
    with ResultOps
    with JsonWrites[LwmModel] =>

  import DBFilterOps._

  def invalidate(id: String, secureContext: SecureContext = contextFrom(Delete)) = secureContext asyncAction { _ =>
    id.uuidF.flatMap(invalidate0).jsonResult
  }

  protected def invalidate0(uuid: UUID): Future[LwmModel] = { // TODO why does this function exists?
    abstractDao.invalidate(uuid).map(toLwmModel)
  }
}
