package controllers.core

import java.util.UUID

import controllers.helper.{AttributeFilter, RawJsonParser}
import database.UniqueTable
import models.{UniqueDbEntity, UniqueEntity}
import play.api.mvc.{AnyContent, Request}
import slick.jdbc.PostgresProfile.api.Table

import scala.concurrent.Future

trait JsonBodyParser[Protocol, T <: Table[DbModel] with UniqueTable, DbModel <: UniqueDbEntity, LwmModel <: UniqueEntity]
  extends RawJsonParser {
  self: Core[Protocol, T, DbModel, LwmModel]
    with AttributeFilter
    with JsonReads[Protocol] =>

  protected def toDbModel(protocol: Protocol, existingId: Option[UUID]): DbModel

  protected def parsed(id: Option[UUID], action: DbModel => Future[DbModel])(implicit request: Request[AnyContent]): Future[LwmModel] = {
    for {
      protocol <- Future.fromTry(parseJson(request)(reads))
      dbModel = toDbModel(protocol, id)
      result <- action(dbModel)
      atomic = isAtomic(default = false)
      lwmModel <- if (atomic)
        abstractDao.getSingle(result.id, atomic)
      else
        Future.successful(Some(toLwmModel(result)))
      if lwmModel.isDefined
    } yield lwmModel.get
  }
}
