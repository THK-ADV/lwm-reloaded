package controllers.core

import dao.AbstractDao
import database.UniqueTable
import models.{UniqueDbEntity, UniqueEntity}
import slick.jdbc.PostgresProfile.api.Table

import scala.concurrent.ExecutionContext

trait Core[Protocol, T <: Table[DbModel] with UniqueTable, DbModel <: UniqueDbEntity, LwmModel <: UniqueEntity] {
  protected def abstractDao: AbstractDao[T, DbModel, LwmModel]
  protected implicit def ctx: ExecutionContext

  protected def toLwmModel(dbModel: DbModel): LwmModel = dbModel.toUniqueEntity.asInstanceOf[LwmModel]
}
