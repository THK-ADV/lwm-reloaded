package dao.helper

import java.util.UUID

import dao.AbstractDao
import dao.helper.TableFilter.labworkFilter
import database.{LabworkIdTable, UniqueTable}
import models.UniqueDbEntity
import slick.jdbc.PostgresProfile.api._

import scala.concurrent.Future

trait CrossInvalidated[T <: Table[DbModel] with UniqueTable with LabworkIdTable, DbModel <: UniqueDbEntity] {
  self: AbstractDao[T, DbModel, _] =>

  def invalidateByLabwork(labwork: UUID): Future[List[DbModel]] = {
    val action = for {
      q <- filterValidOnly(labworkFilter(labwork)).result
      d <- invalidateManyQuery(q.map(_.id).toList)
    } yield d

    db.run(action)
  }
}
