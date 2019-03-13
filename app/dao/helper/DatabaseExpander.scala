package dao.helper

import models.UniqueDbEntity
import slick.dbio.Effect
import slick.dbio.Effect.Write
import slick.jdbc.PostgresProfile
import slick.jdbc.PostgresProfile.api._

trait DatabaseExpander[DbModel <: UniqueDbEntity] {
  def expandCreationOf[E <: Effect](entities: DbModel*): PostgresProfile.api.DBIOAction[Seq[DbModel], PostgresProfile.api.NoStream, Write with Any]

  def expandUpdateOf(entity: DbModel): DBIOAction[DbModel, NoStream, Write]

  def expandDeleteOf(entity: DbModel): DBIOAction[DbModel, NoStream, Write]
}
