package dao.helper

import models.UniqueDbEntity
import slick.dbio.Effect
import slick.dbio.Effect.Write
import slick.jdbc.PostgresProfile.api._

trait DatabaseExpander[DbModel <: UniqueDbEntity] {
  def expandCreationOf[E <: Effect](entities: Seq[DbModel]): DBIOAction[Seq[DbModel], NoStream, Write with E]

  def expandUpdateOf(entity: DbModel): DBIOAction[DbModel, NoStream, Write]

  def expandDeleteOf(entity: DbModel): DBIOAction[DbModel, NoStream, Write]
}
