package services

import java.util.UUID

import models.UniqueEntity
import slick.driver.PostgresDriver.api._
import store.{PostgresDatabase, UniqueTable}

trait AbstractDao[T <: Table[E] with UniqueTable, E <: UniqueEntity] extends PostgresDatabase {

  protected def tableQuery: TableQuery[T]

  /*def create[T <: Table[E]](entity: E)(implicit tableQuery: TableQuery[T]) = (tableQuery returning tableQuery) += entity */

  def createMany(entities: Set[E]) = db.run((tableQuery returning tableQuery) ++= entities)

  def getAll = db.run(tableQuery.result)

  def filter(predicate: T => Rep[Boolean]) = db.run(tableQuery.filter(predicate).result)

  def get(id: UUID) = db.run(tableQuery.filter(_.id === id).result)

  /*def delete[T <: Table[E] with BasicTemplate](id: UUID)(implicit tableQuery: TableQuery[T]) = tableQuery.filter(_.id === id).delete

  def deleteMany[T <: Table[E] with BasicTemplate](ids: List[UUID])(implicit tableQuery: TableQuery[T]) = tableQuery.filter(_.id.inSet(ids)).delete

  def update[T <: Table[E] with BasicTemplate](entity: E)(implicit tableQuery: TableQuery[T]) = tableQuery.filter(_.id === entity.id).update(entity)*/

  def dropAndCreateSchema = db.run(DBIO.seq(drop, create).transactionally)

  def createSchema = db.run(create)

  def dropSchema = db.run(drop)

  private def create = tableQuery.schema.create

  private def drop = tableQuery.schema.drop
}
