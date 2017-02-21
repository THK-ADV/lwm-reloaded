package services

import java.util.UUID

import models.UniqueEntity
import slick.driver.PostgresDriver.api._
import store.{PostgresDatabase, UniqueTable}

import scala.concurrent.Future

trait AbstractDao[T <: Table[E] with UniqueTable, E <: UniqueEntity] extends PostgresDatabase {

  import scala.concurrent.ExecutionContext.Implicits.global

  protected def tableQuery: TableQuery[T]

  def create(entity: E): Future[E] = db.run((tableQuery returning tableQuery) += entity)

  def createMany(entities: Set[E]): Future[Seq[E]] = db.run((tableQuery returning tableQuery) ++= entities)

  def getAll: Future[Seq[E]] = db.run(tableQuery.result)

  def filter(predicate: T => Rep[Boolean]): Future[Seq[E]] = db.run(tableQuery.filter(predicate).result)

  def get(id: UUID): Future[Option[E]] = db.run(tableQuery.filter(_.id === id).result.map(_.headOption))

  def delete(id: UUID): Future[Int] = db.run(tableQuery.filter(_.id === id).delete)

  def deleteMany(ids: Set[UUID]): Future[Int] = db.run(tableQuery.filter(_.id.inSet(ids)).delete)

  def update(entity: E): Future[Int] = db.run(tableQuery.filter(_.id === entity.id).update(entity))

  def dropAndCreateSchema = db.run(DBIO.seq(drop, create).transactionally)

  def createSchema = db.run(create)

  def dropSchema = db.run(drop)

  private def create = tableQuery.schema.create

  private def drop = tableQuery.schema.drop
}
