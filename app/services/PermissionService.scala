package services

import models.{PermissionDb, PostgresPermission}
import slick.driver.PostgresDriver
import slick.driver.PostgresDriver.api._
import store.{PermissionTable, TableFilter}

import scala.concurrent.Future

case class PermissionValueFilter(value: String) extends TableFilter[PermissionTable] {
  override def predicate = _.value.toLowerCase === value.toLowerCase
}

trait PermissionService extends AbstractDao[PermissionTable, PermissionDb, PostgresPermission] {
  import scala.concurrent.ExecutionContext.Implicits.global

  override val tableQuery: TableQuery[PermissionTable] = TableQuery[PermissionTable]

  override protected def shouldUpdate(existing: PermissionDb, toUpdate: PermissionDb): Boolean = {
    existing.description != toUpdate.description && existing.value == toUpdate.value
  }

  override protected def existsQuery(entity: PermissionDb): Query[PermissionTable, PermissionDb, Seq] = {
    filterBy(List(PermissionValueFilter(entity.value)))
  }

  override protected def toAtomic(query: Query[PermissionTable, PermissionDb, Seq]): Future[Seq[PostgresPermission]] = toUniqueEntity(query)

  override protected def toUniqueEntity(query: Query[PermissionTable, PermissionDb, Seq]): Future[Seq[PostgresPermission]] = db.run(query.result.map(_.map(_.toLwmModel)))
}

final class PermissionServiceImpl(val db: PostgresDriver.backend.Database) extends PermissionService
