package services

import models.{PermissionDb, PostgresPermission}
import store.{PermissionTable, PostgresDatabase}
import slick.driver.PostgresDriver.api._

import scala.concurrent.Future

trait PermissionService extends AbstractDao[PermissionTable, PermissionDb, PostgresPermission] { self: PostgresDatabase =>
  import scala.concurrent.ExecutionContext.Implicits.global

  override protected def tableQuery: TableQuery[PermissionTable] = TableQuery[PermissionTable]

  override protected def toAtomic(query: Query[PermissionTable, PermissionDb, Seq]): Future[Seq[PostgresPermission]] = toUniqueEntity(query)

  override protected def toUniqueEntity(query: Query[PermissionTable, PermissionDb, Seq]): Future[Seq[PostgresPermission]] = db.run(query.result.map(_.map(_.toPermission)))
}

object PermissionService extends PermissionService with PostgresDatabase
