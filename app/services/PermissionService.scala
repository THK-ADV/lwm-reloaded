package services

import models.PostgresPermission
import store.PermissionTable
import slick.driver.PostgresDriver.api._
import scala.concurrent.Future

trait PermissionService extends AbstractDao[PermissionTable, PostgresPermission, PostgresPermission] {
  override protected def tableQuery: TableQuery[PermissionTable] = TableQuery[PermissionTable]

  override protected def toAtomic(query: Query[PermissionTable, PostgresPermission, Seq]): Future[Seq[PostgresPermission]] = ???

  override protected def toUniqueEntity(query: Query[PermissionTable, PostgresPermission, Seq]): Future[Seq[PostgresPermission]] = db.run(query.result)
}

object PermissionService extends PermissionService
