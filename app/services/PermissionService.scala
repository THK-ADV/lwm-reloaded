package services

import models.{PermissionDb, PostgresPermission}
import org.joda.time.DateTime
import store.{PermissionTable, TableFilter}
import slick.driver.PostgresDriver.api._

import scala.concurrent.Future
import models.LwmDateTime.DateTimeConverter
import slick.driver.PostgresDriver

case class PermissionValueFilter(value: String) extends TableFilter[PermissionTable] {
  override def predicate = _.value.toLowerCase like s"%${value.toLowerCase}%"
}

case class PermissionPrefixFilter(value: String) extends TableFilter[PermissionTable] {
  override def predicate = _.value.toLowerCase like s"%${value.toLowerCase}%:%"
}

case class PermissionSuffixFilter(value: String) extends TableFilter[PermissionTable] {
  override def predicate = _.value.toLowerCase like s"%:%${value.toLowerCase}%"
}

case class PermissionDescriptionFilter(value: String) extends TableFilter[PermissionTable] {
  override def predicate = _.description.toLowerCase like s"%${value.toLowerCase}%"
}


trait PermissionService extends AbstractDao[PermissionTable, PermissionDb, PostgresPermission] {
  import scala.concurrent.ExecutionContext.Implicits.global

  override val tableQuery: TableQuery[PermissionTable] = TableQuery[PermissionTable]

  override protected def setInvalidated(entity: PermissionDb): PermissionDb = {
    val now = DateTime.now.timestamp

    PermissionDb(
      entity.value,
      entity.description,
      now,
      Some(now),
      entity.id
    )
  }

  override protected def shouldUpdate(existing: PermissionDb, toUpdate: PermissionDb): Boolean = {
    existing.description != toUpdate.description && existing.value == toUpdate.value
  }

  override protected def existsQuery(entity: PermissionDb): Query[PermissionTable, PermissionDb, Seq] = {
    filterBy(List(PermissionValueFilter(entity.value)))
  }

  override protected def toAtomic(query: Query[PermissionTable, PermissionDb, Seq]): Future[Seq[PostgresPermission]] = toUniqueEntity(query)

  override protected def toUniqueEntity(query: Query[PermissionTable, PermissionDb, Seq]): Future[Seq[PostgresPermission]] = db.run(query.result.map(_.map(_.toPermission)))
}

final class PermissionServiceImpl(val db: PostgresDriver.backend.Database) extends PermissionService
