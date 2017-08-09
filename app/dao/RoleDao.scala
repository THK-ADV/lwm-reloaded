package dao

import models._
import slick.dbio.Effect
import slick.driver.PostgresDriver
import slick.driver.PostgresDriver.api._
import store.{RoleTable, TableFilter}

import scala.concurrent.Future

case class RoleLabelFilter(value: String) extends TableFilter[RoleTable] {
  override def predicate = _.label.toLowerCase === value.toLowerCase
}

trait RoleDao extends AbstractDao[RoleTable, RoleDb, PostgresRole] {

  import scala.concurrent.ExecutionContext.Implicits.global

  override val tableQuery: TableQuery[RoleTable] = TableQuery[RoleTable]

  override protected def shouldUpdate(existing: RoleDb, toUpdate: RoleDb): Boolean = false

  override protected def existsQuery(entity: RoleDb): Query[RoleTable, RoleDb, Seq] = {
    filterBy(List(RoleLabelFilter(entity.label)))
  }

  override protected def toAtomic(query: Query[RoleTable, RoleDb, Seq]): Future[Seq[PostgresRole]] = toUniqueEntity(query)

  override protected def toUniqueEntity(query: Query[RoleTable, RoleDb, Seq]): Future[Seq[PostgresRole]] = {
    db.run(query.result.map(_.map(_.toLwmModel)))
  }

  def byUserStatus(status: String): Future[Option[RoleDb]] = { // TODO get rid of db.run calls. return queries instead
    db.run(byUserStatusQuery(status))
  }

  def byUserStatusQuery(status: String): DBIOAction[Option[RoleDb], NoStream, Effect.Read] = {
    tableQuery.filter(_.label === Roles.fromUserStatus(status)).result.headOption
  }

  def byRoleLabelQuery(label: String): DBIOAction[Option[RoleDb], NoStream, Effect.Read] = {
    tableQuery.filter(_.label === label).result.headOption
  }
}

final class RoleDaoImpl(val db: PostgresDriver.backend.Database) extends RoleDao