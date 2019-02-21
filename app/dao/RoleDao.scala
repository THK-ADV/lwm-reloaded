package dao

import database.helper.{EmployeeStatus, LdapUserStatus, LecturerStatus, StudentStatus}
import javax.inject.Inject
import models._
import slick.dbio.Effect
import slick.jdbc.PostgresProfile
import slick.jdbc.PostgresProfile.api._
import database.{RoleDb, RoleTable, TableFilter}
import models.Role.{EmployeeRole, StudentRole}

import scala.concurrent.Future

case class RoleLabelFilter(value: String) extends TableFilter[RoleTable] {
  override def predicate = _.label.toLowerCase === value.toLowerCase
}

trait RoleDao extends AbstractDao[RoleTable, RoleDb, Role] {

  import scala.concurrent.ExecutionContext.Implicits.global

  override val tableQuery: TableQuery[RoleTable] = TableQuery[RoleTable]

  override protected def shouldUpdate(existing: RoleDb, toUpdate: RoleDb): Boolean = false

  override protected def existsQuery(entity: RoleDb): Query[RoleTable, RoleDb, Seq] = {
    filterBy(List(RoleLabelFilter(entity.label)))
  }

  override protected def toAtomic(query: Query[RoleTable, RoleDb, Seq]): Future[Seq[Role]] = toUniqueEntity(query)

  override protected def toUniqueEntity(query: Query[RoleTable, RoleDb, Seq]): Future[Seq[Role]] = {
    db.run(query.result.map(_.map(_.toUniqueEntity)))
  }

  def byUserStatusQuery(status: LdapUserStatus): DBIOAction[Option[RoleDb], NoStream, Effect.Read] = { // TODO test
    byRoleLabelQuery(roleOf(status).label)
  }

  def byRoleLabelQuery(label: String): DBIOAction[Option[RoleDb], NoStream, Effect.Read] = {
    tableQuery.filter(_.label === label).result.headOption
  }

  private def roleOf(status: LdapUserStatus): LWMRole = status match {
    case EmployeeStatus => EmployeeRole
    case LecturerStatus => EmployeeRole
    case StudentStatus => StudentRole
  }
}

final class RoleDaoImpl @Inject()(val db: PostgresProfile.backend.Database) extends RoleDao