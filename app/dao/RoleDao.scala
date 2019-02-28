package dao

import java.util.UUID

import database.helper.{EmployeeStatus, LdapUserStatus, LecturerStatus, StudentStatus}
import database.{RoleDb, RoleTable, TableFilter}
import javax.inject.Inject
import models.Role.{EmployeeRole, StudentRole}
import models._
import slick.dbio.Effect
import slick.jdbc.PostgresProfile
import slick.jdbc.PostgresProfile.api._

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

  def checkAuthority[R <: LWMRole](check: (Option[UUID], List[R]))(authorities: Seq[Authority]): Future[Boolean] = check match { // TODO test and refactor
    case (_, roles) if roles contains Role.God => Future.successful(false)
    case (optCourse, minRoles) =>
      def isAdmin(implicit roles: Seq[Role]) = roles
        .find(_.label == Role.Admin.label)
        .exists(admin => authorities.exists(_.role == admin.id))

      def hasPermission(implicit roles: Seq[Role]) = authorities
        .filter(_.course == optCourse)
        .flatMap(authority => roles.filter(_.id == authority.role))
        .exists(r => minRoles.exists(_.label == r.label))

      get().map { implicit roles =>
        isAdmin || hasPermission
      }
  }
}

final class RoleDaoImpl @Inject()(val db: PostgresProfile.backend.Database) extends RoleDao