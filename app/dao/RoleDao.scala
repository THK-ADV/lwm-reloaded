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

import scala.concurrent.{ExecutionContext, Future}

case class RoleLabelFilter(value: String) extends TableFilter[RoleTable] {
  override def predicate = _.label.toLowerCase === value.toLowerCase
}

trait RoleDao extends AbstractDao[RoleTable, RoleDb, Role] {

  override val tableQuery: TableQuery[RoleTable] = TableQuery[RoleTable]

  override protected def shouldUpdate(existing: RoleDb, toUpdate: RoleDb): Boolean = false

  override protected def existsQuery(entity: RoleDb): Query[RoleTable, RoleDb, Seq] = {
    filterBy(List(RoleLabelFilter(entity.label)))
  }

  override protected def toAtomic(query: Query[RoleTable, RoleDb, Seq]): Future[Seq[Role]] = toUniqueEntity(query)

  override protected def toUniqueEntity(query: Query[RoleTable, RoleDb, Seq]): Future[Seq[Role]] = {
    db.run(query.result.map(_.map(_.toUniqueEntity)))
  }

  def byUserStatusQuery(status: LdapUserStatus): DBIOAction[Option[RoleDb], NoStream, Effect.Read] = {
    byRoleLabelQuery(roleOf(status).label)
  }

  def byRoleLabelQuery(label: String): DBIOAction[Option[RoleDb], NoStream, Effect.Read] = {
    filterValidOnly(_.label === label).take(1).result.headOption
  }

  private def roleOf(status: LdapUserStatus): LWMRole = status match {
    case EmployeeStatus => EmployeeRole
    case LecturerStatus => EmployeeRole
    case StudentStatus => StudentRole
  }

  def isAuthorized(restricted: Option[UUID], required: List[LWMRole])(authorities: Seq[Authority]): Future[Boolean] = (restricted, required) match {
    case (_, roles) if roles contains Role.God => Future.successful(false)
    case (optCourse, minRoles) =>
      def isAdmin(implicit roles: Seq[Role]) = roles
        .find(_.label == Role.Admin.label)
        .exists(admin => authorities.exists(_.role == admin.id))

      def hasPermission(implicit roles: Seq[Role]) = authorities
        .filter(_.course == optCourse)
        .flatMap(authority => roles.filter(_.id == authority.role))
        .exists(r => minRoles.exists(_.label == r.label))

      get().map(implicit roles => isAdmin || hasPermission)
  }
}

final class RoleDaoImpl @Inject()(val db: PostgresProfile.backend.Database, val executionContext: ExecutionContext) extends RoleDao