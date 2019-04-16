package dao

import java.util.UUID

import database.helper.LdapUserStatus
import database.helper.LdapUserStatus._
import database.{RoleDb, RoleTable}
import javax.inject.Inject
import models.Role.{EmployeeRole, StudentRole}
import models._
import slick.dbio.Effect
import slick.jdbc.PostgresProfile.api._

import scala.concurrent.{ExecutionContext, Future}

trait RoleDao extends AbstractDao[RoleTable, RoleDb, Role] {

  import dao.helper.TableFilter.labelFilterEquals

  override val tableQuery: TableQuery[RoleTable] = TableQuery[RoleTable]

  override protected def shouldUpdate(existing: RoleDb, toUpdate: RoleDb): Boolean = false

  override protected def existsQuery(entity: RoleDb): Query[RoleTable, RoleDb, Seq] = {
    filterBy(List(labelFilterEquals(entity.label)))
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
    case (optCourse, requiredRoles) =>
      val userRoles = authorities.map(_.role)
      val restrictedUserRoles = authorities.filter(_.course == optCourse).map(_.role)
      val requiredRoleLabels = requiredRoles.map(_.label)

      val query = filterValidOnly { r =>
        def isAdmin: Rep[Boolean] = r.label === Role.Admin.label && r.id.inSet(userRoles)

        def hasPermission: Rep[Boolean] = r.id.inSet(restrictedUserRoles) && r.label.inSet(requiredRoleLabels)

        isAdmin || hasPermission
      }

      db.run(query.exists.result)
  }
}

final class RoleDaoImpl @Inject()(val db: Database, val executionContext: ExecutionContext) extends RoleDao