package services

import models._
import org.joda.time.DateTime
import store.{PostgresDatabase, RolePermissionTable, RoleTable}
import slick.driver.PostgresDriver.api._

import scala.concurrent.Future

trait RoleService2 extends AbstractDao[RoleTable, RoleDb, Role] { self: PostgresDatabase =>
  import scala.concurrent.ExecutionContext.Implicits.global

  protected def rolePermissionService: RolePermissionService

  override val tableQuery: TableQuery[RoleTable] = TableQuery[RoleTable]

  override protected def setInvalidated(entity: RoleDb): RoleDb = {
    RoleDb(entity.label, entity.permissions, Some(DateTime.now), entity.id)
  }

  override protected def toAtomic(query: Query[RoleTable, RoleDb, Seq]): Future[Seq[Role]] = joinPermissions(query) {
    case (role, rolePerms) =>
      val permissions = rolePerms.map {
        case (_, perm) => PostgresPermission(perm.value, perm.description, perm.id)
      }

      PostgresRoleAtom(role.label, permissions.toSet, role.id)
  }

  override protected def toUniqueEntity(query: Query[RoleTable, RoleDb, Seq]): Future[Seq[Role]] = joinPermissions(query) {
    case (role, rolePerms) => PostgresRole(role.label, rolePerms.map(_._2.id).toSet, role.id)
  }

  private def joinPermissions(query: Query[RoleTable, RoleDb, Seq])(buildRole: (RoleDb, Seq[(RoleDb, PermissionDb)]) => Role): Future[Seq[Role]] = {
    val rolesWithPermissions = for {
      q <- query
      p <- q.permissions
    } yield (q, p)

    db.run(rolesWithPermissions.result.map(_.groupBy(_._1).map {
      case (role, rolePerms) => buildRole(role, rolePerms)
    }.toSeq))
  }

  def byUserStatus(status: String): Future[Option[RoleDb]] = {
    db.run(tableQuery.filter(_.isLabel(Roles.fromUserStatus(status))).result.headOption)
  }

  def createManyWithPermissions(roles: List[RoleDb]): Future[Map[Option[PostgresRole], Seq[RolePermission]]] = {
    for {
      rs <- createMany(roles)
      rolePermissions = roles.flatMap(r => r.permissions.map(p => RolePermission(r.id, p)))
      rps <- rolePermissionService.createMany(rolePermissions)
    } yield rps.groupBy(_.role).map {
      case ((r, rp)) => (rs.find(_.id == r).map(r => PostgresRole(r.label, r.permissions, r.id)), rp)
    }
  }
}

trait RolePermissionService extends AbstractDao[RolePermissionTable, RolePermission, RolePermission] { self: PostgresDatabase =>
  override val tableQuery: TableQuery[RolePermissionTable] = TableQuery[RolePermissionTable]

  override protected def setInvalidated(entity: RolePermission): RolePermission = {
    RolePermission(entity.role, entity.permission, Some(DateTime.now), entity.id)
  }

  override protected def toAtomic(query: Query[RolePermissionTable, RolePermission, Seq]): Future[Seq[RolePermission]] = ???

  override protected def toUniqueEntity(query: Query[RolePermissionTable, RolePermission, Seq]): Future[Seq[RolePermission]] = ???
}

object RoleService2 extends RoleService2 with PostgresDatabase {
  override protected def rolePermissionService: RolePermissionService = RolePermissionService
}
object RolePermissionService extends RolePermissionService with PostgresDatabase