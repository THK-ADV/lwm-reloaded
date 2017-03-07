package services

import models._
import store.{RolePermissionTable, RoleTable}
import slick.driver.PostgresDriver.api._

import scala.concurrent.Future

trait RoleService2 extends AbstractDao[RoleTable, PostgresRole, Role] {
  import scala.concurrent.ExecutionContext.Implicits.global

  override protected def tableQuery: TableQuery[RoleTable] = TableQuery[RoleTable]

  override protected def toAtomic(query: Query[RoleTable, PostgresRole, Seq]): Future[Seq[Role]] = {
    val a = db.run {
      val z = (for {
        q <- query
        p <- q.permissions
      } yield (q, p)).result
      z.statements.foreach(println)
      z
    }

    a.map(_.groupBy(_._1).map {
      case (a, b) => PostgresRoleAtom(a.label, b.map(_._2).toSet, a.id)
    }.toSeq)
  }

  override protected def toUniqueEntity(query: Query[RoleTable, PostgresRole, Seq]): Future[Seq[Role]] = {
    val a = db.run {
      val z = (for {
        q <- query
        p <- q.permissions
      } yield (q, p)).result
      z.statements.foreach(println)
      z
    }

    a.map(_.groupBy(_._1).map {
      case (a, b) => PostgresRole(a.label, b.map(_._2.id).toSet, a.id)
    }.toSeq)
  }

  def byUserStatus(status: String): Future[Option[PostgresRole]] = {
    def roleLabel = status match {
      case User.EmployeeType => Roles.EmployeeLabel
      case User.LecturerType => Roles.EmployeeLabel
      case User.StudentType => Roles.StudentLabel
    }

    db.run(tableQuery.filter(_.isLabel(roleLabel)).result).map(_.headOption)
  }

  def createWithPermissions(role: PostgresRole): Future[(PostgresRole, Seq[RolePermission])] = {
    for {
      r <- create(role)
      rolePermissions = role.permissions.map(p => RolePermission(role.id, p))
      rps <- RolePermissionService.createMany(rolePermissions)
    } yield (r, rps)
  }

  def createManyWithPermissions(roles: Set[PostgresRole]): Future[Map[Option[PostgresRole], Seq[RolePermission]]] = {
    for {
      rs <- createMany(roles)
      rolePermissions = roles.flatMap(r => r.permissions.map(p => RolePermission(r.id, p)))
      rps <- RolePermissionService.createMany(rolePermissions)
    } yield rps.groupBy(_.role).map {
      case ((r, rp)) => (rs.find(_.id == r), rp)
    }
  }
}

trait RolePermissionService extends AbstractDao[RolePermissionTable, RolePermission, RolePermission] {
  override protected def tableQuery: TableQuery[RolePermissionTable] = TableQuery[RolePermissionTable]

  override protected def toAtomic(query: Query[RolePermissionTable, RolePermission, Seq]): Future[Seq[RolePermission]] = ???

  override protected def toUniqueEntity(query: Query[RolePermissionTable, RolePermission, Seq]): Future[Seq[RolePermission]] = ???
}

object RoleService2 extends RoleService2
object RolePermissionService extends RolePermissionService