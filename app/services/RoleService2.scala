package services

import models._
import org.joda.time.DateTime
import store.{RolePermissionTable, RoleTable, TableFilter}
import slick.driver.PostgresDriver.api._

import scala.concurrent.Future
import models.LwmDateTime.DateTimeConverter
import slick.driver
import slick.driver.PostgresDriver

case class RoleLabelFilter(value: String) extends TableFilter[RoleTable] {
  override def predicate = _.label.toLowerCase === value.toLowerCase
}

trait RoleService2 extends AbstractDao[RoleTable, RoleDb, Role] {

  import scala.concurrent.ExecutionContext.Implicits.global

  override val tableQuery: TableQuery[RoleTable] = TableQuery[RoleTable]

  protected val rolePermissionQuery: TableQuery[RolePermissionTable] = TableQuery[RolePermissionTable]

  final def byUserStatus(status: String): Future[Option[RoleDb]] = {
    db.run(tableQuery.filter(_.isLabel(Roles.fromUserStatus(status))).result.headOption)
  }

  override protected def setInvalidated(entity: RoleDb): RoleDb = {
    val now = DateTime.now.timestamp

    entity.copy(entity.label, entity.permissions, now, Some(now))
  }

  override protected def shouldUpdate(existing: RoleDb, toUpdate: RoleDb): Boolean = {
    existing.permissions != toUpdate.permissions && existing.label == toUpdate.label
  }

  override protected def existsQuery(entity: RoleDb): Query[RoleTable, RoleDb, Seq] = {
    filterBy(List(RoleLabelFilter(entity.label)))
  }

  override protected def toAtomic(query: Query[RoleTable, RoleDb, Seq]): Future[Seq[Role]] = joinPermissions(query) {
    case (role, rolePerms) =>
      val permissions = rolePerms.map {
        case (_, perm) => PostgresPermission(perm.value, perm.description, perm.id)
      }

      PostgresRoleAtom(role.label, permissions.toSet, role.id)
  }

  private def joinPermissions(query: Query[RoleTable, RoleDb, Seq])(buildRole: (RoleDb, Seq[(RoleDb, PermissionDb)]) => Role): Future[Seq[Role]] = {
    val rolesWithPermissions = for {
      q <- query
      p <- q.permissions
    } yield (q, p)

    db.run(rolesWithPermissions.result.map(_.groupBy(_._1).map(r => buildRole(r._1, r._2)).toSeq))
  }

  override protected def toUniqueEntity(query: Query[RoleTable, RoleDb, Seq]): Future[Seq[Role]] = joinPermissions(query) {
    case (role, rolePerms) => PostgresRole(role.label, rolePerms.map(_._2.id).toSet, role.id)
  }

  override protected def databaseExpander: Option[DatabaseExpander[RoleDb]] = Some(new DatabaseExpander[RoleDb] {
    override def expandCreationOf[X <: Effect](entities: Seq[RoleDb]) = {
      val rolePermissions = entities.flatMap(r => r.permissions.map(p => RolePermission(r.id, p)))

      (rolePermissionQuery ++= rolePermissions).map(_ => entities)
    }

    override def expandDeleteOf(entity: RoleDb) = {
      rolePermissionQuery.filter(_.role === entity.id).delete.map(_ => Some(entity))
    }

    override def expandUpdateOf(entity: RoleDb) = {
      for {
        deleted <- expandDeleteOf(entity) if deleted.isDefined
        created <- expandCreationOf(Seq(entity))
      } yield created.headOption
    }
  })

  private lazy val schemas = List(
    tableQuery.schema,
    rolePermissionQuery.schema
  )

  override def createSchema: Future[Unit] = {
    db.run(DBIO.seq(schemas.map(_.create): _*).transactionally)
  }

  override def dropSchema: Future[Unit] = {
    db.run(DBIO.seq(schemas.reverseMap(_.drop): _*).transactionally)
  }
}

final class RoleServiceImpl(val db: PostgresDriver.backend.Database) extends RoleService2