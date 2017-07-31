package dao

import models._
import slick.dbio.Effect
import slick.driver.PostgresDriver
import slick.driver.PostgresDriver.api._
import store.{RolePermissionTable, RoleTable, TableFilter}

import scala.concurrent.Future

case class RoleLabelFilter(value: String) extends TableFilter[RoleTable] {
  override def predicate = _.label.toLowerCase === value.toLowerCase
}

trait RoleService2 extends AbstractDao[RoleTable, RoleDb, Role] {

  import scala.concurrent.ExecutionContext.Implicits.global

  override val tableQuery: TableQuery[RoleTable] = TableQuery[RoleTable]

  protected val rolePermissionQuery: TableQuery[RolePermissionTable] = TableQuery[RolePermissionTable]

  override protected def shouldUpdate(existing: RoleDb, toUpdate: RoleDb): Boolean = {
    existing.permissions != toUpdate.permissions && existing.label == toUpdate.label
  }

  override protected def existsQuery(entity: RoleDb): Query[RoleTable, RoleDb, Seq] = {
    filterBy(List(RoleLabelFilter(entity.label)))
  }

  override protected def toAtomic(query: Query[RoleTable, RoleDb, Seq]): Future[Seq[Role]] = joinPermissions(query) {
    case (role, perms) => PostgresRoleAtom(role.label, perms.map(_.toLwmModel).toSet, role.id)
  }

  override protected def toUniqueEntity(query: Query[RoleTable, RoleDb, Seq]): Future[Seq[Role]] = joinPermissions(query) {
    case (role, perms) => PostgresRole(role.label, perms.map(_.id).toSet, role.id)
  }

  private def joinPermissions(query: Query[RoleTable, RoleDb, Seq])(build: (RoleDb, Seq[PermissionDb]) => Role): Future[Seq[Role]] = {
    val rolePerms = for {
      rp <- rolePermissionQuery
      p <- rp.permissionFk
    } yield (rp, p)

    val result = query.joinLeft(rolePerms).on(_.id === _._1.role).result.map(_.groupBy(_._1).map {
      case (role, dependencies) =>
        val perms = dependencies.flatMap(_._2.map(_._2))

        build(role, perms)
    }.toSeq)

    db.run(result)
  }

  def byUserStatus(status: String): Future[Option[RoleDb]] = { // TODO get rid of db.run calls. return queries instead
    db.run(byUserStatusQuery(status))
  }

  def byUserStatusQuery(status: String): DBIOAction[Option[RoleDb], NoStream, Effect.Read] = {
    tableQuery.filter(_.isLabel(Roles.fromUserStatus(status))).result.headOption
  }

  def byRoleLabelQuery(label: String): DBIOAction[Option[RoleDb], NoStream, Effect.Read] = {
    tableQuery.filter(_.isLabel(label)).result.headOption
  }

  override protected def databaseExpander: Option[DatabaseExpander[RoleDb]] = Some(new DatabaseExpander[RoleDb] {
    override def expandCreationOf[X <: Effect](entities: Seq[RoleDb]): DBIOAction[Seq[RoleDb], NoStream, Effect.Write] = {
      val rolePermissions = entities.flatMap(r => r.permissions.map(p => RolePermission(r.id, p)))

      (rolePermissionQuery ++= rolePermissions).map(_ => entities)
    }

    override def expandDeleteOf(entity: RoleDb): DBIOAction[Some[RoleDb], NoStream, Effect.Write] = {
      rolePermissionQuery.filter(_.role === entity.id).delete.map(_ => Some(entity))
    }

    override def expandUpdateOf(entity: RoleDb): DBIOAction[Option[RoleDb], NoStream, Effect.Write with Effect.Write] = {
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