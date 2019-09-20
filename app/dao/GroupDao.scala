package dao

import java.util.UUID

import dao.helper.{DatabaseExpander, TableFilter}
import database._
import javax.inject.Inject
import models._
import slick.dbio.Effect
import slick.jdbc
import slick.jdbc.PostgresProfile
import slick.jdbc.PostgresProfile.api._
import slick.sql.{FixedSqlAction, SqlAction}

import scala.concurrent.{ExecutionContext, Future}

object GroupDao extends TableFilter[GroupTable] {
  def studentFilter(student: UUID): TableFilterPredicate = _.contains(student)
}

trait GroupDao extends AbstractDao[GroupTable, GroupDb, GroupLike] {

  import TableFilter.{groupFilter, labelFilterEquals, labworkFilter, userFilter}

  override val tableQuery = TableQuery[GroupTable]

  val groupMembershipQuery: TableQuery[GroupMembershipTable] = TableQuery[GroupMembershipTable]

  def firstStudentIn(group: UUID): SqlAction[Option[UUID], NoStream, Effect.Read] = {
    groupMembershipQuery.filter(_.group === group).map(_.user).result.headOption
  }

  def groupHasAtLeastTwoMembers(group: UUID): FixedSqlAction[Boolean, NoStream, Effect.Read] = {
    (groupMembershipQuery.filter(groupFilter(group)).size > 1).result
  }

  def add(student: UUID, group: UUID): DBIOAction[GroupMembership, NoStream, Effect.Read with Effect.Write] = {
    for {
      exists <- isInGroup(student, group)
      membership <- if (exists)
        DBIO.failed(new Throwable(s"student $student is already in group $group"))
      else
        (groupMembershipQuery returning groupMembershipQuery) += GroupMembership(group, student)
    } yield membership
  }

  def remove(student: UUID, group: UUID): DBIOAction[Int, NoStream, Effect.Read with Effect.Write] = {
    for {
      exists <- isInGroup(student, group)
      membership <- if (exists)
        groupMembershipQuery.filter(g => g.group === group && g.user === student).delete
      else
        DBIO.failed(new Throwable(s"student $student is not a member of group $group"))
    } yield membership
  }

  def isInGroup(student: UUID, group: UUID): FixedSqlAction[Boolean, NoStream, Effect.Read] = {
    groupMembershipQuery.filter(t => groupFilter(group).apply(t) && userFilter(student).apply(t)).exists.result
  }

  override protected def toAtomic(query: Query[GroupTable, GroupDb, Seq]): Future[Seq[GroupLike]] = collectDependencies(query) {
    case (g, l, m) => GroupAtom(g.label, l.toUniqueEntity, m.map(_.toUniqueEntity).toSet, g.id)
  }

  override protected def toUniqueEntity(query: Query[GroupTable, GroupDb, Seq]): Future[Seq[GroupLike]] = collectDependencies(query) {
    case (g, _, m) => Group(g.label, g.labwork, m.map(_.id).toSet, g.id)
  }

  private def collectDependencies(query: Query[GroupTable, GroupDb, Seq])
    (build: (GroupDb, LabworkDb, Seq[UserDb]) => GroupLike) = {
    val mandatory = for {
      q <- query
      l <- q.labworkFk
    } yield (q, l)

    db.run(mandatory
      .joinLeft(groupMembershipQuery).on(_._1.id === _.group)
      .joinLeft(TableQuery[UserTable]).on((l, r) => l._2.map(_.user === r.id).getOrElse(false))
      .result.map(_.groupBy(_._1._1._1).map {
      case (group, dependencies) =>
        val members = dependencies.flatMap(_._2)
        val labwork = dependencies.find(_._1._1._1.labwork == group.labwork).head._1._1._2

        build(group, labwork, members)
    }.toSeq))
  }

  override protected def existsQuery(entity: GroupDb): Query[GroupTable, GroupDb, Seq] = {
    filterBy(List(labworkFilter(entity.labwork), labelFilterEquals(entity.label)))
  }

  override protected def shouldUpdate(existing: GroupDb, toUpdate: GroupDb): Boolean = {
    existing.labwork == toUpdate.labwork && existing.label == toUpdate.label
  }

  override protected val databaseExpander: Option[DatabaseExpander[GroupDb]] = Some(new DatabaseExpander[GroupDb] {
    override def expandCreationOf[E <: Effect](entities: GroupDb*): jdbc.PostgresProfile.api.DBIOAction[Seq[GroupDb], jdbc.PostgresProfile.api.NoStream, Effect.Write with Any] = for {
      _ <- groupMembershipQuery ++= entities.flatMap(g => g.members.map(s => GroupMembership(g.id, s)))
    } yield entities

    override def expandDeleteOf(entity: GroupDb): DBIOAction[GroupDb, NoStream, Effect.Write] = for {
      _ <- groupMembershipQuery.filter(_.group === entity.id).delete
    } yield entity

    override def expandUpdateOf(entity: GroupDb): DBIOAction[GroupDb, NoStream, Effect.Write with Effect.Write] = for {
      d <- expandDeleteOf(entity)
      c <- expandCreationOf(d)
    } yield c.head
  })

  override protected val schemas: List[PostgresProfile.DDL] = List(
    tableQuery.schema,
    groupMembershipQuery.schema
  )
}

final class GroupDaoImpl @Inject()(val db: Database, val executionContext: ExecutionContext) extends GroupDao
