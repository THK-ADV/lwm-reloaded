package dao

import java.util.UUID

import database._
import javax.inject.Inject
import models._
import slick.jdbc.PostgresProfile
import slick.jdbc.PostgresProfile.api._

import scala.concurrent.Future

case class GroupLabworkTableFilter(value: String) extends TableFilter[GroupTable] {
  override def predicate = _.labwork === UUID.fromString(value)
}

case class GroupStudentTableFilter(value: String) extends TableFilter[GroupTable] {
  override def predicate = _.contains(UUID.fromString(value))
}

case class GroupLabelTableFilter(value: String) extends TableFilter[GroupTable] {
  override def predicate = _.label.toLowerCase === value.toLowerCase
}

trait GroupDao extends AbstractDao[GroupTable, GroupDb, GroupLike] {

  import scala.concurrent.ExecutionContext.Implicits.global

  override val tableQuery = TableQuery[GroupTable]
  protected val groupMembershipQuery: TableQuery[GroupMembershipTable] = TableQuery[GroupMembershipTable]

  override protected def toAtomic(query: Query[GroupTable, GroupDb, Seq]): Future[Seq[GroupLike]] = collectDependencies(query) {
    case (g, l, m) => GroupAtom(g.label, l.toUniqueEntity, m.map(_.toUniqueEntity).toSet, g.id)
  }

  override protected def toUniqueEntity(query: Query[GroupTable, GroupDb, Seq]): Future[Seq[GroupLike]] = collectDependencies(query) {
    case (g, _, m) => Group(g.label, g.labwork, m.map(_.id).toSet, g.id)
  }

  private def collectDependencies(query: Query[GroupTable, GroupDb, Seq])
    (build: (GroupDb, LabworkDb, Seq[UserDb]) => GroupLike): Future[Seq[GroupLike]] = {
    val mandatory = for {
      q <- query
      l <- q.labworkFk
    } yield (q, l)

    db.run(mandatory
      .joinLeft(groupMembershipQuery).on(_._1.id === _.group)
      .joinLeft(TableQuery[UserTable]).on((l, r) => l._2.map(_.student === r.id).getOrElse(false))
      .result.map(_.groupBy(_._1._1._1).map {
      case (group, dependencies) =>
        val members = dependencies.flatMap(_._2)
        val labwork = dependencies.find(_._1._1._1.labwork == group.labwork).head._1._1._2

        build(group, labwork, members)
    }.toSeq))
  }

  override protected def existsQuery(entity: GroupDb): Query[GroupTable, GroupDb, Seq] = {
    filterBy(List(IdFilter(entity.id.toString)))
  }

  override protected def shouldUpdate(existing: GroupDb, toUpdate: GroupDb): Boolean = {
    existing.members != toUpdate.members ||
      existing.label != toUpdate.label ||
      existing.labwork != toUpdate.labwork &&
        (existing.id == toUpdate.id)
  }

  override protected def databaseExpander: Option[DatabaseExpander[GroupDb]] = Some(new DatabaseExpander[GroupDb] {
    override def expandCreationOf[E <: Effect](entities: Seq[GroupDb]) = for {
      _ <- groupMembershipQuery ++= entities.flatMap(g => g.members.map(s => GroupMembership(g.id, s)))
    } yield entities

    override def expandDeleteOf(entity: GroupDb) = for {
      _ <- groupMembershipQuery.filter(_.group === entity.id).delete
    } yield entity

    override def expandUpdateOf(entity: GroupDb) = for {
      d <- expandDeleteOf(entity)
      c <- expandCreationOf(Seq(d))
    } yield c.head
  })

  private lazy val schemas = List(
    tableQuery.schema,
    groupMembershipQuery.schema
  )

  override def createSchema: Future[Unit] = {
    db.run(DBIO.seq(schemas.map(_.create): _*).transactionally)
  }

  override def dropSchema: Future[Unit] = {
    db.run(DBIO.seq(schemas.reverseMap(_.drop): _*).transactionally)
  }
}

final class GroupDaoImpl @Inject()(val db: PostgresProfile.backend.Database) extends GroupDao
