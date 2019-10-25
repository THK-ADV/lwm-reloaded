package dao

import java.util.UUID

import dao.helper.{DatabaseExpander, TableFilter}
import database._
import javax.inject.Inject
import models._
import slick.dbio.Effect
import slick.jdbc.PostgresProfile
import slick.jdbc.PostgresProfile.api._
import slick.lifted.TableQuery

import scala.concurrent.{ExecutionContext, Future}

object AssignmentEntryDao extends TableFilter[AssignmentEntryTable] {

  def labworkFilter(labwork: UUID): TableFilterPredicate = TableFilter.labworkFilter(labwork)

  def courseFilter(course: UUID): TableFilterPredicate = TableFilter.courseFilter(course)

  def indexFilter(index: Int): TableFilterPredicate = _.index === index
}

trait AssignmentEntryDao extends AbstractDao[AssignmentEntryTable, AssignmentEntryDb, AssignmentEntryLike] {

  import AssignmentEntryDao._

  override val tableQuery = TableQuery[AssignmentEntryTable]

  val assignmentEntryTypeQuery: TableQuery[AssignmentEntryTypeTable] = TableQuery[AssignmentEntryTypeTable]

  def withSameLabworkAs(id: UUID): DBIOAction[Seq[AssignmentEntryDb], NoStream, Effect.Read] = {
    for {
      x <- filterValidOnly(_.id === id).take(1).result.headOption if x.isDefined
      xs <- filterValidOnly(_.labwork === x.get.labwork).result
    } yield xs
  }

  def updateIndices(xs: List[(UUID, Int)]): DBIOAction[List[Int], NoStream, Effect.Write with Effect.Transactional] = {
    val query = xs.map {
      case (id, i) => filterValidOnly(a => a.id === id).map(_.index).update(i)
    }

    DBIO.sequence(query).transactionally
  }

  override protected def toAtomic(query: Query[AssignmentEntryTable, AssignmentEntryDb, Seq]): Future[Seq[AssignmentEntryLike]] = collectDependencies(query) {
    case (entry, labwork, types) => AssignmentEntryAtom(labwork.toUniqueEntity, entry.index, entry.label, types, entry.duration, entry.id)
  }

  private def collectDependencies(query: Query[AssignmentEntryTable, AssignmentEntryDb, Seq])
    (build: (AssignmentEntryDb, LabworkDb, Set[AssignmentEntryType]) => AssignmentEntryLike) = {
    val mandatory = for {
      q <- query
      l <- q.labworkFk
    } yield (q, l)

    val action = mandatory.joinLeft(assignmentEntryTypeQuery).on(_._1.id === _.assignmentEntry)
      .result
      .map(_.groupBy(_._1._1.id).map {
        case (id, dependencies) =>
          val ((entry, labwork), _) = dependencies.find(_._1._1.id == id).get
          val types = dependencies.flatMap(_._2).map(e => AssignmentEntryType(e.entryType)).toSet

          build(entry, labwork, types)
      }.toSeq)

    db.run(action)
  }

  override protected def toUniqueEntity(query: Query[AssignmentEntryTable, AssignmentEntryDb, Seq]): Future[Seq[AssignmentEntryLike]] = collectDependencies(query) {
    case (entry, labwork, types) => AssignmentEntry(labwork.id, entry.index, entry.label, types, entry.duration, entry.id)
  }

  override protected def existsQuery(entity: AssignmentEntryDb): Query[AssignmentEntryTable, AssignmentEntryDb, Seq] = {
    filterBy(List(labworkFilter(entity.labwork), indexFilter(entity.index)))
  }

  override protected def shouldUpdate(existing: AssignmentEntryDb, toUpdate: AssignmentEntryDb): Boolean = {
    existing.labwork == toUpdate.labwork && existing.index == toUpdate.index
  }

  override protected val databaseExpander: Option[DatabaseExpander[AssignmentEntryDb]] = Some(new DatabaseExpander[AssignmentEntryDb] {

    override def expandCreationOf[X <: Effect](entities: AssignmentEntryDb*): DBIOAction[Seq[AssignmentEntryDb], NoStream, Effect.Write with Any] = {
      for {
        _ <- assignmentEntryTypeQuery ++= entities.flatMap(_.types)
      } yield entities
    }

    override def expandDeleteOf(entity: AssignmentEntryDb) = {
      for {
        _ <- assignmentEntryTypeQuery.filter(_.assignmentEntry === entity.id).delete
      } yield entity
    }

    override def expandUpdateOf(entity: AssignmentEntryDb) = {
      for {
        d <- expandDeleteOf(entity)
        c <- expandCreationOf(d)
      } yield c.head
    }
  })

  override protected val schemas: List[PostgresProfile.DDL] = List(
    tableQuery.schema,
    assignmentEntryTypeQuery.schema
  )
}

final class AssignmentEntryDaoImpl @Inject()(val db: Database, val executionContext: ExecutionContext) extends AssignmentEntryDao