package dao

import java.util.UUID

import models._
import utils.LwmDateTime._
import slick.driver.PostgresDriver.api._
import slick.driver.PostgresDriver
import slick.lifted.TableQuery
import store.{ReportCardEntryTypeTable, ReportCardRetryTable, TableFilter}

case class ReportCardRetryEntryFilter(value: String) extends TableFilter[ReportCardRetryTable] {
  override def predicate = _.reportCardEntry === UUID.fromString(value)
}

case class ReportCardRetryLabworkFilter(value: String) extends TableFilter[ReportCardRetryTable] {
  override def predicate = _.joinReportCardEntry.map(_.labwork).filter(_ === UUID.fromString(value)).exists
}

case class ReportCardRetryCourseFilter(value: String) extends TableFilter[ReportCardRetryTable] {
  override def predicate = _.joinReportCardEntry.map(_.memberOfCourse(value)).exists
}

trait ReportCardRetryDao extends AbstractDao[ReportCardRetryTable, ReportCardRetryDb, ReportCardRetry] {

  import scala.concurrent.ExecutionContext.Implicits.global

  override val tableQuery = TableQuery[ReportCardRetryTable]
  protected val entryTypeQuery: TableQuery[ReportCardEntryTypeTable] = TableQuery[ReportCardEntryTypeTable]

  override protected def toAtomic(query: Query[ReportCardRetryTable, ReportCardRetryDb, Seq]) = collectDependencies(query) {
    case (entry, room, entryTypes) => PostgresReportCardRetryAtom(
      entry.date.localDate,
      entry.start.localTime,
      entry.end.localTime,
      room.toLwmModel,
      entryTypes.map(_.toLwmModel).toSet,
      entry.reason,
      entry.id
    )
  }

  override protected def toUniqueEntity(query: Query[ReportCardRetryTable, ReportCardRetryDb, Seq]) = collectDependencies(query) {
    case (entry, _, entryTypes) => entry.copy(entryTypes = entryTypes.toSet).toLwmModel
  }

  private def collectDependencies(query: Query[ReportCardRetryTable, ReportCardRetryDb, Seq])
                                 (build: (ReportCardRetryDb, RoomDb, Seq[ReportCardEntryTypeDb]) => ReportCardRetry) = {
    val mandatory = for {
      q <- query
      r <- q.roomFk
    } yield (q, r)

    val action = mandatory.joinLeft(entryTypeQuery).on(_._1.id === _.reportCardRetry).result.map(_.groupBy(_._1._1.id).map {
      case (id, dependencies) =>
        val ((retry, room), _) = dependencies.find(_._1._1.id == id).get // lhs
      val entryTypes = dependencies.flatMap(_._2) // rhs

        build(retry, room, entryTypes)
    }.toSeq)

    db.run(action)
  }

  override protected def existsQuery(entity: ReportCardRetryDb): Query[ReportCardRetryTable, ReportCardRetryDb, Seq] = {
    filterBy(List(ReportCardRetryEntryFilter(entity.reportCardEntry.toString)))
  }

  override protected def shouldUpdate(existing: ReportCardRetryDb, toUpdate: ReportCardRetryDb): Boolean = {
    (!existing.date.equals(toUpdate.date) ||
      !existing.start.equals(toUpdate.start) ||
      !existing.end.equals(toUpdate.end) ||
      existing.reason != toUpdate.reason ||
      existing.room != toUpdate.room ||
      existing.entryTypes != toUpdate.entryTypes) &&
      existing.reportCardEntry == toUpdate.reportCardEntry
  }

  override protected def databaseExpander: Option[DatabaseExpander[ReportCardRetryDb]] = Some(new DatabaseExpander[ReportCardRetryDb] {

    override def expandCreationOf[E <: Effect](entities: Seq[ReportCardRetryDb]) = {
     for {
        _ <- entryTypeQuery ++= entities.flatMap(_.entryTypes)
      } yield entities
    }

    override def expandDeleteOf(entity: ReportCardRetryDb) = (for {
      d1 <- entryTypeQuery.filter(_.reportCardRetry === entity.id).delete
    } yield d1).map(_ => Some(entity))

    override def expandUpdateOf(entity: ReportCardRetryDb) = for {
      d <- expandDeleteOf(entity) if d.isDefined
      c <- expandCreationOf(Seq(entity))
    } yield c.headOption
  })
}

final class ReportCardRetryDaoImpl(val db: PostgresDriver.backend.Database) extends ReportCardRetryDao