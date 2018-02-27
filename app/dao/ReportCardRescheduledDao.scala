package dao

import java.util.UUID

import models._
import slick.driver.PostgresDriver
import slick.driver.PostgresDriver.api._
import store.{ReportCardRescheduledTable, TableFilter}

case class ReportCardRescheduledEntryFilter(value: String) extends TableFilter[ReportCardRescheduledTable] {
  override def predicate = _.reportCardEntry === UUID.fromString(value)
}

case class ReportCardRescheduledLabworkFilter(value: String) extends TableFilter[ReportCardRescheduledTable] {
  override def predicate = _.joinReportCardEntry.map(_.labwork).filter(_ === UUID.fromString(value)).exists
}

case class ReportCardRescheduledCourseFilter(value: String) extends TableFilter[ReportCardRescheduledTable] {
  override def predicate = _.joinReportCardEntry.map(_.memberOfCourse(value)).exists
}

trait ReportCardRescheduledDao extends AbstractDao[ReportCardRescheduledTable, ReportCardRescheduledDb, ReportCardRescheduled] {
  import scala.concurrent.ExecutionContext.Implicits.global
  import utils.LwmDateTime._

  override val tableQuery = TableQuery[ReportCardRescheduledTable]

  override protected def toAtomic(query: Query[ReportCardRescheduledTable, ReportCardRescheduledDb, Seq]) = collectDependencies(query) {
    case (entry, room) => PostgresReportCardRescheduledAtom(entry.date.localDate, entry.start.localTime, entry.end.localTime, room.toLwmModel, entry.reason, entry.id)
  }

  override protected def toUniqueEntity(query: Query[ReportCardRescheduledTable, ReportCardRescheduledDb, Seq]) = collectDependencies(query) {
    case (entry, _) => entry.toLwmModel
  }

  private def collectDependencies(query: Query[ReportCardRescheduledTable, ReportCardRescheduledDb, Seq])
                                 (build: (ReportCardRescheduledDb, RoomDb) => ReportCardRescheduled) = {
    val mandatory = for {
      q <- query
      r <- q.roomFk
    } yield (q, r)

    mandatory.result.map(_.map(t => build(t._1, t._2)))
  }

  override protected def existsQuery(entity: ReportCardRescheduledDb): Query[ReportCardRescheduledTable, ReportCardRescheduledDb, Seq] = {
    filterBy(List(ReportCardRescheduledEntryFilter(entity.reportCardEntry.toString)))
  }

  override protected def shouldUpdate(existing: ReportCardRescheduledDb, toUpdate: ReportCardRescheduledDb): Boolean = {
    (!existing.date.equals(toUpdate.date) ||
      !existing.start.equals(toUpdate.start) ||
      !existing.end.equals(toUpdate.end) ||
      existing.reason != toUpdate.reason ||
      existing.room != toUpdate.room) &&
      existing.reportCardEntry == toUpdate.reportCardEntry
  }
}

final class ReportCardRescheduledImpl(val db: PostgresDriver.backend.Database) extends ReportCardRescheduledDao
