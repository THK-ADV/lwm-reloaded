package dao

import java.util.UUID

import javax.inject.Inject
import models._
import slick.jdbc.PostgresProfile
import slick.jdbc.PostgresProfile.api._
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

  import utils.LwmDateTime._

  import scala.concurrent.ExecutionContext.Implicits.global

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

    db.run(mandatory.result.map(_.map(t => build(t._1, t._2)).toSeq))
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

final class ReportCardRescheduledDaoImpl @Inject()(val db: PostgresProfile.backend.Database) extends ReportCardRescheduledDao
