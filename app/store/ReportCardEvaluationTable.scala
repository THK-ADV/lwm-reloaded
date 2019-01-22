package store

import java.sql.Timestamp
import java.util.UUID

import models.{ReportCardEvaluation, ReportCardEvaluationProtocol, UniqueDbEntity}
import org.joda.time.DateTime
import slick.jdbc.PostgresProfile.api._
import utils.LwmDateTime.{DateTimeConverter, SqlTimestampConverter}

class ReportCardEvaluationTable(tag: Tag) extends Table[ReportCardEvaluationDb](tag, "REPORT_CARD_EVALUATION") with UniqueTable with StudentIdTable with LabworkIdTable with LabelTable with EntryTypeLikeTable {

  override def * = (student, labwork, label, bool, int, lastModified, invalidated, id) <> ((ReportCardEvaluationDb.apply _).tupled, ReportCardEvaluationDb.unapply)
}

case class ReportCardEvaluationDb(student: UUID, labwork: UUID, label: String, bool: Boolean, int: Int, lastModified: Timestamp = DateTime.now.timestamp, invalidated: Option[Timestamp] = None, id: UUID = UUID.randomUUID) extends UniqueDbEntity {
  override def toUniqueEntity = ReportCardEvaluation(student, labwork, label, bool, int, lastModified.dateTime, id)
}

object ReportCardEvaluationDb {
  def from(p: ReportCardEvaluationProtocol, existing: Option[UUID]) = {
    ReportCardEvaluationDb(p.student, p.labwork, p.label, p.bool, p.int, id = existing getOrElse UUID.randomUUID)
  }
}

