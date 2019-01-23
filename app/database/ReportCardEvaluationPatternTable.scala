package database

import java.sql.Timestamp
import java.util.UUID

import models.{EvaluationProperty, ReportCardEvaluationPattern, ReportCardEvaluationPatternProtocol, UniqueDbEntity}
import org.joda.time.DateTime
import slick.jdbc.PostgresProfile.api._
import utils.LwmDateTime.DateTimeConverter

class ReportCardEvaluationPatternTable(tag: Tag) extends Table[ReportCardEvaluationPatternDb](tag, "REPORT_CARD_EVALUATION_PATTERN") with UniqueTable with LabworkIdTable with EntryTypeTable {
  def min = column[Int]("MIN")

  def property = column[String]("property")

  override def * = (labwork, entryType, min, property, lastModified, invalidated, id) <> ((ReportCardEvaluationPatternDb.apply _).tupled, ReportCardEvaluationPatternDb.unapply)
}

case class ReportCardEvaluationPatternDb(labwork: UUID, entryType: String, min: Int, property: String, lastModified: Timestamp = DateTime.now.timestamp, invalidated: Option[Timestamp] = None, id: UUID = UUID.randomUUID) extends UniqueDbEntity {
  override def toUniqueEntity = ReportCardEvaluationPattern(labwork, entryType, min, EvaluationProperty(property), id)
}

object ReportCardEvaluationPatternDb {
  def from(protocol: ReportCardEvaluationPatternProtocol, existingId: Option[UUID]) = {
    ReportCardEvaluationPatternDb(protocol.labwork, protocol.entryType, protocol.min, protocol.property.toString, id = existingId getOrElse UUID.randomUUID)
  }
}