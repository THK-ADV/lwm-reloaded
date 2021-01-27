package database

import models.AnnotationLike.Annotation
import models.UniqueDbEntity
import org.joda.time.DateTime
import slick.jdbc.PostgresProfile.api._
import utils.date.DateTimeOps.{DateTimeConverter, SqlTimestampConverter}

import java.sql.Timestamp
import java.util.UUID

class AnnotationTable(tag: Tag) extends Table[AnnotationDb](tag, "ANNOTATIONS") with UniqueTable with UserIdTable with ReportCardEntryIdTable {

  def message = column[String]("MESSAGE")

  override protected def userColumnName: String = "AUTHOR"

  override def * = (reportCardEntry, user, message, lastModified, invalidated, id) <> ((AnnotationDb.apply _).tupled, AnnotationDb.unapply)
}

case class AnnotationDb(reportCardEntry: UUID, author: UUID, message: String, lastModified: Timestamp = DateTime.now.timestamp, invalidated: Option[Timestamp] = None, id: UUID = UUID.randomUUID) extends UniqueDbEntity {
  override def toUniqueEntity: Annotation = Annotation(reportCardEntry, author, message, lastModified.dateTime, id)
}