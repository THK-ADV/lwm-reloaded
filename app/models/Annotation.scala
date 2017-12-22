package models

import java.util.UUID

import org.joda.time.DateTime

case class Annotation(student: UUID, labwork: UUID, reportCardEntry: UUID, message: String, timestamp: DateTime = DateTime.now, invalidated: Option[DateTime] = None, id: UUID = UUID.randomUUID) extends UniqueEntity

case class AnnotationProtocol(student: UUID, labwork: UUID, reportCardEntry: UUID, message: String)

case class AnnotationAtom(student: SesameStudent, labwork: SesameLabwork, reportCardEntry: SesameReportCardEntry, message: String, timestamp: DateTime, invalidated: Option[DateTime], id: UUID) extends UniqueEntity