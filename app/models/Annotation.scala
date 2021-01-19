package models

import org.joda.time.DateTime

import java.util.UUID

sealed trait AnnotationLike extends UniqueEntity

object AnnotationLike {

  case class Annotation(reportCardEntry: UUID, author: UUID, message: String, lastModified: DateTime, id: UUID) extends AnnotationLike

  case class AnnotationAtom(reportCardEntry: ReportCardEntry, author: User, message: String, lastModified: DateTime, id: UUID) extends AnnotationLike

}

case class AnnotationProtocol(reportCardEntry: UUID, author: UUID, message: String)