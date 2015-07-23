package utils

import play.api.http.ContentTypes
import play.api.mvc.Accepting

import scala.language.implicitConversions

case class LWMMimeType(value: String)

object LWMMimeType {

  implicit def unboxMimeType(mime: LWMMimeType): String = mime.value

  val loginV1Json = LWMMimeType("application/vnd.fhk.login.V1+json")

  val studentV1Json = LWMMimeType("application/vnd.fhk.student.V1+json")

  val employeeV1Json = LWMMimeType("application/vnd.fhk.employee.V1+json")

  val courseV1Json = LWMMimeType("application/vnd.fhk.course.V1+json")

  val degreeV1Json = LWMMimeType("application/vnd.fhk.degree.V1+json")

  val groupV1Json = LWMMimeType("application/vnd.fhk.group.V1+json")

  val laboworkV1Json = LWMMimeType("application/vnd.fhk.labwork.V1+json")

  val roomV1Json = LWMMimeType("application/vnd.fhk.room.V1+json")

  val semesterV1Json = LWMMimeType("application/vnd.fhk.semester.V1+json")

  val groupScheduleAssociationV1Json = LWMMimeType("application/vnd.fhk.groupScheduleAssociation.V1+json")

  val studentScheduleAssociationV1Json = LWMMimeType("application/vnd.fhk.studentScheduleAssociation.V1+json")

  val timetableEntryV1Json = LWMMimeType("application/vnd.fhk.timetableEntry.V1+json")
}

object LWMContentTypes extends ContentTypes {

  import play.api.mvc.Codec

  def loginV1ContentType(implicit codec: Codec) = withCharset(LWMMimeType.loginV1Json)

  def studentV1ContentType(implicit codec: Codec) = withCharset(LWMMimeType.studentV1Json)

  def employeeV1ContentType(implicit codec: Codec) = withCharset(LWMMimeType.employeeV1Json)

  def courseV1ContentType(implicit codec: Codec) = withCharset(LWMMimeType.courseV1Json)

  def degreeV1ContentType(implicit codec: Codec) = withCharset(LWMMimeType.degreeV1Json)

  def groupV1ContentType(implicit codec: Codec) = withCharset(LWMMimeType.groupV1Json)

  def labworkV1ContentType(implicit codec: Codec) = withCharset(LWMMimeType.laboworkV1Json)

  def roomV1ContentType(implicit codec: Codec) = withCharset(LWMMimeType.roomV1Json)

  def semesterV1ContentType(implicit codec: Codec) = withCharset(LWMMimeType.semesterV1Json)

  def groupScheduleAssociationV1ContentType(implicit codec: Codec) = withCharset(LWMMimeType.groupScheduleAssociationV1Json)

  def studentScheduleAssociationV1ContentType(implicit codec: Codec) = withCharset(LWMMimeType.studentScheduleAssociationV1Json)

  def timetableEntryV1ContentType(implicit codec: Codec) = withCharset(LWMMimeType.timetableEntryV1Json)
}

object LWMAccepts {
  val LoginV1Accept = Accepting(LWMMimeType.loginV1Json)
  val StudentV1Accept = Accepting(LWMMimeType.studentV1Json)
  val EmployeeV1Accept = Accepting(LWMMimeType.employeeV1Json)
  val CourseV1Accept = Accepting(LWMMimeType.courseV1Json)
  val DegreeV1Accept = Accepting(LWMMimeType.degreeV1Json)
  val GroupV1Accept = Accepting(LWMMimeType.groupV1Json)
  val LabworkV1Accept = Accepting(LWMMimeType.laboworkV1Json)
  val RoomV1Accept = Accepting(LWMMimeType.roomV1Json)
  val SemesterV1Accept = Accepting(LWMMimeType.semesterV1Json)
  val StudentScheduleAssociationV1Accept = Accepting(LWMMimeType.studentScheduleAssociationV1Json)
  val TimetableEntryV1Accept = Accepting(LWMMimeType.timetableEntryV1Json)
}