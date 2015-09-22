package utils

import play.api.http.ContentTypes
import play.api.mvc.Accepting

import scala.language.implicitConversions

case class LwmMimeType(value: String)

object LwmMimeType {

  val loginV1Json = LwmMimeType("application/vnd.fhk.login.V1+json")
  val studentV1Json = LwmMimeType("application/vnd.fhk.student.V1+json")
  val employeeV1Json = LwmMimeType("application/vnd.fhk.employee.V1+json")
  val courseV1Json = LwmMimeType("application/vnd.fhk.course.V1+json")
  val degreeV1Json = LwmMimeType("application/vnd.fhk.degree.V1+json")
  val groupV1Json = LwmMimeType("application/vnd.fhk.group.V1+json")
  val labworkV1Json = LwmMimeType("application/vnd.fhk.labwork.V1+json")
  val roomV1Json = LwmMimeType("application/vnd.fhk.room.V1+json")
  val semesterV1Json = LwmMimeType("application/vnd.fhk.semester.V1+json")
  val groupScheduleAssociationV1Json = LwmMimeType("application/vnd.fhk.groupScheduleAssociation.V1+json")
  val studentScheduleAssociationV1Json = LwmMimeType("application/vnd.fhk.studentScheduleAssociation.V1+json")
  val timetableEntryV1Json = LwmMimeType("application/vnd.fhk.timetableEntry.V1+json")
  val refRoleV1Json = LwmMimeType("application/vnd.fhk.refRole.V1+json")
  val authorityV1Json = LwmMimeType("application/vnd.fhk.authority.V1+json")

  implicit def unboxMimeType(mime: LwmMimeType): String = mime.value
}

object LwmContentTypes extends ContentTypes {

  import play.api.mvc.Codec

  def loginV1ContentType(implicit codec: Codec) = withCharset(LwmMimeType.loginV1Json)

  def studentV1ContentType(implicit codec: Codec) = withCharset(LwmMimeType.studentV1Json)

  def employeeV1ContentType(implicit codec: Codec) = withCharset(LwmMimeType.employeeV1Json)

  def courseV1ContentType(implicit codec: Codec) = withCharset(LwmMimeType.courseV1Json)

  def degreeV1ContentType(implicit codec: Codec) = withCharset(LwmMimeType.degreeV1Json)

  def groupV1ContentType(implicit codec: Codec) = withCharset(LwmMimeType.groupV1Json)

  def labworkV1ContentType(implicit codec: Codec) = withCharset(LwmMimeType.labworkV1Json)

  def roomV1ContentType(implicit codec: Codec) = withCharset(LwmMimeType.roomV1Json)

  def semesterV1ContentType(implicit codec: Codec) = withCharset(LwmMimeType.semesterV1Json)

  def groupScheduleAssociationV1ContentType(implicit codec: Codec) = withCharset(LwmMimeType.groupScheduleAssociationV1Json)

  def studentScheduleAssociationV1ContentType(implicit codec: Codec) = withCharset(LwmMimeType.studentScheduleAssociationV1Json)

  def timetableEntryV1ContentType(implicit codec: Codec) = withCharset(LwmMimeType.timetableEntryV1Json)

  def refRoleV1ContentType(implicit codec: Codec) = withCharset(LwmMimeType.refRoleV1Json)

  def authorityV1ContentType(implicit codec: Codec) = withCharset(LwmMimeType.authorityV1Json)
}

object LwmAccepts {
  val LoginV1Accept = Accepting(LwmMimeType.loginV1Json)
  val StudentV1Accept = Accepting(LwmMimeType.studentV1Json)
  val EmployeeV1Accept = Accepting(LwmMimeType.employeeV1Json)
  val CourseV1Accept = Accepting(LwmMimeType.courseV1Json)
  val DegreeV1Accept = Accepting(LwmMimeType.degreeV1Json)
  val GroupV1Accept = Accepting(LwmMimeType.groupV1Json)
  val LabworkV1Accept = Accepting(LwmMimeType.labworkV1Json)
  val RoomV1Accept = Accepting(LwmMimeType.roomV1Json)
  val SemesterV1Accept = Accepting(LwmMimeType.semesterV1Json)
  val StudentScheduleAssociationV1Accept = Accepting(LwmMimeType.studentScheduleAssociationV1Json)
  val TimetableEntryV1Accept = Accepting(LwmMimeType.timetableEntryV1Json)
  val RefRoleV1Accept = Accepting(LwmMimeType.refRoleV1Json)
  val AuthorityV1Accept = Accepting(LwmMimeType.authorityV1Json)
}