package utils

import play.api.http.ContentTypes
import play.api.mvc.Accepting

import scala.language.implicitConversions

case class LwmMimeType(value: String)

object LwmMimeType {
  val userV1Json = LwmMimeType("application/vnd.fhk.user.V1+json")
  val loginV1Json = LwmMimeType("application/vnd.fhk.login.V1+json")
  val courseV1Json = LwmMimeType("application/vnd.fhk.course.V1+json")
  val degreeV1Json = LwmMimeType("application/vnd.fhk.degree.V1+json")
  val groupV1Json = LwmMimeType("application/vnd.fhk.group.V1+json")
  val labworkV1Json = LwmMimeType("application/vnd.fhk.labwork.V1+json")
  val roomV1Json = LwmMimeType("application/vnd.fhk.room.V1+json")
  val semesterV1Json = LwmMimeType("application/vnd.fhk.semester.V1+json")
  val refRoleV1Json = LwmMimeType("application/vnd.fhk.refRole.V1+json")
  val authorityV1Json = LwmMimeType("application/vnd.fhk.authority.V1+json")
  val roleV1Json = LwmMimeType("application/vnd.fhk.role.V1+json")
  val permissionV1Json = LwmMimeType("application/vnd.fhk.permission.V1+json")
  val entryTypeV1Json = LwmMimeType("application/vnd.fhk.entryType.V1+json")
  val labworkApplicationV1Json = LwmMimeType("application/vnd.fhk.labworkApplication.V1+json")
  val scheduleV1Json = LwmMimeType("application/vnd.fhk.schedule.V1+json")
  val scheduleEntryV1Json = LwmMimeType("application/vnd.fhk.scheduleEntry.V1+json")
  val timetableV1Json = LwmMimeType("application/vnd.fhk.timetable.V1+json")
  val blacklistV1Json = LwmMimeType("application/vnd.fhk.blacklist.V1+json")
  val reportCardV1Json = LwmMimeType("application/vnd.fhk.reportCard.V1+json")
  val reportCardEntryV1Json = LwmMimeType("application/vnd.fhk.reportCardEntry.V1+json")
  val reportCardEntryTypeV1Json = LwmMimeType("application/vnd.fhk.reportCardEntryType.V1+json")
  val assignmentPlanV1Json = LwmMimeType("application/vnd.fhk.assignmentPlan.V1+json")
  val annotationV1Json = LwmMimeType("application/vnd.fhk.annotation.V1+json")
  val reportCardEvaluationV1Json = LwmMimeType("application/vnd.fhk.reportCardEvaluation.V1+json")

  val apiDataV1Json = LwmMimeType("application/vnd.fhk.apiData.V1+json")
  val lwmServiceV1Json = LwmMimeType("application/vnd.fhk.lwmService.V1+json")

  implicit def unboxMimeType(mime: LwmMimeType): String = mime.value
}

object LwmContentTypes extends ContentTypes {

  import play.api.mvc.Codec

  def loginV1ContentType(implicit codec: Codec) = withCharset(LwmMimeType.loginV1Json)
  def courseV1ContentType(implicit codec: Codec) = withCharset(LwmMimeType.courseV1Json)
  def degreeV1ContentType(implicit codec: Codec) = withCharset(LwmMimeType.degreeV1Json)
  def groupV1ContentType(implicit codec: Codec) = withCharset(LwmMimeType.groupV1Json)
  def labworkV1ContentType(implicit codec: Codec) = withCharset(LwmMimeType.labworkV1Json)
  def roomV1ContentType(implicit codec: Codec) = withCharset(LwmMimeType.roomV1Json)
  def semesterV1ContentType(implicit codec: Codec) = withCharset(LwmMimeType.semesterV1Json)
  def userV1ContentType(implicit codec: Codec) = withCharset(LwmMimeType.userV1Json)
  def refRoleV1ContentType(implicit codec: Codec) = withCharset(LwmMimeType.refRoleV1Json)
  def authorityV1ContentType(implicit codec: Codec) = withCharset(LwmMimeType.authorityV1Json)
  def roleV1ContentType(implicit codec: Codec) = withCharset(LwmMimeType.roleV1Json)
  def permissionV1ContentType(implicit codec: Codec) = withCharset(LwmMimeType.permissionV1Json)
  def entryTypeV1ContentType(implicit codec: Codec) = withCharset(LwmMimeType.entryTypeV1Json)
  def labworkApplicationV1ContentType(implicit codec: Codec) = withCharset(LwmMimeType.labworkApplicationV1Json)
  def scheduleV1ContentType(implicit codec: Codec) = withCharset(LwmMimeType.scheduleV1Json)
  def scheduleEntryV1ContentType(implicit codec: Codec) = withCharset(LwmMimeType.scheduleEntryV1Json)
  def timetableV1ContentType(implicit codec: Codec) = withCharset(LwmMimeType.timetableV1Json)
  def blacklistV1ContentType(implicit codec: Codec) = withCharset(LwmMimeType.blacklistV1Json)
  def reportCardV1ContentType(implicit codec: Codec) = withCharset(LwmMimeType.reportCardV1Json)
  def reportCardEntryV1ContentType(implicit codec: Codec) = withCharset(LwmMimeType.reportCardEntryV1Json)
  def reportCardEntryTypeV1ContentType(implicit codec: Codec) = withCharset(LwmMimeType.reportCardEntryTypeV1Json)
  def assignmentPlanV1ContentType(implicit codec: Codec) = withCharset(LwmMimeType.assignmentPlanV1Json)
  def annotationV1ContentType(implicit codec: Codec) = withCharset(LwmMimeType.annotationV1Json)
  def reportCardEvaluationV1ContentType(implicit codec: Codec) = withCharset(LwmMimeType.reportCardEvaluationV1Json)
}

object LwmAccepts {
  val LoginV1Accept = Accepting(LwmMimeType.loginV1Json)
  val UserV1Accept = Accepting(LwmMimeType.userV1Json)
  val CourseV1Accept = Accepting(LwmMimeType.courseV1Json)
  val DegreeV1Accept = Accepting(LwmMimeType.degreeV1Json)
  val GroupV1Accept = Accepting(LwmMimeType.groupV1Json)
  val LabworkV1Accept = Accepting(LwmMimeType.labworkV1Json)
  val RoomV1Accept = Accepting(LwmMimeType.roomV1Json)
  val SemesterV1Accept = Accepting(LwmMimeType.semesterV1Json)
  val RefRoleV1Accept = Accepting(LwmMimeType.refRoleV1Json)
  val AuthorityV1Accept = Accepting(LwmMimeType.authorityV1Json)
  val RoleV1Accept = Accepting(LwmMimeType.roleV1Json)
  val PermissionV1Accept = Accepting(LwmMimeType.permissionV1Json)
  val EntryTypeV1Accept = Accepting(LwmMimeType.entryTypeV1Json)
  val LabworkApplicationV1Accept = Accepting(LwmMimeType.labworkApplicationV1Json)
  val ScheduleV1Accept = Accepting(LwmMimeType.scheduleV1Json)
  val ScheduleEntryV1Accept = Accepting(LwmMimeType.scheduleEntryV1Json)
  val TimetableV1Accept = Accepting(LwmMimeType.timetableV1Json)
  val BlacklistV1Accept = Accepting(LwmMimeType.blacklistV1Json)
  val ReportCardV1Accept = Accepting(LwmMimeType.reportCardV1Json)
  val ReportCardEntryV1Accept = Accepting(LwmMimeType.reportCardEntryV1Json)
  val ReportCardEntryTypeV1Accept = Accepting(LwmMimeType.reportCardEntryTypeV1Json)
  val AssignmentPlanV1Accept = Accepting(LwmMimeType.assignmentPlanV1Json)
  val AnnotationV1Accept = Accepting(LwmMimeType.annotationV1Json)
  val ReportCardEvaluationV1Accept = Accepting(LwmMimeType.reportCardEvaluationV1Json)
}