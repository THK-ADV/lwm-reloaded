package controllers

import java.io.File
import java.util.UUID

import com.google.inject.Inject
import controllers.helper.{JsonParser, ResultOps}
import database.helper.DatabaseMigrator
import javax.inject.Singleton
import models._
import org.apache.commons.io.FileUtils
import org.joda.time.{DateTime, LocalDate, LocalTime}
import play.api.libs.json._
import play.api.mvc.{AbstractController, AnyContent, ControllerComponents, Request}

import scala.concurrent.{ExecutionContext, Future}
import scala.io.Codec
import scala.util.Try

@Singleton
final class MigrationController @Inject()(
  cc: ControllerComponents,
  val migrator: DatabaseMigrator,
  implicit val ctx: ExecutionContext
) extends AbstractController(cc)
  with JsonParser
  with ResultOps {

  import utils.Ops.TryOps
  import utils.date.DateTimeJsonFormatter.{readLocalDate, readLocalDateTime}

  implicit def readLocaltime: Reads[LocalTime] = new Reads[LocalTime] {
    override def reads(json: JsValue) = JsSuccess(LocalTime.parse(json.as[JsString].value.dropRight(4)))
  }

  private case class FilePath(path: String)

  def dropDatabase = Action.async { _ =>
    migrator.dropDatabase.jsonResult(_ => ok())
  }

  def createDatabase = Action.async { _ =>
    migrator.createDatabase.jsonResult(_ => ok())
  }

  def migrateDegrees = Action.async { implicit request =>
    import Degree.writes
    withParser(parse(Json.reads[Degree]), migrator.migrateDegrees)
  }

  def migrateRoles = Action.async { implicit request =>
    import Role.writes
    withParser(parse(Json.reads[Role]), migrator.migrateRoles)
  }

  def migrateStudents = Action.async { implicit request =>
    import User.writes
    withParser(parse(Json.reads[Student]), migrator.migrateStudents)
  }

  def migrateEmployees = Action.async { implicit request =>
    import User.writes
    withParser(parse(Json.reads[Employee]), migrator.migrateEmployees)
  }

  def migrateCourses = Action.async { implicit request =>
    import Course.writes
    withParser(parse(Json.reads[Course]), migrator.migrateCourses)
  }

  def migrateSemesters = Action.async { implicit request =>
    import Semester.writes
    withParser(parse(Json.reads[Semester]), migrator.migrateSemesters)
  }

  def migrateAuthorities = Action.async { implicit request =>
    import Authority.writes
    withParser(parse(Json.reads[Authority]), migrator.migrateAuthorities)
  }

  def migrateLabworks = Action.async { implicit request =>
    import Labwork.writes
    withParser(parse(Json.reads[Labwork]), migrator.migrateLabworks)
  }

  def migrateLabworkApplications = Action.async { implicit request =>
    import LabworkApplication.writes
    withParser(parseLabworkApplication, migrator.migrateLabworkApplications)
  }

  def migrateRooms = Action.async { implicit request =>
    import Room.writes
    withParser(parseRoom, migrator.migrateRooms)
  }

  def migrateAssignmentPlans = Action.async { implicit request =>
    import AssignmentEntry.writes
    withParser(parseAssignmentPlan, migrator.migrateAssignmentPlans)
  }

//  def migrateBlacklists = Action.async { implicit request =>
//    withParser(parseBlacklist, migrator.migrateGlobalBlacklists)
//  }

  def migrateTimetables = Action.async { implicit request =>
    import Timetable.writes
    withParser(parseTimetable, migrator.migrateTimetables)
  }

  def migrateReportCardEntries = Action.async { implicit request =>
    import ReportCardEntry.writes
    withParser(parseReportCardEntry, migrator.migrateReportCardEntries)
  }

  def migrateReportCardEvaluations = Action.async { implicit request =>
    import ReportCardEvaluation.writes
    withParser(parse(Json.reads[ReportCardEvaluation]), migrator.migrateReportCardEvaluations)
  }

  def migrateGroups = Action.async { implicit request =>
    import Group.writes
    withParser(parse(Json.reads[Group]), migrator.migrateGroups)
  }

  def migrateScheduleEntries = Action.async { implicit request =>
    import ScheduleEntry.writes
    withParser(parse(Json.reads[ScheduleEntry]), migrator.migrateScheduleEntries)
  }

  private def withParser[A, B](
    parse: JsValue => A,
    migrate: List[A] => Future[Seq[B]]
  )(implicit request: Request[AnyContent], writes: Writes[B]) = {
    (for {
      parsed <- parseAndProcessJsonFile(request, parse)
      migrated <- migrate(parsed)
    } yield migrated).created
  }

  private def parseAndProcessJsonFile[A](request: Request[AnyContent], f: JsValue => A): Future[List[A]] =
    parseJsonFile(request).flatMap(processFile(_, f))

  private def processFile[A](file: File, f: JsValue => A): Future[List[A]] =
    Try(FileUtils.readFileToString(file, Codec.UTF8.name))
      .map(parseJsonArray(_, f))
      .toFuture

  private def parseJsonFile(request: Request[AnyContent]): Future[File] = {
    implicit def reads: Reads[FilePath] = Json.reads[FilePath]

    parseJson(request)(reads).map(p => new File(p.path)).toFuture
  }

  private def parseJsonArray[A](s: String, f: JsValue => A): List[A] =
    Json.parse(s).as[JsArray].value.map(f).toList

  private def parse[A](reads: Reads[A])(v: JsValue): A = reads.reads(v).get

  private def parseLabworkApplication(v: JsValue) = LabworkApplication(
    uuid(v.\("labwork")),
    uuid(v.\("applicant")),
    array(v.\("friends"), uuid),
    dateTime(v.\("timestamp")),
    uuid(v.\("id"))
  )

  private def parseRoom(v: JsValue) = Room(
    string(v.\("label")),
    string(v.\("description")),
    -1,
    uuid(v.\("id"))
  )

  private def parseAssignmentPlan(v: JsValue): (UUID, List[(Int, String, Set[String], Int)]) = (
    uuid(v.\("labwork")),
    array(v.\("entries"), e => {
      (
        int(e.\("index")),
        string(e.\("label")),
        array(e.\("types"), t => string(t.\("entryType"))),
        int(e.\("duration"))
      )
    }).toList
  )

  private def parseBlacklist(v: JsValue): (String, List[DateTime]) = (
    string(v.\("label")),
    array(v.\("dates"), dateTime).toList
  )

  private def parseTimetable(v: JsValue): (UUID, Set[(Set[UUID], UUID, Int, LocalTime, LocalTime)], LocalDate, UUID) = (
    uuid(v.\("labwork")),
    array(v.\("entries"), e => (
      array(e.\("supervisor"), uuid),
      uuid(e.\("room")),
      int(e.\("dayIndex")),
      localTime(e.\("start")),
      localTime(e.\("end"))
    )),
    localDate(v.\("start")),
    uuid(v.\("id"))
  )

  private def parseReportCardEntry(v: JsValue): (UUID, UUID, String, LocalDate, LocalTime, LocalTime, UUID, Set[(String, Boolean, Int, UUID)], Option[(LocalDate, LocalTime, LocalTime, UUID)], UUID) = (
    uuid(v.\("student")),
    uuid(v.\("labwork")),
    string(v.\("label")),
    localDate(v.\("date")),
    localTime(v.\("start")),
    localTime(v.\("end")),
    uuid(v.\("room")),
    array(v.\("entryTypes"), t => (
      string(t.\("entryType")),
      bool(t.\("bool")),
      int(t.\("int")),
      uuid(t.\("id")),
    )),
    v.\("rescheduled").toOption.map(r => (
      localDate(r.\("date")),
      localTime(r.\("start")),
      localTime(r.\("end")),
      uuid(r.\("room"))
    )),
    uuid(v.\("id"))
  )

  private def string[A <: JsReadable](r: A): String = r.validate[String].get

  private def int[A <: JsReadable](r: A): Int = r.validate[Int].get

  private def bool[A <: JsReadable](r: A): Boolean = r.validate[Boolean].get

  private def uuid[A <: JsReadable](r: A): UUID = UUID.fromString(string(r))

  private def dateTime[A <: JsReadable](r: A): DateTime = r.validate[DateTime].get

  private def localTime[A <: JsReadable](r: A): LocalTime = r.validate[LocalTime].get

  private def localDate[A <: JsReadable](r: A): LocalDate = r.validate[LocalDate].get

  private def array[A <: JsReadable, B](r: A, f: JsValue => B): Set[B] = r.as[JsArray].value.map(f).toSet
}
