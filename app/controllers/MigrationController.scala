package controllers

import java.util.UUID

import akka.actor.ActorSystem
import com.google.inject.Inject
import controllers.helper.{RawJsonParser, ResultOps}
import database.{GroupDb, LabworkApplicationDb, ReportCardEntryDb, ReportCardEntryTypeDb, ReportCardEvaluationDb, ReportCardRescheduledDb, UserDb}
import database.helper.{DatabaseMigrator, LdapUserStatus}
import javax.inject.Singleton
import models._
import org.joda.time.{DateTime, LocalDate, LocalTime}
import play.api.libs.json._
import play.api.mvc._
import service.Webservice
import service.actor.ReportCardMigrationAktor
import service.actor.ReportCardMigrationAktor.MigrationRequest

import scala.concurrent.{ExecutionContext, Future}

@Singleton
final class MigrationController @Inject()(
  cc: ControllerComponents,
  val migrator: DatabaseMigrator,
  val ws: Webservice,
  val system: ActorSystem,
  implicit val ctx: ExecutionContext
) extends AbstractController(cc)
  with RawJsonParser
  with ResultOps {

  import slick.jdbc.PostgresProfile.api._
  import utils.Ops.TryOps
  import utils.date.DateTimeOps._
  import utils.date.DateTimeJsonFormatter.{readLocalDate, readLocalDateTime}

  implicit def readLocaltime: Reads[LocalTime] = new Reads[LocalTime] {
    override def reads(json: JsValue) = JsSuccess(LocalTime.parse(json.as[JsString].value.dropRight(4)))
  }

  val ref = ReportCardMigrationAktor.props(ctx)
  val actor = system.actorOf(ref)
  val rootUrl = "http://lwivs10.gm.fh-koeln.de:9000"

  private case class FilePath(path: String)

  def dropDatabase = Action.async { _ =>
    migrator.dropDatabase.jsonResult(_ => ok())
  }

  def createDatabase = Action.async { _ =>
    migrator.createDatabase.jsonResult(_ => ok())
  }

  def actorAction[A](f: Request[AnyContent] => MigrationRequest[A]) = Action { request =>
    actor ! f(request)
    ok("status" -> "workin")
  }

  private def migrationResult[A, B](parse: String => A, migrate: A => Future[Seq[B]], mapUrl: String => String)(implicit request: Request[AnyContent]): Future[Seq[B]] = {
    for {
      json <- unwrap(request).toFuture
      (url, cookie) = (string(json.\("url")), string(json.\("cookie")))
      parsed <- ws.getWithCookie(mapUrl(url), cookie)(parse)
      resp <- migrate(parsed)
    } yield resp
  }

  def rightsManagerId = UUID.fromString("d9f69433-ecf8-4127-8e49-b2b7222bb3ef")

  def substituteCourse(id: UUID)(url: String): String =
    url.replace(":c", id.toString)

  def substituteCourseAndLabwork(c: UUID, l: UUID)(url: String): String =
    url.replace(":c", c.toString).replace(":l", l.toString)

  def migrateDegrees = actorAction { implicit request =>
    new MigrationRequest[Degree] {
      override def name = "degree"

      override def f = () => {
        migrationResult(unchunk(_, parse(Json.reads[Degree])), migrator.migrateDegrees, identity)
      }
    }
  }

  def migrateRoles = actorAction { implicit request =>
    new MigrationRequest[Role] {
      override def name = "role"

      override def f = () => {
        migrationResult(unchunk(_, parse(Json.reads[Role])).filterNot(_.id == rightsManagerId), migrator.migrateRoles, identity)
      }
    }
  }

  def migrateStudents = actorAction { implicit request =>
    def markDuplicates(xs: List[Student]) = xs
      .groupBy(_.systemId)
      .flatMap {
        case (_, students) =>
          if (students.size > 1)
            students.zipWithIndex.map(s => s._1.copy(systemId = s"${s._2}_${s._1.systemId}"))
          else
            students
      }
      .toList

    new MigrationRequest[User] {
      override def name = "student"

      override def f = () => {
        migrationResult(s => markDuplicates(unchunk(s, parse(Json.reads[Student]))), migrator.migrateStudents, identity)
      }
    }
  }

  def migrateEmployees = actorAction { implicit request =>
    new MigrationRequest[User] {
      override def name = "employee"

      override def f = () => {
        migrationResult(unchunk(_, parse(Json.reads[Employee])), migrator.migrateEmployees, identity)
      }
    }
  }

  def migrateCourses = actorAction { implicit request =>
    new MigrationRequest[Course] {
      override def name = "courses"

      override def f = () => {
        migrationResult(unchunk(_, parse(Json.reads[Course])), migrator.migrateCourses, identity)
      }
    }
  }

  def migrateSemesters = actorAction { implicit request =>
    new MigrationRequest[Semester] {
      override def name = "semester"

      override def f = () => {
        migrationResult(unchunk(_, parse(Json.reads[Semester])), migrator.migrateSemesters, identity)
      }
    }
  }

  def migrateAuthorities = actorAction { implicit request =>
    new MigrationRequest[Authority] {
      def filterNotRightsManager(xs: List[Authority]) = xs.filterNot(_.role == rightsManagerId)

      override def name = "authorities"

      override def f = () => {
        migrationResult(
          s => filterNotRightsManager(unchunk(s, parse(Json.reads[Authority]))),
          migrator.migrateAuthorities,
          identity
        )
      }
    }
  }

  def migrateLabworks = actorAction { implicit request =>
    new MigrationRequest[Labwork] {
      override def name = "labwork"

      override def f = () => {
        for {
          courses <- migrator.courseDao.get(atomic = false)
          labworks <- Future.sequence(
            courses
              .map(c => migrationResult(unchunk(_, parse(Json.reads[Labwork])), migrator.migrateLabworks, substituteCourse(c.id)))
          )
        } yield labworks.flatten
      }
    }
  }

  def migrateLabworkApplications = actorAction { implicit request =>
    new MigrationRequest[LabworkApplication] {
      def removeSelfContainedFriends(xs: List[LabworkApplication]) = xs
        .map(app => if (app.friends.contains(app.applicant)) app.copy(friends = app.friends - app.applicant) else app)

      def removeDuplicates(xs: List[LabworkApplication]) = xs
        .groupBy(a => a.applicant.toString + a.labwork.toString)
        .map(_._2.head)
        .toList

      override def name = "labworkApplications"

      override def f = () => {
        migrationResult(
          s => (removeSelfContainedFriends _ andThen removeDuplicates) (unchunk(s, parseLabworkApplication)),
          migrator.migrateLabworkApplications,
          identity
        )
      }
    }
  }

  def migrateRooms = actorAction { implicit request =>
    new MigrationRequest[Room] {
      override def name = "rooms"

      override def f = () => {
        migrationResult(unchunk(_, parseRoom), migrator.migrateRooms, identity)
      }
    }
  }

  def migrateAssignmentEntries = actorAction { implicit request =>
    new MigrationRequest[AssignmentEntry] {
      override def name = "assignmentEntry"

      override def f = () => {
        for {
          courses <- migrator.courseDao.get(atomic = false)
          entries <- Future.sequence(
            courses
              .map(c => migrationResult(unchunk(_, parseAssignmentEntries), migrator.migrateAssignmentEntries, substituteCourse(c.id)))
          )
        } yield entries.flatten
      }
    }
  }

  def migrateTimetables = actorAction { implicit request =>
    new MigrationRequest[Timetable] {
      override def name = "timetable"

      override def f = () => {
        for {
          courses <- migrator.courseDao.get(atomic = false)
          entries <- Future.sequence(
            courses
              .map(c => migrationResult(unchunk(_, parseTimetable), migrator.migrateTimetables, substituteCourse(c.id)))
          )
        } yield entries.flatten
      }
    }
  }

  def migrateReportCardEntries = actorAction { implicit request =>
    def calculateAssignmentIndex(xs: List[ReportCardEntry]) = {
      import utils.date.DateTimeOps.localDateTimeOrd
      xs
        .groupBy(e => e.student.toString + e.labwork.toString)
        .flatMap {
          case (_, entries) => entries
            .sortBy(e => e.date.toLocalDateTime(e.start))
            .zipWithIndex
            .map(t => t._1.copy(assignmentIndex = t._2))
        }
        .toList
    }

    new MigrationRequest[ReportCardEntry] {
      override def name = "reportCardEntry"

      override def f = () => {
        for {
          courses <- migrator.courseDao.get(atomic = false)
          entries <- Future.sequence(
            courses
              .map(c => migrationResult(s => calculateAssignmentIndex(unchunk(s, parseReportCardEntry)), migrator.migrateReportCardEntries, substituteCourse(c.id)))
          )
        } yield entries.flatten
      }
    }
  }

  def migrateReportCardEvaluations = actorAction { implicit request =>
    new MigrationRequest[ReportCardEvaluation] {
      override def name = "reportCardEvaluation"

      override def f = () => {
        for {
          labworks <- migrator.labworkDao.get(atomic = false)
          entries <- Future.sequence(
            labworks
              .map(l => {
                val labwork = l.asInstanceOf[Labwork]
                migrationResult(unchunk(_, parseReportCardEvaluation), migrator.migrateReportCardEvaluations, substituteCourseAndLabwork(labwork.course, labwork.id))
              })
          )
        } yield entries.flatten
      }
    }
  }

  def migrateGroups = actorAction { implicit request =>
    new MigrationRequest[Group] {
      override def name = "groups"

      override def f = () => {
        for {
          labworks <- migrator.labworkDao.get(atomic = false)
          entries <- Future.sequence(
            labworks
              .map(l => {
                val labwork = l.asInstanceOf[Labwork]
                migrationResult(unchunk(_, parse(Json.reads[Group])), migrator.migrateGroups, substituteCourseAndLabwork(labwork.course, labwork.id))
              })
          )
        } yield entries.flatten
      }
    }
  }

  def migrateScheduleEntries = actorAction { implicit request =>
    new MigrationRequest[ScheduleEntry] {
      override def name = "scheduleEntries"

      override def f = () => {
        for {
          labworks <- migrator.labworkDao.get(atomic = false)
          entries <- Future.sequence(
            labworks
              .map(l => {
                val labwork = l.asInstanceOf[Labwork]
                migrationResult(unchunk(_, parse(Json.reads[ScheduleEntry])), migrator.migrateScheduleEntries, substituteCourseAndLabwork(labwork.course, labwork.id))
              })
          )
        } yield entries.flatten
      }
    }
  }

  def resolveDuplicateStudents = Action.async { implicit request =>
    import User.writes

    def duplicates() = migrator.userDao
      .get(atomic = false)
      .map(_.filter(_.systemId.contains("_")))
      .map(_.groupBy(_.systemId.split("_").last))

    def deleteApps(drop: List[UUID]) = {
      for {
        allApps <- migrator.lappDao.get(atomic = false).map(_.map(_.asInstanceOf[LabworkApplication]))
        deleteApps = allApps.filter(a => drop.contains(a.applicant))
        toUpdate = allApps
          .filterNot(a => deleteApps.exists(_.id == a.id))
          .filter(a => a.friends.exists(drop.contains))
          .map(a => a.copy(friends = a.friends.filterNot(drop.contains)))
        _ <- migrator.lappDao.invalidateMany(deleteApps.map(_.id).toList)
        _ <- migrator.lappDao.updateMany(toUpdate.map(toLappDb).toList)
      } yield (deleteApps, toUpdate)
    }

    def deleteGroups(drop: List[UUID]) = {
      for {
        groups <- migrator.groupDao.filter(_.containsAny(drop), atomic = false)
        toUpdate = groups
          .map(_.asInstanceOf[Group])
          .map(g => g.copy(members = g.members.filterNot(drop.contains)))
        _ <- migrator.groupDao.updateMany(toUpdate.map(toGroupDb).toList)
      } yield toUpdate
    }

    def deleteAuths(drop: List[UUID]) = {
      for {
        auths <- migrator.authorityDao.filter(a => a.user.inSet(drop), atomic = false)
        _ <- migrator.authorityDao.invalidateMany(auths.map(_.id).toList)
      } yield auths
    }

    def deleteCards(pairs: Map[UUID, Seq[UUID]]) = {
      val anyDups = pairs.flatMap(_._2)

      def findOrigin(dup: UUID): UUID = pairs.find(_._2.contains(dup)).get._1

      for {
        dupCards <- migrator.reportCardEntryDao.filter(c => c.user.inSet(anyDups), atomic = false)
        replaced = dupCards
          .map(_.asInstanceOf[ReportCardEntry])
          .map(e => e.copy(student = findOrigin(e.student)))
        _ <- migrator.reportCardEntryDao.updateMany(replaced.map(toReportCardEntryDb).toList)
      } yield replaced
    }

    def deleteEvals(pairs: Map[UUID, Seq[UUID]]) = {
      val anyDups = pairs.flatMap(_._2)

      def findOrigin(dup: UUID): UUID = pairs.find(_._2.contains(dup)).get._1

      for {
        dupEvals <- migrator.reportCardEvaluationDao.filter(c => c.user.inSet(anyDups), atomic = false)
        replaced = dupEvals
          .map(_.asInstanceOf[ReportCardEvaluation])
          .map(e => e.copy(student = findOrigin(e.student)))
        _ <- migrator.reportCardEvaluationDao.updateMany(replaced.map(toReportCardEvalDb).toList)
      } yield replaced
    }

    def deleteStudents(drop: List[UUID]) = {
      for {
        delete <- migrator.userDao.filter(u => u.id.inSet(drop), atomic = false)
        _ <- migrator.userDao.invalidateMany(delete.map(_.id).toList)
      } yield delete
    }

    def unmarkStudents(keep: List[UUID]) = {
      for {
        students <- migrator.userDao.filter(u => u.id.inSet(keep), atomic = false)
        unmarked = students
          .map(_.asInstanceOf[Student])
          .map(s => s.copy(systemId = s.systemId.split("_").last))
        _ <- migrator.userDao.updateMany(unmarked.map(toStudentDb).toList)
      } yield unmarked
    }

    (for {
      dups <- duplicates()
      pairs = dups.map(t => (t._2.head.id, t._2.tail.map(_.id)))
      drop = pairs.flatMap(_._2).toList
      _ <- deleteApps(drop)
      _ <- deleteGroups(drop)
      _ <- deleteAuths(drop)
      _ <- deleteCards(pairs)
      _ <- deleteEvals(pairs)
      _ <- deleteStudents(drop)
      r <- unmarkStudents(pairs.keys.toList)
    } yield r).jsonResult
  }

  private def toLappDb(a: LabworkApplication) = {
    LabworkApplicationDb(a.labwork, a.applicant, a.friends, a.lastModified.timestamp, id = a.id)
  }

  private def toGroupDb(g: Group) = GroupDb(
    g.label, g.labwork, g.members, id = g.id
  )

  private def toReportCardEntryDb(e: ReportCardEntry) = {
    ReportCardEntryDb(
      e.student,
      e.labwork,
      e.label,
      e.date.sqlDate,
      e.start.sqlTime,
      e.end.sqlTime,
      e.room,
      e.entryTypes.map(t => ReportCardEntryTypeDb(Some(e.id), None, t.entryType, t.bool, t.int, id = t.id)),
      e.assignmentIndex,
      e.rescheduled.map(r => ReportCardRescheduledDb(e.id, r.date.sqlDate, r.start.sqlTime, r.end.sqlTime, r.room, r.reason, id = r.id)),
      None,
      id = e.id
    )
  }

  private def toReportCardEvalDb(e: ReportCardEvaluation) =
    ReportCardEvaluationDb(e.student, e.labwork, e.label, e.bool, e.int, e.lastModified.timestamp, id = e.id)

  private def toStudentDb(s: Student) =
    UserDb(s.systemId, s.lastname, s.firstname, s.email, LdapUserStatus.StudentStatus, Some(s.registrationId), Some(s.enrollment), id = s.id)

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

  private def parseAssignmentEntries(v: JsValue): (UUID, List[(Int, String, Set[String], Int)]) = (
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

  private def parseReportCardEvaluation(v: JsValue): ReportCardEvaluation = ReportCardEvaluation(
    uuid(v.\("student")),
    uuid(v.\("labwork")),
    string(v.\("label")),
    bool(v.\("bool")),
    int(v.\("int")),
    dateTime(v.\("timestamp")),
    uuid(v.\("id")),
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

  private def parseReportCardEntry(v: JsValue): ReportCardEntry = {
    ReportCardEntry(
      uuid(v.\("student")),
      uuid(v.\("labwork")),
      string(v.\("label")),
      localDate(v.\("date")),
      localTime(v.\("start")),
      localTime(v.\("end")),
      uuid(v.\("room")),
      array(v.\("entryTypes"), t => ReportCardEntryType(
        string(t.\("entryType")),
        Some(bool(t.\("bool"))),
        int(t.\("int")),
        uuid(t.\("id"))
      )),
      -1,
      v.\("rescheduled").toOption.map(r => ReportCardRescheduled(
        localDate(r.\("date")),
        localTime(r.\("start")),
        localTime(r.\("end")),
        uuid(r.\("room"))
      )),
      None,
      uuid(v.\("id"))
    )
  }

  private def unchunk[A](raw: String, f: JsValue => A): List[A] = {
    val json = raw.replace("}{", "},{")
    val jsonArray = s"[$json]"
    Json.parse(jsonArray).as[JsArray].value.map(f).toList
  }
}
