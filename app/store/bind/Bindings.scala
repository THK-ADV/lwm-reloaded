package store.bind

import java.util.UUID
import models._
import models.applications.LabworkApplication
import models.schedule._
import models.security._
import models.semester.{Blacklist, Semester}
import models.users.{Employee, Student, User}
import org.joda.time.{DateTime, LocalDate, LocalTime}
import org.joda.time.format.ISODateTimeFormat
import org.w3.banana._
import org.w3.banana.binder.{ClassUrisFor, PGBinder, RecordBinder}
import store.Namespace
import store.Prefixes.LWMPrefix

import scala.language.{implicitConversions, postfixOps}
import scala.util.Try

class Bindings[Rdf <: RDF](implicit baseNs: Namespace, ops: RDFOps[Rdf], recordBinder: RecordBinder[Rdf]) {

  import ops._
  import recordBinder._

  val lwm = LWMPrefix[Rdf]
  val rdf = RDFPrefix[Rdf]
  val xsd = XSDPrefix[Rdf]

  implicit val uuidBinder = new PGBinder[Rdf, UUID] {
    override def toPG(t: UUID): PointedGraph[Rdf] = {
      PointedGraph(ops.makeLiteral(t.toString, xsd.string))
    }

    override def fromPG(pointed: PointedGraph[Rdf]): Try[UUID] = {
      pointed.pointer.as[String].map { value =>
        UUID.fromString(value)
      }
    }
  }

  implicit def uuidRefBinder(splitter: URLSplit[UUID]): PGBinder[Rdf, UUID] = new PGBinder[Rdf, UUID] {
    override def toPG(t: UUID): PointedGraph[Rdf] = {
      PointedGraph(ops.makeUri(splitter.to(t)))
  }

    override def fromPG(pointed: PointedGraph[Rdf]): Try[UUID] = {
      pointed.pointer.as[Rdf#URI].map { value =>
        splitter.from(value.toString)
      }
    }
  }

  implicit val jodaDateTimeBinder = new PGBinder[Rdf, DateTime] {
    val formatter = ISODateTimeFormat.dateTime()

    override def toPG(t: DateTime): PointedGraph[Rdf] = {
      PointedGraph(ops.makeLiteral(formatter.print(t), xsd.dateTime))
    }

    override def fromPG(pointed: PointedGraph[Rdf]): Try[DateTime] = {
      pointed.pointer.as[Rdf#Literal].map(ops.fromLiteral).map {
        case (stringVal, uri, optLang) => DateTime.parse(stringVal)
      }
    }
  }

  implicit val jodaLocalDateBinder = new PGBinder[Rdf, LocalDate] {
    // LocalDate.toString formats to ISO8601 (yyyy-MM-dd)
    override def toPG(t: LocalDate): PointedGraph[Rdf] = {
      PointedGraph(ops.makeLiteral(t.toString(), lwm.localDate))
    }

    override def fromPG(pointed: PointedGraph[Rdf]): Try[LocalDate] = {
      pointed.pointer.as[Rdf#Literal].map(ops.fromLiteral).map {
        case (stringVal, uri, optLang) => LocalDate.parse(stringVal)
      }
    }
  }

  implicit val jodaLocalTimeBinder = new PGBinder[Rdf, LocalTime] {
    // LocalTime.toString formats to ISO8601 (yyyy-MM-dd)
    override def toPG(t: LocalTime): PointedGraph[Rdf] = {
      PointedGraph(ops.makeLiteral(t.toString(), lwm.localTime))
    }

    override def fromPG(pointed: PointedGraph[Rdf]): Try[LocalTime] = {
      pointed.pointer.as[Rdf#Literal].map(ops.fromLiteral).map {
        case (stringVal, uri, optLang) => LocalTime.parse(stringVal)
      }
    }
  }

  implicit val permissionBinder = new PGBinder[Rdf, Permission] {
    override def toPG(t: Permission): PointedGraph[Rdf] = {
      PointedGraph(ops.makeLiteral(t.value, xsd.string))
    }

    override def fromPG(pointed: PointedGraph[Rdf]): Try[Permission] = {
      pointed.pointer.as[String].map(Permission.apply)
    }
  }

  val id = property[UUID](lwm.id)

  object UserBinding {
    val clazz = lwm.User
    val classUri: ClassUrisFor[Rdf, User] = classUrisFor[User](clazz)

    implicit val userBinder: PGBinder[Rdf, User] = new PGBinder[Rdf, User] {
      import StudentBinding._
      import EmployeeBinding._

      override def fromPG(pointed: PointedGraph[Rdf]): Try[User] = {
        studentBinder.fromPG(pointed) orElse employeeBinder.fromPG(pointed)
      }

      override def toPG(t: User): PointedGraph[Rdf] = t match {
        case s: Student => s.toPG(studentBinder)
        case e: Employee => e.toPG(employeeBinder)
      }
    }
  }

  object LabworkApplicationBinding {
    implicit val clazz = lwm.LabworkApplication
    implicit val classUri = classUrisFor[LabworkApplication](clazz)

    private val labwork = property[UUID](lwm.labwork)(uuidRefBinder(Labwork.splitter))
    private val applicant = property[UUID](lwm.applicant)(uuidRefBinder(User.splitter))
    private val timestamp = property[DateTime](lwm.timestamp)
    private val friends = set[UUID](lwm.friends)(uuidRefBinder(User.splitter))

    implicit val labworkApplicationBinder: PGBinder[Rdf, LabworkApplication] = pgbWithId[LabworkApplication](application => makeUri(LabworkApplication.generateUri(application)))(labwork, applicant, friends, timestamp, id)(LabworkApplication.apply, LabworkApplication.unapply) withClasses classUri
  }

  object StudentBinding {
    implicit val clazz = lwm.User
    implicit val classUri: ClassUrisFor[Rdf, Student] = classUrisFor[Student](clazz)

    private val lastname = property[String](lwm.lastname)
    private val firstname = property[String](lwm.firstname)
    private val registrationId = property[String](lwm.registrationId)
    private val systemId = property[String](lwm.systemId)
    private val enrollment = property[UUID](lwm.enrollment)(uuidRefBinder(Degree.splitter))
    private val email = property[String](lwm.email)

    implicit val studentBinder: PGBinder[Rdf, Student] = pgbWithId[Student](student => makeUri(User.generateUri(student)))(systemId, lastname, firstname, email, registrationId, enrollment, id)(Student.apply, Student.unapply) withClasses classUri
  }

  object EmployeeBinding {
    implicit val clazz = lwm.User
    implicit val classUri: ClassUrisFor[Rdf, Employee] = classUrisFor[Employee](clazz)

    private val lastname = property[String](lwm.lastname)
    private val firstname = property[String](lwm.firstname)
    private val systemId = property[String](lwm.systemId)
    private val email = property[String](lwm.email)
    private val status = property[String](lwm.status)

    implicit val employeeBinder: PGBinder[Rdf, Employee] = pgbWithId[Employee](employee => makeUri(User.generateUri(employee)))(systemId, lastname, firstname, email, status, id)(Employee.apply, Employee.unapply) withClasses classUri
  }

  object RoleBinding {
    implicit val clazz = lwm.Role
    implicit val classUri = classUrisFor[Role](clazz)

    private val label = property[String](lwm.label)
    private val permissions = set[Permission](lwm.permissions)

    implicit val roleBinder: PGBinder[Rdf, Role] = pgbWithId[Role](role => makeUri(Role.generateUri(role)))(label, permissions, id)(Role.apply, Role.unapply) withClasses classUri
  }

  object RefRoleBinding {
    import RoleBinding.roleBinder

    implicit val clazz = lwm.RefRole
    implicit val classUri = classUrisFor[RefRole](clazz)

    private val course = optional[UUID](lwm.course)(uuidRefBinder(Course.splitter))
    private val role = property[UUID](lwm.role)(uuidRefBinder(Role.splitter))

    implicit val refRoleBinder: PGBinder[Rdf, RefRole] = pgbWithId[RefRole](refRole => makeUri(RefRole.generateUri(refRole)))(course, role, id)(RefRole.apply, RefRole.unapply) withClasses classUri
  }

  object AuthorityBinding {
    implicit val clazz = lwm.Authority
    implicit val classUri = classUrisFor[Authority](clazz)

    private val privileged = property[UUID](lwm.privileged)(uuidRefBinder(User.splitter))
    private val refroles = set[UUID](lwm.refroles)(uuidRefBinder(RefRole.splitter))

    implicit val authorityBinder: PGBinder[Rdf, Authority] = pgbWithId[Authority](auth => makeUri(Authority.generateUri(auth)))(privileged, refroles, id)(Authority.apply, Authority.unapply) withClasses classUri
  }

  object AssignmentEntryTypeBinding {
    implicit val clazz = lwm.AssignmentEntryType
    implicit val classUri = classUrisFor[AssignmentEntryType](clazz)

    private val entryType = property[String](lwm.entryType)
    private val bool = property[Boolean](lwm.bool)
    private val int = property[Int](lwm.int)

    implicit val assignmentEntryTypeBinder: PGBinder[Rdf, AssignmentEntryType] = pgb[AssignmentEntryType](entryType, bool, int)(AssignmentEntryType.apply, AssignmentEntryType.unapply) withClasses classUri
  }

  object AssignmentEntryBinding {
    implicit val clazz = lwm.AssignmentEntry
    implicit val classUri = classUrisFor[AssignmentEntry](clazz)

    private val index = property[Int](lwm.index)
    private val label = property[String](lwm.label)
    private val duration = property[Int](lwm.duration)
    private val types = set[AssignmentEntryType](lwm.types)(AssignmentEntryTypeBinding.assignmentEntryTypeBinder)

    implicit val assignmentEntryBinder: PGBinder[Rdf, AssignmentEntry] = pgb[AssignmentEntry](index, label, types, duration)(AssignmentEntry.apply, AssignmentEntry.unapply) withClasses classUri
  }

  object AssignmentPlanBinding {
    implicit val clazz = lwm.AssignmentPlan
    implicit val classUri = classUrisFor[AssignmentPlan](clazz)

    private val labwork = property[UUID](lwm.labwork)(uuidRefBinder(Labwork.splitter))
    private val attendance = property[Int](lwm.attendance)
    private val mandatory = property[Int](lwm.mandatory)
    private val entries = set[AssignmentEntry](lwm.entries)(AssignmentEntryBinding.assignmentEntryBinder)

    implicit val assignmentPlanBinder: PGBinder[Rdf, AssignmentPlan] = pgbWithId[AssignmentPlan](aPlan => makeUri(AssignmentPlan.generateUri(aPlan)))(labwork, attendance, mandatory, entries, id)(AssignmentPlan.apply, AssignmentPlan.unapply) withClasses classUri
  }

  object LabworkBinding {
    implicit val clazz = lwm.Labwork
    implicit val classUri = classUrisFor[Labwork](clazz)

    val label = property[String](lwm.label)
    val description = property[String](lwm.description)
    val semester = property[UUID](lwm.semester)(uuidRefBinder(Semester.splitter))
    val course = property[UUID](lwm.course)(uuidRefBinder(Course.splitter))
    val degree = property[UUID](lwm.degree)(uuidRefBinder(Degree.splitter))
    val subscribable = property[Boolean](lwm.subscribable)

    implicit val labworkBinder: PGBinder[Rdf, Labwork] = pgbWithId[Labwork](labwork => makeUri(Labwork.generateUri(labwork)))(label, description, semester, course, degree, subscribable, id)(Labwork.apply, Labwork.unapply) withClasses classUri
  }

  object CourseBinding {
    implicit val clazz = lwm.Course
    implicit val classUri = classUrisFor[Course](clazz)

    private val label = property[String](lwm.label)
    private val description = property[String](lwm.description)
    private val abbreviation = property[String](lwm.abbreviation)
    private val lecturer = property[UUID](lwm.lecturer)(uuidRefBinder(User.splitter))
    private val semesterIndex = property[Int](lwm.semesterIndex)

    implicit val courseBinder: PGBinder[Rdf, Course] = pgbWithId[Course](course => makeUri(Course.generateUri(course)))(label, description, abbreviation, lecturer, semesterIndex, id)(Course.apply, Course.unapply) withClasses classUri
  }

  object DegreeBinding {
    implicit val clazz = lwm.Degree
    implicit val classUri = classUrisFor[Degree](clazz)

    private val label = property[String](lwm.label)
    private val abbreviation = property[String](lwm.abbreviation)

    implicit val degreeBinder: PGBinder[Rdf, Degree] = pgbWithId[Degree](degree => makeUri(Degree.generateUri(degree)))(label, abbreviation, id)(Degree.apply, Degree.unapply) withClasses classUri
  }

  object GroupBinding {
    implicit val clazz = lwm.Group
    implicit val classUri = classUrisFor[Group](clazz)

    private val label = property[String](lwm.label)
    private val labwork = property[UUID](lwm.labwork)(uuidRefBinder(Labwork.splitter))
    private val members = set[UUID](lwm.members)(uuidRefBinder(User.splitter))

    implicit val groupBinder: PGBinder[Rdf, Group] = pgbWithId[Group](group => makeUri(Group.generateUri(group)))(label, labwork, members, id)(Group.apply, Group.unapply) withClasses classUri
  }

  object RoomBinding {
    implicit val clazz = lwm.Room
    implicit val classUri = classUrisFor[Room](clazz)

    private val label = property[String](lwm.label)
    private val description = property[String](lwm.description)

    implicit val roomBinder: PGBinder[Rdf, Room] = pgbWithId[Room](room => makeUri(Room.generateUri(room)))(label, description, id)(Room.apply, Room.unapply) withClasses classUri
  }

  object SemesterBinding {
    implicit val clazz = lwm.Semester
    implicit val classUri = classUrisFor[Semester](clazz)

    private val label = property[String](lwm.label)
    private val abbreviation = property[String](lwm.abbreviation)
    private val start = property[LocalDate](lwm.start)
    private val end = property[LocalDate](lwm.end)
    private val examStart = property[LocalDate](lwm.examStart)

    implicit val semesterBinder: PGBinder[Rdf, Semester] = pgbWithId[Semester](semester => makeUri(Semester.generateUri(semester)))(label, abbreviation, start, end, examStart, id)(Semester.apply, Semester.unapply) withClasses classUri
  }

  object TimetableBinding {
    implicit val clazz = lwm.Timetable
    implicit val classUri = classUrisFor[Timetable](clazz)

    private val labwork = property[UUID](lwm.labwork)(uuidRefBinder(Labwork.splitter))
    private val entries = set[TimetableEntry](lwm.entries)(TimetableEntryBinding.timetableEntryBinder)
    private val start = property[LocalDate](lwm.start)
    private val blacklist = property[Blacklist](lwm.blacklist)(BlacklistBinding.blacklistBinder)

    implicit val timetableBinder: PGBinder[Rdf, Timetable] = pgbWithId[Timetable](timetable => makeUri(Timetable.generateUri(timetable)))(labwork, entries, start, blacklist, id)(Timetable.apply, Timetable.unapply) withClasses classUri
  }

  object TimetableEntryBinding {
    implicit val clazz = lwm.TimetableEntry
    implicit val classUri = classUrisFor[TimetableEntry](clazz)

    private val supervisor = property[UUID](lwm.supervisor)(uuidRefBinder(User.splitter))
    private val room = property[UUID](lwm.room)(uuidRefBinder(Room.splitter))
    private val degree = property[UUID](lwm.degree)(uuidRefBinder(Degree.splitter))
    private val dayIndex = property[Int](lwm.dayIndex)
    private val start = property[LocalTime](lwm.start)
    private val end = property[LocalTime](lwm.end)

    implicit val timetableEntryBinder: PGBinder[Rdf, TimetableEntry] = pgb[TimetableEntry](supervisor, room, degree, dayIndex, start, end)(TimetableEntry.apply, TimetableEntry.unapply) withClasses classUri
  }

  object ScheduleBinding {
    implicit val clazz = lwm.Schedule
    implicit val classUri = classUrisFor[Schedule](clazz)

    private val labwork = property[UUID](lwm.labwork)(uuidRefBinder(Labwork.splitter))
    private val entries = set[ScheduleEntry](lwm.entries)(ScheduleEntryBinding.scheduleEntryBinder)
    private val published = property[Boolean](lwm.published)

    implicit val scheduleBinder: PGBinder[Rdf, Schedule] = pgbWithId[Schedule](schedule => makeUri(Schedule.generateUri(schedule)))(labwork, entries, published, id)(Schedule.apply, Schedule.unapply) withClasses classUri
  }

  object ScheduleEntryBinding {
    implicit val clazz = lwm.ScheduleEntry
    implicit val classUri = classUrisFor[ScheduleEntry](clazz)

    private val start = property[LocalTime](lwm.start)
    private val end = property[LocalTime](lwm.end)
    private val date = property[LocalDate](lwm.date)
    private val room = property[UUID](lwm.room)(uuidRefBinder(Room.splitter))
    private val supervisor = property[UUID](lwm.supervisor)(uuidRefBinder(User.splitter))
    private val group = property[UUID](lwm.group)(uuidRefBinder(Group.splitter))

    implicit val scheduleEntryBinder: PGBinder[Rdf, ScheduleEntry] = pgb[ScheduleEntry](start, end, date, room, supervisor, group)(ScheduleEntry.apply, ScheduleEntry.unapply) withClasses classUri
  }

  object BlacklistBinding {
    implicit val clazz = lwm.Blacklist
    implicit val classUri = classUrisFor[Blacklist](clazz)

    private val label = property[String](lwm.label)
    private val dates = set[DateTime](lwm.dates)

    implicit val blacklistBinder: PGBinder[Rdf, Blacklist] = pgbWithId[Blacklist](blacklist => makeUri(Blacklist.generateUri(blacklist)))(label, dates, id)(Blacklist.apply, Blacklist.unapply) withClasses classUri
  }

  object ReportCardBinding {
    implicit val clazz = lwm.ReportCard
    implicit val classUri = classUrisFor[ReportCard](clazz)

    private val student = property[UUID](lwm.student)(uuidRefBinder(User.splitter))
    private val labwork = property[UUID](lwm.labwork)(uuidRefBinder(Labwork.splitter))
    private val entries = set[ReportCardEntry](lwm.entries)(ReportCardEntryBinding.reportCardEntryBinding)

    implicit val reportCardBinder: PGBinder[Rdf, ReportCard] = pgbWithId[ReportCard](reportCard => makeUri(ReportCard.generateUri(reportCard)))(student, labwork, entries, id)(ReportCard.apply, ReportCard.unapply) withClasses classUri
  }

  object ReportCardEntryBinding {
    implicit val clazz = lwm.ReportCardEntry
    implicit val classUri = classUrisFor[ReportCardEntry](clazz)

    private val index = property[Int](lwm.index)
    private val label = property[String](lwm.label)
    private val date = property[LocalDate](lwm.date)
    private val start = property[LocalTime](lwm.start)
    private val end = property[LocalTime](lwm.end)
    private val room = property[UUID](lwm.room)(uuidRefBinder(Room.splitter))
    private val rescheduled = optional[Rescheduled](lwm.rescheduled)(RescheduledBinding.rescheduledBinding)
    private val types = set[ReportCardEntryType](lwm.types)(ReportCardEntryTypeBinding.reportCardEntryTypeBinding)

    implicit val reportCardEntryBinding: PGBinder[Rdf, ReportCardEntry] = pgbWithId[ReportCardEntry](reportCardEntry => makeUri(ReportCardEntry.generateUri(reportCardEntry)))(index, label, date, start, end, room, types, rescheduled, id)(ReportCardEntry.apply, ReportCardEntry.unapply) withClasses classUri
  }

  object ReportCardEntryTypeBinding {
    implicit val clazz = lwm.ReportCardEntryType
    implicit val classUri = classUrisFor[ReportCardEntryType](clazz)

    private val entryType = property[String](lwm.entryType)
    private val bool = property[Boolean](lwm.bool)
    private val int = property[Int](lwm.int)

    implicit val reportCardEntryTypeBinding: PGBinder[Rdf, ReportCardEntryType] = pgbWithId[ReportCardEntryType](reportCardEntryType => makeUri(ReportCardEntryType.generateUri(reportCardEntryType)))(entryType, bool, int, id)(ReportCardEntryType.apply, ReportCardEntryType.unapply) withClasses classUri
  }

  object RescheduledBinding {
    implicit val clazz = lwm.Rescheduled
    implicit val classUri = classUrisFor[Rescheduled](clazz)

    private val date = property[LocalDate](lwm.date)
    private val start = property[LocalTime](lwm.start)
    private val end = property[LocalTime](lwm.end)
    private val room = property[UUID](lwm.room)(uuidRefBinder(Room.splitter))

    implicit val rescheduledBinding: PGBinder[Rdf, Rescheduled] = pgb[Rescheduled](date, start, end, room)(Rescheduled.apply, Rescheduled.unapply) withClasses classUri
  }
}

object Bindings {
  def apply[Rdf <: RDF](base: Namespace)(implicit ops: RDFOps[Rdf], recordBinder: RecordBinder[Rdf]) = new Bindings[Rdf]()(base, ops, recordBinder)
}