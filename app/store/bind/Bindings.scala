package store.bind

import java.util.UUID

import models._
import models.applications.LabworkApplication
import models.schedule._
import models.security._
import models.semester.{Blacklist, Semester}
import models.users.{Employee, Student, StudentProtocol, User}
import org.joda.time.{DateTime, LocalDate, LocalTime}
import org.joda.time.format.ISODateTimeFormat
import org.w3.banana._
import org.w3.banana.binder.{PGBinder, RecordBinder}
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

  object LabworkApplicationBinding {
    implicit val clazz = lwm.LabworkApplication
    implicit val classUri = classUrisFor[LabworkApplication](clazz)

    private val labwork = property[UUID](lwm.labwork)(uuidRefBinder(Labwork.splitter))
    private val applicant = property[UUID](lwm.applicant)(uuidRefBinder(Student.splitter))
    private val timestamp = property[DateTime](lwm.timestamp)
    private val friends = set[UUID](lwm.friends)(uuidRefBinder(Student.splitter))

    implicit val labworkApplicationBinder = pgbWithId[LabworkApplication](application => makeUri(LabworkApplication.generateUri(application)))(labwork, applicant, friends, timestamp, id)(LabworkApplication.apply, LabworkApplication.unapply) withClasses classUri
  }


  object StudentBinding {
    implicit val clazz = lwm.Student
    implicit val classUri = classUrisFor[Student](clazz)

    private val lastname = property[String](lwm.lastname)
    private val firstname = property[String](lwm.firstname)
    private val registrationId = property[String](lwm.registrationId)
    private val systemId = property[String](lwm.systemId)
    private val enrollment = property[UUID](lwm.enrollment)(uuidRefBinder(Degree.splitter))
    private val email = property[String](lwm.email)

    implicit val studentBinder = pgbWithId[Student](student => makeUri(Student.generateUri(student)))(systemId, lastname, firstname, email, registrationId, enrollment, id)(Student.apply, Student.unapply) withClasses classUri
  }

  object EmployeeBinding {
    implicit val clazz = lwm.Employee
    implicit val classUri = classUrisFor[Employee](clazz)

    private val lastname = property[String](lwm.lastname)
    private val firstname = property[String](lwm.firstname)
    private val systemId = property[String](lwm.systemId)
    private val email = property[String](lwm.email)

    implicit val employeeBinder = pgbWithId[Employee](employee => makeUri(Employee.generateUri(employee)))(systemId, lastname, firstname, email, id)(Employee.apply, Employee.unapply) withClasses classUri
  }

  object RoleBinding {
    implicit val clazz = lwm.Role
    implicit val classUri = classUrisFor[Role](clazz)

    private val name = property[String](lwm.name)
    private val permissions = set[Permission](lwm.permissions)

    implicit val roleBinder = pgbWithId[Role](role => makeUri(Role.generateUri(role)))(name, permissions, id)(Role.apply, Role.unapply) withClasses classUri
  }

  object RefRoleBinding {
    import RoleBinding.roleBinder

    implicit val clazz = lwm.RefRole
    implicit val classUri = classUrisFor[RefRole](clazz)

    private val module = optional[UUID](lwm.module)(uuidRefBinder(Course.splitter))
    private val role = property[UUID](lwm.role)(uuidRefBinder(Role.splitter))

    implicit val refRoleBinder = pgbWithId[RefRole](refRole => makeUri(RefRole.generateUri(refRole)))(module, role, id)(RefRole.apply, RefRole.unapply) withClasses classUri
  }

  object AuthorityBinding {
    implicit val clazz = lwm.Authority
    implicit val classUri = classUrisFor[Authority](clazz)

    private val privileged = property[UUID](lwm.privileged)(uuidRefBinder(User.splitter))
    private val refroles = set[UUID](lwm.refroles)(uuidRefBinder(RefRole.splitter))

    implicit val authorityBinder = pgbWithId[Authority](auth => makeUri(Authority.generateUri(auth)))(privileged, refroles, id)(Authority.apply, Authority.unapply) withClasses classUri
  }

  object AssignmentEntryTypeBinding {
    implicit val clazz = lwm.AssignmentEntryType
    implicit val classUri = classUrisFor[AssignmentEntryType](clazz)

    private val entryType = property[String](lwm.entryType)
    private val bool = property[Boolean](lwm.bool)
    private val int = property[Int](lwm.int)

    implicit val assignmentEntryTypeBinder = pgbWithId[AssignmentEntryType](aEntryType => makeUri(AssignmentEntryType.generateUri(aEntryType)))(entryType, bool, int, id)(AssignmentEntryType.apply, AssignmentEntryType.unapply) withClasses classUri
  }

  object AssignmentEntryBinding {
    implicit val clazz = lwm.AssignmentEntry
    implicit val classUri = classUrisFor[AssignmentEntry](clazz)

    private val index = property[Int](lwm.index)
    private val label = property[String](lwm.label)
    private val duration = property[Int](lwm.duration)
    private val types = set[AssignmentEntryType](lwm.types)(AssignmentEntryTypeBinding.assignmentEntryTypeBinder)

    implicit val assignmentEntryBinder = pgbWithId[AssignmentEntry](aEntry => makeUri(AssignmentEntry.generateUri(aEntry)))(index, label, types, duration, id)(AssignmentEntry.apply, AssignmentEntry.unapply) withClasses classUri
  }

  object AssignmentPlanBinding {
    implicit val clazz = lwm.AssignmentPlan
    implicit val classUri = classUrisFor[AssignmentPlan](clazz)

    private val labwork = property[UUID](lwm.labwork)(uuidRefBinder(Labwork.splitter))
    private val attendance = property[Int](lwm.attendance)
    private val mandatory = property[Int](lwm.mandatory)
    private val entries = set[AssignmentEntry](lwm.entries)(AssignmentEntryBinding.assignmentEntryBinder)

    implicit val assignmentPlanBinder = pgbWithId[AssignmentPlan](aPlan => makeUri(AssignmentPlan.generateUri(aPlan)))(labwork, attendance, mandatory, entries, id)(AssignmentPlan.apply, AssignmentPlan.unapply) withClasses classUri
  }

  object LabworkBinding {
    implicit val clazz = lwm.Labwork
    implicit val classUri = classUrisFor[Labwork](clazz)

    val label = property[String](lwm.label)
    val description = property[String](lwm.description)
    val semester = property[UUID](lwm.semester)(uuidRefBinder(Semester.splitter))
    val course = property[UUID](lwm.course)(uuidRefBinder(Course.splitter))
    val degree = property[UUID](lwm.degree)(uuidRefBinder(Degree.splitter))

    implicit val labworkBinder = pgbWithId[Labwork](labwork => makeUri(Labwork.generateUri(labwork)))(label, description, semester, course, degree, id)(Labwork.apply, Labwork.unapply) withClasses classUri
  }

  object CourseBinding {
    implicit val clazz = lwm.Course
    implicit val classUri = classUrisFor[Course](clazz)

    private val label = property[String](lwm.label)
    private val description = property[String](lwm.description)
    private val abbreviation = property[String](lwm.abbreviation)
    private val lecturer = property[UUID](lwm.lecturer)(uuidRefBinder(Employee.splitter))
    private val semesterIndex = property[Int](lwm.semesterIndex)

    implicit val courseBinder = pgbWithId[Course](course => makeUri(Course.generateUri(course)))(label, description, abbreviation, lecturer, semesterIndex, id)(Course.apply, Course.unapply) withClasses classUri
  }

  object DegreeBinding {
    implicit val clazz = lwm.Degree
    implicit val classUri = classUrisFor[Degree](clazz)

    private val label = property[String](lwm.label)
    private val abbreviation = property[String](lwm.abbreviation)

    implicit val degreeBinder = pgbWithId[Degree](degree => makeUri(Degree.generateUri(degree)))(label, abbreviation, id)(Degree.apply, Degree.unapply) withClasses classUri
  }

  object GroupBinding {
    implicit val clazz = lwm.Group
    implicit val classUri = classUrisFor[Group](clazz)

    private val label = property[String](lwm.label)
    private val labwork = property[UUID](lwm.labwork)(uuidRefBinder(Labwork.splitter))
    private val members = set[UUID](lwm.members)(uuidRefBinder(Student.splitter))

    implicit val groupBinder = pgbWithId[Group](group => makeUri(Group.generateUri(group)))(label, labwork, members, id)(Group.apply, Group.unapply) withClasses classUri
  }

  object RoomBinding {
    implicit val clazz = lwm.Room
    implicit val classUri = classUrisFor[Room](clazz)

    private val label = property[String](lwm.label)
    private val description = property[String](lwm.description)

    implicit val roomBinder = pgbWithId[Room](room => makeUri(Room.generateUri(room)))(label, description, id)(Room.apply, Room.unapply) withClasses classUri
  }

  object SemesterBinding {
    implicit val clazz = lwm.Semester
    implicit val classUri = classUrisFor[Semester](clazz)

    private val label = property[String](lwm.label)
    private val abbreviation = property[String](lwm.abbreviation)
    private val start = property[LocalDate](lwm.start)
    private val end = property[LocalDate](lwm.end)
    private val examStart = property[LocalDate](lwm.examStart)

    implicit val semesterBinder = pgbWithId[Semester](semester => makeUri(Semester.generateUri(semester)))(label, abbreviation, start, end, examStart, id)(Semester.apply, Semester.unapply) withClasses classUri
  }

  object TimetableBinding {
    implicit val clazz = lwm.Timetable
    implicit val classUri = classUrisFor[Timetable](clazz)

    private val labwork = property[UUID](lwm.labwork)(uuidRefBinder(Labwork.splitter))
    private val entries = set[TimetableEntry](lwm.entries)(TimetableEntryBinding.timetableEntryBinder)
    private val start = property[LocalDate](lwm.start)
    private val blacklist = property[Blacklist](lwm.blacklist)(BlacklistBinding.blacklistBinder)

    implicit val timetableBinder = pgbWithId[Timetable](timetable => makeUri(Timetable.generateUri(timetable)))(labwork, entries, start, blacklist, id)(Timetable.apply, Timetable.unapply) withClasses classUri
  }

  object TimetableEntryBinding {
    implicit val clazz = lwm.TimetableEntry
    implicit val classUri = classUrisFor[TimetableEntry](clazz)

    private val supervisor = property[UUID](lwm.supervisor)(uuidRefBinder(Employee.splitter))
    private val room = property[UUID](lwm.room)(uuidRefBinder(Room.splitter))
    private val degree = property[UUID](lwm.degree)(uuidRefBinder(Degree.splitter))
    private val dayIndex = property[Int](lwm.dayIndex)
    private val start = property[LocalTime](lwm.start)
    private val end = property[LocalTime](lwm.end)

    implicit val timetableEntryBinder = pgbWithId[TimetableEntry](timetableEntry => makeUri(TimetableEntry.generateUri(timetableEntry)))(supervisor, room, degree, dayIndex, start, end, id)(TimetableEntry.apply, TimetableEntry.unapply) withClasses classUri
  }

  object ScheduleBinding {
    implicit val clazz = lwm.Schedule
    implicit val classUri = classUrisFor[Schedule](clazz)

    private val labwork = property[UUID](lwm.labwork)(uuidRefBinder(Labwork.splitter))
    private val entries = set[ScheduleEntry](lwm.entries)(ScheduleEntryBinding.scheduleEntryBinder)
    private val published = property[Boolean](lwm.published)

    implicit val scheduleBinder = pgbWithId[Schedule](schedule => makeUri(Schedule.generateUri(schedule)))(labwork, entries, published, id)(Schedule.apply, Schedule.unapply) withClasses classUri
  }

  object ScheduleEntryBinding {
    implicit val clazz = lwm.ScheduleEntry
    implicit val classUri = classUrisFor[ScheduleEntry](clazz)

    private val start = property[LocalTime](lwm.start)
    private val end = property[LocalTime](lwm.end)
    private val date = property[LocalDate](lwm.date)
    private val room = property[UUID](lwm.room)(uuidRefBinder(Room.splitter))
    private val supervisor = property[UUID](lwm.supervisor)(uuidRefBinder(Employee.splitter))
    private val group = property[UUID](lwm.group)(uuidRefBinder(Group.splitter))

    implicit val scheduleEntryBinder = pgbWithId[ScheduleEntry](scheduleEntry => makeUri(ScheduleEntry.generateUri(scheduleEntry)))(start, end, date, room, supervisor, group, id)(ScheduleEntry.apply, ScheduleEntry.unapply) withClasses classUri
  }

  object BlacklistBinding {
    implicit val clazz = lwm.Blacklist
    implicit val classUri = classUrisFor[Blacklist](clazz)

    private val dates = set[DateTime](lwm.dates)

    implicit val blacklistBinder = pgbWithId[Blacklist](blacklist => makeUri(Blacklist.generateUri(blacklist)))(dates, id)(Blacklist.apply, Blacklist.unapply) withClasses classUri
  }

  object ReportCardBinding {
    implicit val clazz = lwm.ReportCard
    implicit val classUri = classUrisFor[ReportCard](clazz)

    private val student = property[UUID](lwm.student)(uuidRefBinder(Student.splitter))
    private val labwork = property[UUID](lwm.labwork)(uuidRefBinder(Labwork.splitter))
    private val entries = set[ReportCardEntry](lwm.entries)(ReportCardEntryBinding.reportCardEntryBinding)

    implicit val reportCardBinder = pgbWithId[ReportCard](reportCard => makeUri(ReportCard.generateUri(reportCard)))(student, labwork, entries, id)(ReportCard.apply, ReportCard.unapply) withClasses classUri
  }

  object ReportCardEntryBinding {
    implicit val clazz = lwm.ReportCardEntry
    implicit val classUri = classUrisFor[ReportCardEntry](clazz)

    private val index = property[Int](lwm.index)
    private val label = property[String](lwm.label)
    private val date = property[LocalDate](lwm.date)
    private val start = property[LocalTime](lwm.start)
    private val end = property[LocalTime](lwm.end)
    private val room = property[UUID](lwm.room)
    private val types = set[AssignmentEntryType](lwm.types)(AssignmentEntryTypeBinding.assignmentEntryTypeBinder)

    implicit val reportCardEntryBinding = pgbWithId[ReportCardEntry](reportCardEntry => makeUri(ReportCardEntry.generateUri(reportCardEntry)))(index, label, date, start, end, room, types, id)(ReportCardEntry.apply, ReportCardEntry.unapply) withClasses classUri
  }
}

object Bindings {
  def apply[Rdf <: RDF](base: Namespace)(implicit ops: RDFOps[Rdf], recordBinder: RecordBinder[Rdf]) = new Bindings[Rdf]()(base, ops, recordBinder)
}