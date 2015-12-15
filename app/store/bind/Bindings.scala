package store.bind

import java.util.UUID

import models._
import models.applications.LabworkApplication
import models.schedule.{ScheduleEntry, Schedule, TimetableEntry, Timetable}
import models.security.{Authority, Permission, Role, RefRole}
import models.users.{Employee, Student}
import org.joda.time.DateTime
import org.joda.time.format.ISODateTimeFormat
import org.w3.banana._
import org.w3.banana.binder.{PGBinder, RecordBinder}
import store.Namespace
import store.Prefixes.LWMPrefix

import scala.language.{postfixOps, implicitConversions}
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

  implicit val permissionBinder = new PGBinder[Rdf, Permission] {
    override def toPG(t: Permission): PointedGraph[Rdf] = {
      PointedGraph(ops.makeLiteral(t.value, xsd.string))
    }

    override def fromPG(pointed: PointedGraph[Rdf]): Try[Permission] = {
      pointed.pointer.as[String].map(Permission.apply)
    }
  }

  implicit val entryTypeBinder = new PGBinder[Rdf, EntryType] {
    override def toPG(t: EntryType): PointedGraph[Rdf] = {
      PointedGraph(ops.makeLiteral(t.value, xsd.string))
    }

    override def fromPG(pointed: PointedGraph[Rdf]): Try[EntryType] = {
      pointed.pointer.as[String].map(EntryType.apply)
    }
  }

  val id = property[UUID](lwm.id)

  object LabworkApplicationBinding {
    implicit val clazz = lwm.LabworkApplication
    implicit val classUri = classUrisFor[LabworkApplication](clazz)

    private val labwork = property[UUID](lwm.labwork)
    private val applicant = property[UUID](lwm.applicant)
    private val timestamp = property[DateTime](lwm.timestamp)
    private val friends = set[UUID](lwm.friends)

    implicit val labworkApplicationBinder = pgbWithId[LabworkApplication](application => makeUri(LabworkApplication.generateUri(application)))(labwork, applicant, friends, timestamp, id)(LabworkApplication.apply, LabworkApplication.unapply) withClasses classUri
  }

  object StudentBinding {
    implicit val clazz = lwm.Student
    implicit val classUri = classUrisFor[Student](clazz)

    private val lastname = property[String](lwm.lastname)
    private val firstname = property[String](lwm.firstname)
    private val registrationId = property[String](lwm.registrationId)
    private val systemId = property[String](lwm.systemId)
    private val enrollment = property[UUID](lwm.enrollment)
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
    import RoleBinding._

    implicit val clazz = lwm.RefRole
    implicit val classUri = classUrisFor[RefRole](clazz)

    private val module = optional[UUID](lwm.module)
    private val role = property[UUID](lwm.role)

    implicit val refRoleBinder = pgbWithId[RefRole](refRole => makeUri(RefRole.generateUri(refRole)))(module, role, id)(RefRole.apply, RefRole.unapply) withClasses classUri
  }

  object AuthorityBinding {
    import RefRoleBinding._

    implicit val clazz = lwm.Authority
    implicit val classUri = classUrisFor[Authority](clazz)

    private val privileged = property[UUID](lwm.privileged)
    private val refroles = set[RefRole](lwm.refroles)

    implicit val authorityBinder = pgbWithId[Authority](auth => makeUri(Authority.generateUri(auth)))(privileged, refroles, id)(Authority.apply, Authority.unapply) withClasses classUri
  }

  object AssignmentEntryBinding {
    implicit val clazz = lwm.AssignmentEntry
    implicit val classUri = classUrisFor[AssignmentEntry](clazz)

    private val index = property[Int](lwm.index)
    private val types = set[EntryType](lwm.types)

    implicit val assignmentEntryBinder = pgbWithId[AssignmentEntry](aEntry => makeUri(AssignmentEntry.generateUri(aEntry)))(index, types, id)(AssignmentEntry.apply, AssignmentEntry.unapply) withClasses classUri
  }


  object AssignmentPlanBinding {
    import AssignmentEntryBinding._

    implicit val clazz = lwm.AssignmentPlan
    implicit val classUri = classUrisFor[AssignmentPlan](clazz)

    private val numberOfEntries = property[Int](lwm.numberOfEntries)
    private val entries = set[AssignmentEntry](lwm.entries)

    implicit val assignmentPlanBinder = pgbWithId[AssignmentPlan](aPlan => makeUri(AssignmentPlan.generateUri(aPlan)))(numberOfEntries, entries, id)(AssignmentPlan.apply, AssignmentPlan.unapply) withClasses classUri
  }

  object LabworkBinding {
    import AssignmentPlanBinding._

    implicit val clazz = lwm.Labwork
    implicit val classUri = classUrisFor[Labwork](clazz)

    val label = property[String](lwm.label)
    val description = property[String](lwm.description)
    val semester = property[UUID](lwm.semester)
    val course = property[UUID](lwm.course)
    val degree = property[UUID](lwm.degree)
    val assignmentPlan = property[AssignmentPlan](lwm.assignmentPlan)

    implicit val labworkBinder = pgbWithId[Labwork](labwork => makeUri(Labwork.generateUri(labwork)))(label, description, semester, course, degree, assignmentPlan, id)(Labwork.apply, Labwork.unapply) withClasses classUri
  }

  object CourseBinding {
    implicit val clazz = lwm.Course
    implicit val classUri = classUrisFor[Course](clazz)

    private val label = property[String](lwm.label)
    private val abbreviation = property[String](lwm.abbreviation)
    private val lecturer = property[UUID](lwm.lecturer)

    implicit val courseBinder = pgbWithId[Course](course => makeUri(Course.generateUri(course)))(label, abbreviation, lecturer, id)(Course.apply, Course.unapply) withClasses classUri
  }

  object DegreeBinding {
    implicit val clazz = lwm.Degree
    implicit val classUri = classUrisFor[Degree](clazz)

    private val label = property[String](lwm.label)

    implicit val degreeBinder = pgbWithId[Degree](degree => makeUri(Degree.generateUri(degree)))(label, id)(Degree.apply, Degree.unapply) withClasses classUri
  }

  object GroupBinding {
    implicit val clazz = lwm.Group
    implicit val classUri = classUrisFor[Group](clazz)

    private val label = property[String](lwm.label)
    private val labwork = property[UUID](lwm.labwork)
    private val members = set[UUID](lwm.members)

    implicit val groupBinder = pgbWithId[Group](group => makeUri(Group.generateUri(group)))(label, labwork, members, id)(Group.apply, Group.unapply) withClasses classUri
  }

  object RoomBinding {
    implicit val clazz = lwm.Room
    implicit val classUri = classUrisFor[Room](clazz)

    private val label = property[String](lwm.label)

    implicit val roomBinder = pgbWithId[Room](room => makeUri(Room.generateUri(room)))(label, id)(Room.apply, Room.unapply) withClasses classUri
  }

  object SemesterBinding {
    implicit val clazz = lwm.Semester
    implicit val classUri = classUrisFor[Semester](clazz)

    private val name = property[String](lwm.name)
    private val start = property[String](lwm.start)
    private val end = property[String](lwm.end)
    private val exam = property[String](lwm.exam)

    implicit val semesterBinder = pgbWithId[Semester](semester => makeUri(Semester.generateUri(semester)))(name, start, end, exam, id)(Semester.apply, Semester.unapply) withClasses classUri
  }

  object TimetableBinding {
    import TimetableEntryBinding._

    implicit val clazz = lwm.Timetable
    implicit val classUri = classUrisFor[Timetable](clazz)

    private val labwork = property[UUID](lwm.labwork)
    private val entries = set[TimetableEntry](lwm.entries)(TimetableEntryBinding.timetableEntryBinder)
    private val start = property[DateTime](lwm.start)
    private val blacklist = set[DateTime](lwm.blacklist)
    private val buffer = property[Int](lwm.buffer)

    implicit val timetableBinder = pgbWithId[Timetable](timetable => makeUri(Timetable.generateUri(timetable)))(labwork, entries, start, blacklist, buffer, id)(Timetable.apply, Timetable.unapply) withClasses classUri
  }

  object TimetableEntryBinding {
    implicit val clazz = lwm.TimetableEntry
    implicit val classUri = classUrisFor[TimetableEntry](clazz)

    private val supervisor = property[UUID](lwm.supervisor)
    private val room = property[UUID](lwm.room)
    private val degree = property[UUID](lwm.degree)
    private val day = property[DateTime](lwm.day)
    private val start = property[DateTime](lwm.start)
    private val end = property[DateTime](lwm.end)
    private val date = property[DateTime](lwm.date)

    implicit val timetableEntryBinder = pgbWithId[TimetableEntry](timetableEntry => makeUri(TimetableEntry.generateUri(timetableEntry)))(supervisor, room, degree, day, start, end, date, id)(TimetableEntry.apply, TimetableEntry.unapply) withClasses classUri
  }

  object ScheduleBinding {
    import ScheduleEntryBinding._

    implicit val clazz = lwm.Schedule
    implicit val classUri = classUrisFor[Schedule](clazz)

    private val labwork = property[UUID](lwm.labwork)
    private val entries = set[ScheduleEntry](lwm.entries)(ScheduleEntryBinding.scheduleEntryBinder)

    implicit val scheduleBinder = pgbWithId[Schedule](schedule => makeUri(Schedule.generateUri(schedule)))(labwork, entries, id)(Schedule.apply, Schedule.unapply) withClasses classUri
  }

  object ScheduleEntryBinding {
    implicit val clazz = lwm.ScheduleEntry
    implicit val classUri = classUrisFor[ScheduleEntry](clazz)

    private val start = property[DateTime](lwm.start)
    private val end = property[DateTime](lwm.end)
    private val day = property[DateTime](lwm.day)
    private val date = property[DateTime](lwm.date)
    private val room = property[UUID](lwm.room)
    private val supervisor = property[UUID](lwm.supervisor)
    private val group = property[UUID](lwm.group)

    implicit val scheduleEntryBinder = pgbWithId[ScheduleEntry](scheduleEntry => makeUri(ScheduleEntry.generateUri(scheduleEntry)))(start, end, day, date, room, supervisor, group, id)(ScheduleEntry.apply, ScheduleEntry.unapply) withClasses classUri
  }
}

object Bindings {
  def apply[Rdf <: RDF](base: Namespace)(implicit ops: RDFOps[Rdf], recordBinder: RecordBinder[Rdf]) = new Bindings[Rdf]()(base, ops, recordBinder)
}