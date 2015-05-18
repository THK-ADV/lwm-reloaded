package store.bind

import java.util.UUID

import models._
import models.schedules.{GroupSchedule, GroupScheduleAssociation, StudentSchedule, StudentScheduleAssociation}
import models.timetable.{Timetable, TimetableEntry}
import models.users.{Employee, Student}
import models.users.{Employee, Student}
import store.Namespace
import store.Prefixes.LWMPrefix
import org.w3.banana._
import org.w3.banana.binder.{PGBinder, RecordBinder}

import scala.language.implicitConversions
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

  val id = property[UUID](lwm.id)

  object StudentBinding {
    implicit val clazz = lwm.Student
    implicit val classUri = classUrisFor[Student](clazz)
    private val lastname = property[String](lwm.lastname)
    private val firstname = property[String](lwm.firstname)
    private val registrationId = property[String](lwm.registrationId)
    private val id = property[UUID](lwm.id)
    private val systemId = property[String](lwm.systemId)
    private val email = property[String](lwm.email)
    implicit val studentBinder = pgbWithId[Student](student => makeUri(Student.generateUri(student)))(systemId, lastname, firstname, email, registrationId, id)(Student.apply, Student.unapply) withClasses classUri

  }

  object EmployeeBinding {
    implicit val clazz = lwm.Employee
    implicit val classUri = classUrisFor[Employee](clazz)
    private val lastname = property[String](lwm.lastname)
    private val firstname = property[String](lwm.firstname)
    private val id = property[UUID](lwm.id)
    private val systemId = property[String](lwm.systemId)
    private val email = property[String](lwm.email)
    implicit val employeeBinder = pgbWithId[Employee](employee => makeUri(Employee.generateUri(employee)))(systemId, lastname, firstname, email, id)(Employee.apply, Employee.unapply) withClasses classUri
  }


  object LabworkBinding {
    val clazz = lwm.Labwork
    implicit val classUri = classUrisFor[Labwork](clazz)
    val label = property[String](lwm.label)

    implicit val labworkBinder = pgbWithId[Labwork](labwork => makeUri(Labwork.generateUri(labwork)))(label, id)(Labwork.apply, Labwork.unapply) withClasses classUri
  }


  object CourseBinding {
    val clazz = lwm.Course
    implicit val classUri = classUrisFor[Course](clazz)
    private val label = property[String](lwm.label)
    private val lecturer = property[String](lwm.lecturer)

    implicit val courseBinder = pgbWithId[Course](course => makeUri(Course.generateUri(course)))(label, lecturer, id)(Course.apply, Course.unapply) withClasses classUri
  }

  object DegreeBinding {
    val clazz = lwm.Degree
    implicit val classUri = classUrisFor[Degree](clazz)
    private val label = property[String](lwm.label)

    implicit val degreeBinder = pgbWithId[Degree](degree => makeUri(Degree.generateUri(degree)))(label, id)(Degree.apply, Degree.unapply) withClasses classUri

  }

  object GroupBinding {
    val clazz = lwm.Group
    implicit val classUri = classUrisFor[Group](clazz)
    private val groupSchedule = property[String](lwm.groupSchedule)
    private val label = property[String](lwm.label)
    private val labwork = property[String](lwm.labwork)

    implicit val groupBinder = pgbWithId[Group](group => makeUri(Group.generateUri(group)))(groupSchedule, label, labwork, id)(Group.apply, Group.unapply) withClasses classUri
  }

  object GroupScheduleBinding {
    val clazz = lwm.GroupSchedule
    implicit val classUri = classUrisFor[GroupSchedule](clazz)


    implicit val groupScheduleBinder = pgbWithId[GroupSchedule](groupSchedule => makeUri(GroupSchedule.generateUri(groupSchedule)))(id)(GroupSchedule.apply, GroupSchedule.unapply) withClasses classUri
  }

  object GroupScheduleAssociationBinding {
    val clazz = lwm.GroupScheduleAssociation
    implicit val classUri = classUrisFor[GroupScheduleAssociation](clazz)
    private val date = property[String](lwm.date)
    private val timetableEntry = property[String](lwm.timetableEntry)

    implicit val groupScheduleAssociationBinder = pgbWithId[GroupScheduleAssociation](groupScheduleAssociation => makeUri(GroupScheduleAssociation.generateUri(groupScheduleAssociation)))(date, timetableEntry, id)(GroupScheduleAssociation.apply, GroupScheduleAssociation.unapply) withClasses classUri

  }

  object RoomBinding {
    val clazz = lwm.Room
    implicit val classUri = classUrisFor[Room](clazz)
    private val label = property[String](lwm.label)

    implicit val roomBinder = pgbWithId[Room](room => makeUri(Room.generateUri(room)))(label, id)(Room.apply, Room.unapply) withClasses classUri
  }

  object SemesterBinding {
    val clazz = lwm.Semester
    implicit val classUri = classUrisFor[Semester](clazz)
    private val name = property[String](lwm.name)
    private val startDate = property[String](lwm.startDate)
    private val endDate = property[String](lwm.endDate)

    implicit val semesterBinder = pgbWithId[Semester](semester => makeUri(Semester.generateUri(semester)))(name, startDate, endDate, id)(Semester.apply, Semester.unapply) withClasses classUri

  }


  object StudentScheduleBinding {
    val clazz = lwm.StudentSchedule
    implicit val classUri = classUrisFor[StudentSchedule](clazz)

    implicit val studentScheduleBinder = pgbWithId[StudentSchedule](studentSchedule => makeUri(StudentSchedule.generateUri(studentSchedule)))(id)(StudentSchedule.apply, StudentSchedule.unapply) withClasses classUri
  }

  object StudentScheduleAssociationBinding {
    implicit val clazz = lwm.StudentScheduleAssociation
    implicit val classUri = classUrisFor[StudentScheduleAssociation](clazz)
    private val date = property[String](lwm.date)
    private val groupScheduleAssociation = property[String](lwm.groupScheduleAssociation)
    private val timetableEntry = property[String](lwm.timetableEntry)

    implicit val studentScheduleAssociationBinder = pgbWithId[StudentScheduleAssociation](studentScheduleAssociation => makeUri(StudentScheduleAssociation.generateUri(studentScheduleAssociation)))(date, groupScheduleAssociation, timetableEntry, id)(StudentScheduleAssociation.apply, StudentScheduleAssociation.unapply) withClasses classUri

  }


  object TimetableBinding {
    implicit val clazz = lwm.Timetable
    implicit val classUri = classUrisFor[Timetable](clazz)


    implicit val timetableBinder = pgbWithId[Timetable](timetable => makeUri(Timetable.generateUri(timetable)))(id)(Timetable.apply, Timetable.unapply) withClasses classUri
  }

  object TimetableEntryBinding {
    implicit val clazz = lwm.TimetableEntry
    implicit val classUri = classUrisFor[TimetableEntry](clazz)
    private val supervisor = property[String](lwm.supervisor)
    private val room = property[String](lwm.room)
    private val startTime = property[String](lwm.startTime)
    private val endTime = property[String](lwm.endTime)

    implicit val timetableEntryBinder = pgbWithId[TimetableEntry](timetableEntry => makeUri(TimetableEntry.generateUri(timetableEntry)))(supervisor, room, startTime, endTime, id)(TimetableEntry.apply, TimetableEntry.unapply) withClasses classUri

  }

}

object Bindings {
  def apply[Rdf <: RDF](base: Namespace)(implicit ops: RDFOps[Rdf], recordBinder: RecordBinder[Rdf]) = new Bindings[Rdf]()(base, ops, recordBinder)
}
