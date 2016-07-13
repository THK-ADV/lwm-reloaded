package store.bind

import java.util.UUID

import models._
import models.labwork._
import models.security._
import models.semester.{Blacklist, Semester}
import models.users.{Employee, Student, StudentAtom, User}
import org.joda.time.{DateTime, LocalDate, LocalTime}
import org.joda.time.format.ISODateTimeFormat
import org.w3.banana._
import org.w3.banana.binder.{ClassUrisFor, PGBinder, RecordBinder}
import store.Namespace
import store.Prefixes.LWMPrefix

import scala.language.{implicitConversions, postfixOps}
import scala.util.Try
import Descriptor._

object Descriptor {

  trait Descriptor[Rdf <: RDF, T] {
    def clazz: Rdf#URI

    def compositeClassUris: CompositeClassUris[Rdf, T]

    def binder: PGBinder[Rdf, T]
  }

  def compositeClass[Rdf <: RDF, T](clazz: Rdf#URI, classComponents: Rdf#URI*): CompositeClassUris[Rdf, T] = new CompositeClassUris[Rdf, T] {
    override def classes: Iterable[Rdf#URI] = Iterable(clazz)

    override def components: Iterable[Rdf#URI] = classComponents
  }

  trait CompositeClassUris[Rdf <: RDF, T] {
    self =>
    def classes: Iterable[Rdf#URI]

    def components: Iterable[Rdf#URI]

    def classUris: ClassUrisFor[Rdf, T] = new ClassUrisFor[Rdf, T] {
      override def classes: Iterable[Rdf#URI] = self.classes
    }

    def #>[A](c: CompositeClassUris[Rdf, A]): CompositeClassUris[Rdf, T] = new CompositeClassUris[Rdf, T] {
      override def classes: Iterable[Rdf#URI] = self.classes

      override def components: Iterable[Rdf#URI] = self.components ++ c.classes ++ c.components
    }
  }

}

class Bindings[Rdf <: RDF](implicit baseNs: Namespace, ops: RDFOps[Rdf], recordBinder: RecordBinder[Rdf]) {

  import ops._
  import recordBinder._

  val lwm = LWMPrefix[Rdf]
  val rdf = RDFPrefix[Rdf]
  val xsd = XSDPrefix[Rdf]

  def innerUri: Rdf#URI = makeUri(s"$baseNs/${newUri("#")}")

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

  implicit val dateTimeBinder = new PGBinder[Rdf, DateTime] {
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

  implicit val localDateBinder = new PGBinder[Rdf, LocalDate] {
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

  implicit val localTimeBinder = new PGBinder[Rdf, LocalTime] {
    // LocalTime.toString formats to HH:mm:ms:mms
    override def toPG(t: LocalTime): PointedGraph[Rdf] = {
      PointedGraph(ops.makeLiteral(t.toString, lwm.localTime))
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

  implicit lazy val UserDescriptor: Descriptor[Rdf, User] = new Descriptor[Rdf, User] {
    override val clazz: Rdf#URI = lwm.User

    override val compositeClassUris: CompositeClassUris[Rdf, User] = compositeClass[Rdf, User](clazz)

    override val binder: PGBinder[Rdf, User] = new PGBinder[Rdf, User] {
      override def fromPG(pointed: PointedGraph[Rdf]): Try[User] = {
        StudentDescriptor.binder.fromPG(pointed) orElse EmployeeDescriptor.binder.fromPG(pointed)
      }

      override def toPG(t: User): PointedGraph[Rdf] = t match {
        case s: Student => s.toPG(StudentDescriptor.binder)
        case e: Employee => e.toPG(EmployeeDescriptor.binder)
      }
    }
  }

  implicit lazy val StudentDescriptor: Descriptor[Rdf, Student] = new Descriptor[Rdf, Student] {
    override val clazz: Rdf#URI = lwm.User

    override val compositeClassUris: CompositeClassUris[Rdf, Student] = compositeClass[Rdf, Student](clazz)

    private val lastname = property[String](lwm.lastname)
    private val firstname = property[String](lwm.firstname)
    private val registrationId = property[String](lwm.registrationId)
    private val systemId = property[String](lwm.systemId)
    private val enrollment = property[UUID](lwm.enrollment)(uuidRefBinder(Degree.splitter))
    private val email = property[String](lwm.email)

    override val binder: PGBinder[Rdf, Student] =
      pgbWithId[Student](student =>
        makeUri(User.generateUri(student)))(systemId, lastname, firstname, email, registrationId, enrollment, id)(Student.apply, Student.unapply) withClasses compositeClassUris.classUris
  }

  implicit lazy val StudentAtomDescriptor: Descriptor[Rdf, StudentAtom] = new Descriptor[Rdf, StudentAtom] {
    override val clazz: Rdf#URI = lwm.User

    override val compositeClassUris: CompositeClassUris[Rdf, StudentAtom] =
      compositeClass[Rdf, StudentAtom](clazz) #>
        DegreeDescriptor.compositeClassUris

    private val lastname = property[String](lwm.lastname)
    private val firstname = property[String](lwm.firstname)
    private val registrationId = property[String](lwm.registrationId)
    private val enrollment = property[Degree](lwm.enrollment)(DegreeDescriptor.binder)
    private val systemId = property[String](lwm.systemId)
    private val email = property[String](lwm.email)


    override val binder: PGBinder[Rdf, StudentAtom] =
      pgbWithId[StudentAtom](student =>
        makeUri(User.generateUri(student.id)))(systemId, lastname, firstname, email, registrationId, enrollment, id)(StudentAtom.apply, StudentAtom.unapply) withClasses compositeClassUris.classUris
  }

  implicit lazy val EmployeeDescriptor: Descriptor[Rdf, Employee] = new Descriptor[Rdf, Employee] {
    override val clazz: Rdf#URI = lwm.User

    override val compositeClassUris: CompositeClassUris[Rdf, Employee] = compositeClass[Rdf, Employee](clazz)

    private val lastname = property[String](lwm.lastname)
    private val firstname = property[String](lwm.firstname)
    private val systemId = property[String](lwm.systemId)
    private val email = property[String](lwm.email)
    private val status = property[String](lwm.status)

    override val binder: PGBinder[Rdf, Employee] =
      pgbWithId[Employee](employee =>
        makeUri(User.generateUri(employee)))(systemId, lastname, firstname, email, status, id)(Employee.apply, Employee.unapply) withClasses compositeClassUris.classUris

  }

  implicit lazy val LabworkApplicationDescriptor: Descriptor[Rdf, LabworkApplication] = new Descriptor[Rdf, LabworkApplication] {
    override val clazz: Rdf#URI = lwm.LabworkApplication

    override val compositeClassUris: CompositeClassUris[Rdf, LabworkApplication] =
      compositeClass[Rdf, LabworkApplication](clazz) #>
        LabworkDescriptor.compositeClassUris #>
        StudentDescriptor.compositeClassUris

    private val labwork = property[UUID](lwm.labwork)(uuidRefBinder(Labwork.splitter))
    private val applicant = property[UUID](lwm.applicant)(uuidRefBinder(User.splitter))
    private val timestamp = property[DateTime](lwm.timestamp)
    private val friends = set[UUID](lwm.friends)(uuidRefBinder(User.splitter))

    override val binder: PGBinder[Rdf, LabworkApplication] =
      pgbWithId[LabworkApplication](application =>
        makeUri(LabworkApplication.generateUri(application)))(labwork, applicant, friends, timestamp, id)(LabworkApplication.apply, LabworkApplication.unapply) withClasses compositeClassUris.classUris
  }

  implicit lazy val LabworkApplicationAtomDescriptor: Descriptor[Rdf, LabworkApplicationAtom] = new Descriptor[Rdf, LabworkApplicationAtom] {
    override val clazz: Rdf#URI = lwm.LabworkApplication

    override val compositeClassUris: CompositeClassUris[Rdf, LabworkApplicationAtom] =
      compositeClass[Rdf, LabworkApplicationAtom](clazz) #>
        LabworkAtomDescriptor.compositeClassUris #>
        StudentAtomDescriptor.compositeClassUris

    private val labwork = property[Labwork](lwm.labwork)(LabworkDescriptor.binder)
    private val applicant = property[Student](lwm.applicant)(StudentDescriptor.binder)
    private val friends = set[Student](lwm.friends)(StudentDescriptor.binder)
    private val timestamp = property[DateTime](lwm.timestamp)

    override val binder: PGBinder[Rdf, LabworkApplicationAtom] =
      pgbWithId[LabworkApplicationAtom](application =>
        makeUri(LabworkApplication.generateUri(application.id)))(labwork, applicant, friends, timestamp, id)(LabworkApplicationAtom.apply, LabworkApplicationAtom.unapply) withClasses compositeClassUris.classUris

  }

  implicit lazy val RoleDescriptor: Descriptor[Rdf, Role] = new Descriptor[Rdf, Role] {
    override val clazz: Rdf#URI = lwm.Role

    override val compositeClassUris: CompositeClassUris[Rdf, Role] = compositeClass[Rdf, Role](clazz)

    private val label = property[String](lwm.label)
    private val permissions = set[Permission](lwm.permissions)

    override val binder: PGBinder[Rdf, Role] =
      pgbWithId[Role](role =>
        makeUri(Role.generateUri(role)))(label, permissions, id)(Role.apply, Role.unapply) withClasses compositeClassUris.classUris

  }

  implicit lazy val RefRoleDescriptor: Descriptor[Rdf, RefRole] = new Descriptor[Rdf, RefRole] {
    override val clazz: Rdf#URI = lwm.RefRole

    override val compositeClassUris: CompositeClassUris[Rdf, RefRole] = compositeClass[Rdf, RefRole](clazz)

    private val course = optional[UUID](lwm.course)(uuidRefBinder(Course.splitter))
    private val role = property[UUID](lwm.role)(uuidRefBinder(Role.splitter))

    override val binder: PGBinder[Rdf, RefRole] =
      pgbWithId[RefRole](refRole =>
        makeUri(RefRole.generateUri(refRole)))(course, role, id)(RefRole.apply, RefRole.unapply) withClasses compositeClassUris.classUris
  }

  implicit lazy val RefRoleAtomDescriptor: Descriptor[Rdf, RefRoleAtom] = new Descriptor[Rdf, RefRoleAtom] {
    override val clazz: Rdf#URI = lwm.RefRole

    override val compositeClassUris: CompositeClassUris[Rdf, RefRoleAtom] =
      compositeClass[Rdf, RefRoleAtom](clazz) #>
        CourseAtomDescriptor.compositeClassUris #>
        RoleDescriptor.compositeClassUris

    private val course = optional[CourseAtom](lwm.course)(CourseAtomDescriptor.binder)
    private val role = property[Role](lwm.role)(RoleDescriptor.binder)

    override val binder: PGBinder[Rdf, RefRoleAtom] =
      pgbWithId[RefRoleAtom](refRole =>
        makeUri(RefRole.generateUri(refRole.id)))(course, role, id)(RefRoleAtom.apply, RefRoleAtom.unapply) withClasses compositeClassUris.classUris
  }


  implicit lazy val AuthorityDescriptor: Descriptor[Rdf, Authority] = new Descriptor[Rdf, Authority] {
    override val clazz: Rdf#URI = lwm.Authority

    override val compositeClassUris: CompositeClassUris[Rdf, Authority] = compositeClass[Rdf, Authority](clazz)

    private val privileged = property[UUID](lwm.privileged)(uuidRefBinder(User.splitter))
    private val refroles = set[UUID](lwm.refroles)(uuidRefBinder(RefRole.splitter))

    override val binder: PGBinder[Rdf, Authority] =
      pgbWithId[Authority](auth =>
        makeUri(Authority.generateUri(auth)))(privileged, refroles, id)(Authority.apply, Authority.unapply) withClasses compositeClassUris.classUris
  }


  implicit lazy val AuthorityAtomDescriptor: Descriptor[Rdf, AuthorityAtom] = new Descriptor[Rdf, AuthorityAtom] {
    override val clazz: Rdf#URI = lwm.Authority

    override val compositeClassUris: CompositeClassUris[Rdf, AuthorityAtom] =
      compositeClass[Rdf, AuthorityAtom](clazz) #>
        UserDescriptor.compositeClassUris #>
        RefRoleAtomDescriptor.compositeClassUris

    private val privileged = property[User](lwm.privileged)(UserDescriptor.binder)
    private val refrole = set[RefRoleAtom](lwm.refroles)(RefRoleAtomDescriptor.binder)

    override val binder: PGBinder[Rdf, AuthorityAtom] =
      pgbWithId[AuthorityAtom](
        auth => makeUri(Authority.generateUri(auth.id)))(privileged, refrole, id)(AuthorityAtom.apply, AuthorityAtom.unapply) withClasses compositeClassUris.classUris
  }

  implicit lazy val AssignmentEntryDescriptor: Descriptor[Rdf, AssignmentEntry] = new Descriptor[Rdf, AssignmentEntry] {
    override val clazz: Rdf#URI = lwm.AssignmentEntry

    override val compositeClassUris: CompositeClassUris[Rdf, AssignmentEntry] =
      compositeClass[Rdf, AssignmentEntry](clazz) #>
        AssignmentEntryTypeDescriptor.compositeClassUris

    private val index = property[Int](lwm.index)
    private val label = property[String](lwm.label)
    private val duration = property[Int](lwm.duration)
    private val types = set[AssignmentEntryType](lwm.entryTypes)(AssignmentEntryTypeDescriptor.binder)

    override val binder: PGBinder[Rdf, AssignmentEntry] =
      pgbWithId[AssignmentEntry](_ => innerUri)(index, label, types, duration)(AssignmentEntry.apply, AssignmentEntry.unapply) withClasses compositeClassUris.classUris
  }

  implicit lazy val AssignmentEntryTypeDescriptor: Descriptor[Rdf, AssignmentEntryType] = new Descriptor[Rdf, AssignmentEntryType] {
    override val clazz: Rdf#URI = lwm.AssignmentEntryType

    override val compositeClassUris: CompositeClassUris[Rdf, AssignmentEntryType] = compositeClass[Rdf, AssignmentEntryType](clazz)

    private val entryType = property[String](lwm.entryType)
    private val bool = property[Boolean](lwm.bool)
    private val int = property[Int](lwm.int)

    override val binder: PGBinder[Rdf, AssignmentEntryType] =
      pgbWithId[AssignmentEntryType](_ => innerUri)(entryType, bool, int)(AssignmentEntryType.apply, AssignmentEntryType.unapply) withClasses compositeClassUris.classUris
  }


  implicit lazy val AssignmentPlanDescriptor: Descriptor[Rdf, AssignmentPlan] = new Descriptor[Rdf, AssignmentPlan] {
    override val clazz: Rdf#URI = lwm.AssignmentPlan

    override val compositeClassUris: CompositeClassUris[Rdf, AssignmentPlan] =
      compositeClass[Rdf, AssignmentPlan](clazz) #>
        AssignmentEntryDescriptor.compositeClassUris

    private val labwork = property[UUID](lwm.labwork)(uuidRefBinder(Labwork.splitter))
    private val attendance = property[Int](lwm.attendance)
    private val mandatory = property[Int](lwm.mandatory)
    private val entries = set[AssignmentEntry](lwm.entries)(AssignmentEntryDescriptor.binder)

    override val binder: PGBinder[Rdf, AssignmentPlan] =
      pgbWithId[AssignmentPlan](aPlan =>
        makeUri(AssignmentPlan.generateUri(aPlan)))(labwork, attendance, mandatory, entries, id)(AssignmentPlan.apply, AssignmentPlan.unapply) withClasses compositeClassUris.classUris
  }

  implicit lazy val AssignmentPlanAtomDescriptor: Descriptor[Rdf, AssignmentPlanAtom] = new Descriptor[Rdf, AssignmentPlanAtom] {
    override val clazz: Rdf#URI = lwm.AssignmentPlan

    override val compositeClassUris: CompositeClassUris[Rdf, AssignmentPlanAtom] =
      compositeClass[Rdf, AssignmentPlanAtom](clazz) #>
        LabworkDescriptor.compositeClassUris #>
        AssignmentEntryDescriptor.compositeClassUris

    private val labwork = property[Labwork](lwm.labwork)(LabworkDescriptor.binder)
    private val attendance = property[Int](lwm.attendance)
    private val mandatory = property[Int](lwm.mandatory)
    private val entries = set[AssignmentEntry](lwm.entries)(AssignmentEntryDescriptor.binder)

    override val binder: PGBinder[Rdf, AssignmentPlanAtom] =
      pgbWithId[AssignmentPlanAtom](aPlan =>
        makeUri(AssignmentPlan.generateUri(aPlan.id)))(labwork, attendance, mandatory, entries, id)(AssignmentPlanAtom.apply, AssignmentPlanAtom.unapply) withClasses compositeClassUris.classUris
  }

  implicit lazy val LabworkDescriptor: Descriptor[Rdf, Labwork] = new Descriptor[Rdf, Labwork] {
    override val clazz: Rdf#URI = lwm.Labwork

    override val compositeClassUris: CompositeClassUris[Rdf, Labwork] = compositeClass[Rdf, Labwork](clazz)

    private val label = property[String](lwm.label)
    private val description = property[String](lwm.description)
    private val semester = property[UUID](lwm.semester)(uuidRefBinder(Semester.splitter))
    private val course = property[UUID](lwm.course)(uuidRefBinder(Course.splitter))
    private val degree = property[UUID](lwm.degree)(uuidRefBinder(Degree.splitter))
    private val subscribable = property[Boolean](lwm.subscribable)
    private val published = property[Boolean](lwm.published)

    override val binder: PGBinder[Rdf, Labwork] =
      pgbWithId[Labwork](labwork =>
        makeUri(Labwork.generateUri(labwork)))(label, description, semester, course, degree, subscribable, published, id)(Labwork.apply, Labwork.unapply) withClasses compositeClassUris.classUris
  }

  implicit lazy val LabworkAtomDescriptor: Descriptor[Rdf, LabworkAtom] = new Descriptor[Rdf, LabworkAtom] {
    override val clazz: Rdf#URI = lwm.Labwork

    override val compositeClassUris: CompositeClassUris[Rdf, LabworkAtom] =
      compositeClass[Rdf, LabworkAtom](clazz) #>
        SemesterDescriptor.compositeClassUris #>
        CourseAtomDescriptor.compositeClassUris #>
        DegreeDescriptor.compositeClassUris

    private val label = property[String](lwm.label)
    private val description = property[String](lwm.description)
    private val semester = property[Semester](lwm.semester)(SemesterDescriptor.binder)
    private val course = property[CourseAtom](lwm.course)(CourseAtomDescriptor.binder)
    private val degree = property[Degree](lwm.degree)(DegreeDescriptor.binder)
    private val subscribable = property[Boolean](lwm.subscribable)
    private val published = property[Boolean](lwm.published)

    override val binder: PGBinder[Rdf, LabworkAtom] =
      pgbWithId[LabworkAtom](
        atom => makeUri(Labwork.generateUri(atom.id)))(label, description, semester, course, degree, subscribable, published, id)(LabworkAtom.apply, LabworkAtom.unapply) withClasses compositeClassUris.classUris

  }

  implicit lazy val CourseDescriptor: Descriptor[Rdf, Course] = new Descriptor[Rdf, Course] {
    override val clazz: Rdf#URI = lwm.Course

    override val compositeClassUris: CompositeClassUris[Rdf, Course] = compositeClass[Rdf, Course](clazz)

    private val label = property[String](lwm.label)
    private val description = property[String](lwm.description)
    private val abbreviation = property[String](lwm.abbreviation)
    private val lecturer = property[UUID](lwm.lecturer)(uuidRefBinder(User.splitter))
    private val semesterIndex = property[Int](lwm.semesterIndex)

    override val binder: PGBinder[Rdf, Course] =
      pgbWithId[Course](course =>
        makeUri(Course.generateUri(course)))(label, description, abbreviation, lecturer, semesterIndex, id)(Course.apply, Course.unapply) withClasses compositeClassUris.classUris
  }

  implicit lazy val CourseAtomDescriptor: Descriptor[Rdf, CourseAtom] = new Descriptor[Rdf, CourseAtom] {
    override val clazz: Rdf#URI = lwm.Course

    override val compositeClassUris: CompositeClassUris[Rdf, CourseAtom] =
      compositeClass[Rdf, CourseAtom](clazz) #>
        EmployeeDescriptor.compositeClassUris

    private val label = property[String](lwm.label)
    private val description = property[String](lwm.description)
    private val abbreviation = property[String](lwm.abbreviation)
    private val lecturer = property[Employee](lwm.lecturer)(EmployeeDescriptor.binder)
    private val semesterIndex = property[Int](lwm.semesterIndex)

    override val binder: PGBinder[Rdf, CourseAtom] =
      pgbWithId[CourseAtom](atom =>
        makeUri(Course.generateUri(atom.id)))(label, description, abbreviation, lecturer, semesterIndex, id)(CourseAtom.apply, CourseAtom.unapply) withClasses compositeClassUris.classUris
  }

  implicit lazy val DegreeDescriptor: Descriptor[Rdf, Degree] = new Descriptor[Rdf, Degree] {
    override val clazz: Rdf#URI = lwm.Degree

    override val compositeClassUris: CompositeClassUris[Rdf, Degree] = compositeClass[Rdf, Degree](clazz)

    private val label = property[String](lwm.label)
    private val abbreviation = property[String](lwm.abbreviation)

    override val binder: PGBinder[Rdf, Degree] =
      pgbWithId[Degree](degree => makeUri(Degree.generateUri(degree)))(label, abbreviation, id)(Degree.apply, Degree.unapply) withClasses compositeClassUris.classUris
  }

  implicit lazy val GroupDescriptor: Descriptor[Rdf, Group] = new Descriptor[Rdf, Group] {
    override val clazz: Rdf#URI = lwm.Group

    override val compositeClassUris: CompositeClassUris[Rdf, Group] = compositeClass[Rdf, Group](clazz)

    private val label = property[String](lwm.label)
    private val labwork = property[UUID](lwm.labwork)(uuidRefBinder(Labwork.splitter))
    private val members = set[UUID](lwm.members)(uuidRefBinder(User.splitter))

    override val binder: PGBinder[Rdf, Group] =
      pgbWithId[Group](group =>
        makeUri(Group.generateUri(group)))(label, labwork, members, id)(Group.apply, Group.unapply) withClasses compositeClassUris.classUris
  }


  implicit lazy val GroupAtomDescriptor: Descriptor[Rdf, GroupAtom] = new Descriptor[Rdf, GroupAtom] {
    override val clazz: Rdf#URI = lwm.Group

    override val compositeClassUris: CompositeClassUris[Rdf, GroupAtom] =
      compositeClass[Rdf, GroupAtom](clazz) #>
        LabworkDescriptor.compositeClassUris #>
        StudentDescriptor.compositeClassUris

    private val label = property[String](lwm.label)
    private val labwork = property[Labwork](lwm.labwork)(LabworkDescriptor.binder)
    private val members = set[Student](lwm.members)(StudentDescriptor.binder)

    override val binder: PGBinder[Rdf, GroupAtom] =
      pgbWithId[GroupAtom](group =>
        makeUri(Group.generateUri(group.id)))(label, labwork, members, id)(GroupAtom.apply, GroupAtom.unapply) withClasses compositeClassUris.classUris
  }

  implicit lazy val RoomDescriptor: Descriptor[Rdf, Room] = new Descriptor[Rdf, Room] {
    override val clazz: Rdf#URI = lwm.Room

    override val compositeClassUris: CompositeClassUris[Rdf, Room] = compositeClass[Rdf, Room](clazz)

    private val label = property[String](lwm.label)
    private val description = property[String](lwm.description)

    override val binder: PGBinder[Rdf, Room] =
      pgbWithId[Room](room =>
        makeUri(Room.generateUri(room)))(label, description, id)(Room.apply, Room.unapply) withClasses compositeClassUris.classUris
  }

  implicit lazy val SemesterDescriptor: Descriptor[Rdf, Semester] = new Descriptor[Rdf, Semester] {
    override val clazz: Rdf#URI = lwm.Semester

    override val compositeClassUris: CompositeClassUris[Rdf, Semester] = compositeClass[Rdf, Semester](clazz)

    private val label = property[String](lwm.label)
    private val abbreviation = property[String](lwm.abbreviation)
    private val start = property[LocalDate](lwm.start)
    private val end = property[LocalDate](lwm.end)
    private val examStart = property[LocalDate](lwm.examStart)

    override val binder: PGBinder[Rdf, Semester] =
      pgbWithId[Semester](semester =>
        makeUri(Semester.generateUri(semester)))(label, abbreviation, start, end, examStart, id)(Semester.apply, Semester.unapply) withClasses compositeClassUris.classUris
  }

  implicit lazy val TimetableDescriptor: Descriptor[Rdf, Timetable] = new Descriptor[Rdf, Timetable] {
    override val clazz: Rdf#URI = lwm.Timetable

    override val compositeClassUris: CompositeClassUris[Rdf, Timetable] =
      compositeClass[Rdf, Timetable](clazz) #>
        TimetableEntryDescriptor.compositeClassUris

    private val labwork = property[UUID](lwm.labwork)(uuidRefBinder(Labwork.splitter))
    private val entries = set[TimetableEntry](lwm.entries)(TimetableEntryDescriptor.binder)
    private val start = property[LocalDate](lwm.start)
    private val blacklist = set[DateTime](lwm.blacklist)

    override val binder: PGBinder[Rdf, Timetable] =
      pgbWithId[Timetable](timetable =>
        makeUri(Timetable.generateUri(timetable)))(labwork, entries, start, blacklist, id)(Timetable.apply, Timetable.unapply) withClasses compositeClassUris.classUris
  }


  implicit lazy val TimetableAtomDescriptor: Descriptor[Rdf, TimetableAtom] = new Descriptor[Rdf, TimetableAtom] {
    override val clazz: Rdf#URI = lwm.Timetable

    override val compositeClassUris: CompositeClassUris[Rdf, TimetableAtom] =
      compositeClass[Rdf, TimetableAtom](clazz) #>
        TimetableEntryAtomDescriptor.compositeClassUris #>
        LabworkDescriptor.compositeClassUris

    private val labwork = property[Labwork](lwm.labwork)(LabworkDescriptor.binder)
    private val entries = set[TimetableEntryAtom](lwm.entries)(TimetableEntryAtomDescriptor.binder)
    private val start = property[LocalDate](lwm.start)
    private val blacklist = set[DateTime](lwm.blacklist)

    override val binder: PGBinder[Rdf, TimetableAtom] =
      pgbWithId[TimetableAtom](timetable =>
        makeUri(Timetable.generateUri(timetable.id)))(labwork, entries, start, blacklist, id)(TimetableAtom.apply, TimetableAtom.unapply) withClasses compositeClassUris.classUris

  }

  implicit lazy val TimetableEntryDescriptor: Descriptor[Rdf, TimetableEntry] = new Descriptor[Rdf, TimetableEntry] {
    override val clazz: Rdf#URI = lwm.TimetableEntry

    override val compositeClassUris: CompositeClassUris[Rdf, TimetableEntry] = compositeClass[Rdf, TimetableEntry](clazz)

    private val supervisor = property[UUID](lwm.supervisor)(uuidRefBinder(User.splitter))
    private val room = property[UUID](lwm.room)(uuidRefBinder(Room.splitter))
    private val degree = property[UUID](lwm.degree)(uuidRefBinder(Degree.splitter))
    private val dayIndex = property[Int](lwm.dayIndex)
    private val start = property[LocalTime](lwm.start)
    private val end = property[LocalTime](lwm.end)

    override val binder: PGBinder[Rdf, TimetableEntry] =
      pgbWithId[TimetableEntry](
        _ => innerUri)(supervisor, room, degree, dayIndex, start, end)(TimetableEntry.apply, TimetableEntry.unapply) withClasses compositeClassUris.classUris
  }

  implicit lazy val TimetableEntryAtomDescriptor: Descriptor[Rdf, TimetableEntryAtom] = new Descriptor[Rdf, TimetableEntryAtom] {
    override val clazz: Rdf#URI = lwm.TimetableEntry

    override val compositeClassUris: CompositeClassUris[Rdf, TimetableEntryAtom] =
      compositeClass[Rdf, TimetableEntryAtom](clazz) #>
        EmployeeDescriptor.compositeClassUris #>
        RoomDescriptor.compositeClassUris #>
        DegreeDescriptor.compositeClassUris

    private val supervisor = property[Employee](lwm.supervisor)(EmployeeDescriptor.binder)
    private val room = property[Room](lwm.room)(RoomDescriptor.binder)
    private val degree = property[Degree](lwm.degree)(DegreeDescriptor.binder)
    private val dayIndex = property[Int](lwm.dayIndex)
    private val start = property[LocalTime](lwm.start)
    private val end = property[LocalTime](lwm.end)

    override val binder: PGBinder[Rdf, TimetableEntryAtom] =
      pgbWithId[TimetableEntryAtom](
        _ => innerUri)(supervisor, room, degree, dayIndex, start, end)(TimetableEntryAtom.apply, TimetableEntryAtom.unapply) withClasses compositeClassUris.classUris
  }

  implicit lazy val ScheduleDescriptor: Descriptor[Rdf, Schedule] = new Descriptor[Rdf, Schedule] {
    override val clazz: Rdf#URI = lwm.Schedule

    override val compositeClassUris: CompositeClassUris[Rdf, Schedule] =
      compositeClass[Rdf, Schedule](clazz) #>
        ScheduleEntryDescriptor.compositeClassUris

    private val labwork = property[UUID](lwm.labwork)(uuidRefBinder(Labwork.splitter))
    private val entries = set[ScheduleEntry](lwm.entries)(ScheduleEntryDescriptor.binder)

    override val binder: PGBinder[Rdf, Schedule] =
      pgbWithId[Schedule](schedule =>
        makeUri(Schedule.generateUri(schedule)))(labwork, entries, id)(Schedule.apply, Schedule.unapply) withClasses compositeClassUris.classUris
  }

  implicit lazy val ScheduleAtomDescriptor: Descriptor[Rdf, ScheduleAtom] = new Descriptor[Rdf, ScheduleAtom] {
    override val clazz: Rdf#URI = lwm.Schedule

    override val compositeClassUris: CompositeClassUris[Rdf, ScheduleAtom] =
      compositeClass[Rdf, ScheduleAtom](clazz) #>
        LabworkDescriptor.compositeClassUris #>
        ScheduleEntryAtomDescriptor.compositeClassUris

    private val labwork = property[Labwork](lwm.labwork)(LabworkDescriptor.binder)
    private val entries = set[ScheduleEntryAtom](lwm.entries)(ScheduleEntryAtomDescriptor.binder)

    override val binder: PGBinder[Rdf, ScheduleAtom] =
      pgbWithId[ScheduleAtom](schedule =>
        makeUri(Schedule.generateUri(schedule.id)))(labwork, entries, id)(ScheduleAtom.apply, ScheduleAtom.unapply) withClasses compositeClassUris.classUris
  }

  implicit lazy val ScheduleEntryDescriptor: Descriptor[Rdf, ScheduleEntry] = new Descriptor[Rdf, ScheduleEntry] {
    override val clazz: Rdf#URI = lwm.ScheduleEntry

    override val compositeClassUris: CompositeClassUris[Rdf, ScheduleEntry] = compositeClass[Rdf, ScheduleEntry](clazz)

    private val labwork = property[UUID](lwm.labwork)(uuidRefBinder(Labwork.splitter))
    private val start = property[LocalTime](lwm.start)
    private val end = property[LocalTime](lwm.end)
    private val date = property[LocalDate](lwm.date)
    private val room = property[UUID](lwm.room)(uuidRefBinder(Room.splitter))
    private val supervisor = property[UUID](lwm.supervisor)(uuidRefBinder(User.splitter))
    private val group = property[UUID](lwm.group)(uuidRefBinder(Group.splitter))

    override val binder: PGBinder[Rdf, ScheduleEntry] =
      pgbWithId[ScheduleEntry](sentry =>
        makeUri(ScheduleEntry.generateUri(sentry)))(labwork, start, end, date, room, supervisor, group, id)(ScheduleEntry.apply, ScheduleEntry.unapply) withClasses compositeClassUris.classUris

  }

  implicit lazy val ScheduleEntryAtomDescriptor: Descriptor[Rdf, ScheduleEntryAtom] = new Descriptor[Rdf, ScheduleEntryAtom] {
    override val clazz: Rdf#URI = lwm.ScheduleEntry

    override val compositeClassUris: CompositeClassUris[Rdf, ScheduleEntryAtom] =
      compositeClass[Rdf, ScheduleEntryAtom](clazz) #>
        LabworkDescriptor.compositeClassUris #>
        RoomDescriptor.compositeClassUris #>
        EmployeeDescriptor.compositeClassUris #>
        GroupDescriptor.compositeClassUris

    private val labwork = property[Labwork](lwm.labwork)(LabworkDescriptor.binder)
    private val room = property[Room](lwm.room)(RoomDescriptor.binder)
    private val supervisor = property[Employee](lwm.supervisor)(EmployeeDescriptor.binder)
    private val group = property[Group](lwm.group)(GroupDescriptor.binder)
    private val start = property[LocalTime](lwm.start)
    private val end = property[LocalTime](lwm.end)
    private val date = property[LocalDate](lwm.date)

    override val binder: PGBinder[Rdf, ScheduleEntryAtom] =
      pgbWithId[ScheduleEntryAtom](sentry =>
        makeUri(ScheduleEntry.generateUri(sentry.id)))(labwork, start, end, date, room, supervisor, group, id)(ScheduleEntryAtom.apply, ScheduleEntryAtom.unapply) withClasses compositeClassUris.classUris

  }

  implicit lazy val BlacklistDescriptor: Descriptor[Rdf, Blacklist] = new Descriptor[Rdf, Blacklist] {
    override val clazz: Rdf#URI = lwm.Blacklist

    override val compositeClassUris: CompositeClassUris[Rdf, Blacklist] = compositeClass[Rdf, Blacklist](clazz)

    private val label = property[String](lwm.label)
    private val dates = set[DateTime](lwm.dates)

    override val binder: PGBinder[Rdf, Blacklist] =
      pgbWithId[Blacklist](blacklist => makeUri(Blacklist.generateUri(blacklist)))(label, dates, id)(Blacklist.apply, Blacklist.unapply) withClasses compositeClassUris.classUris

  }

  implicit lazy val ReportCardEntryDescriptor: Descriptor[Rdf, ReportCardEntry] = new Descriptor[Rdf, ReportCardEntry] {
    override val clazz: Rdf#URI = lwm.ReportCardEntry

    override val compositeClassUris: CompositeClassUris[Rdf, ReportCardEntry] =
      compositeClass[Rdf, ReportCardEntry](clazz) #>
        ReportCardEntryTypeDescriptor.compositeClassUris #>
        RescheduledDescriptor.compositeClassUris

    private val student = property[UUID](lwm.student)(uuidRefBinder(User.splitter))
    private val labwork = property[UUID](lwm.labwork)(uuidRefBinder(Labwork.splitter))
    private val label = property[String](lwm.label)
    private val date = property[LocalDate](lwm.date)
    private val start = property[LocalTime](lwm.start)
    private val end = property[LocalTime](lwm.end)
    private val room = property[UUID](lwm.room)(uuidRefBinder(Room.splitter))
    private val rescheduled = optional[Rescheduled](lwm.rescheduled)(RescheduledDescriptor.binder)
    private val types = set[ReportCardEntryType](lwm.entryTypes)(ReportCardEntryTypeDescriptor.binder)

    override val binder: PGBinder[Rdf, ReportCardEntry] =
      pgbWithId[ReportCardEntry](reportCardEntry =>
        makeUri(ReportCardEntry.generateUri(reportCardEntry)))(student, labwork, label, date, start, end, room, types, rescheduled, id)(ReportCardEntry.apply, ReportCardEntry.unapply) withClasses compositeClassUris.classUris

  }

  implicit lazy val ReportCardEntryAtomDescriptor: Descriptor[Rdf, ReportCardEntryAtom] = new Descriptor[Rdf, ReportCardEntryAtom] {
    override val clazz: Rdf#URI = lwm.ReportCardEntry

    override val compositeClassUris: CompositeClassUris[Rdf, ReportCardEntryAtom] =
      compositeClass[Rdf, ReportCardEntryAtom](clazz) #>
        StudentDescriptor.compositeClassUris #>
        ReportCardEntryTypeDescriptor.compositeClassUris #>
        RescheduledAtomDescriptor.compositeClassUris #>
        LabworkDescriptor.compositeClassUris #>
        RoomDescriptor.compositeClassUris

    private val student = property[Student](lwm.student)(StudentDescriptor.binder)
    private val labwork = property[Labwork](lwm.labwork)(LabworkDescriptor.binder)
    private val room = property[Room](lwm.room)(RoomDescriptor.binder)
    private val rescheduled = optional[RescheduledAtom](lwm.rescheduled)(RescheduledAtomDescriptor.binder)
    private val label = property[String](lwm.label)
    private val date = property[LocalDate](lwm.date)
    private val start = property[LocalTime](lwm.start)
    private val end = property[LocalTime](lwm.end)
    private val types = set[ReportCardEntryType](lwm.entryTypes)(ReportCardEntryTypeDescriptor.binder)

    override val binder: PGBinder[Rdf, ReportCardEntryAtom] =
      pgbWithId[ReportCardEntryAtom](reportCardEntry =>
        makeUri(ReportCardEntry.generateUri(reportCardEntry.id)))(student, labwork, label, date, start, end, room, types, rescheduled, id)(ReportCardEntryAtom.apply, ReportCardEntryAtom.unapply) withClasses compositeClassUris.classUris

  }

  implicit lazy val ReportCardEntryTypeDescriptor: Descriptor[Rdf, ReportCardEntryType] = new Descriptor[Rdf, ReportCardEntryType] {
    override val clazz: Rdf#URI = lwm.ReportCardEntryType

    override val compositeClassUris: CompositeClassUris[Rdf, ReportCardEntryType] = compositeClass[Rdf, ReportCardEntryType](clazz)

    private val entryType = property[String](lwm.entryType)
    private val bool = property[Boolean](lwm.bool)
    private val int = property[Int](lwm.int)

    override val binder: PGBinder[Rdf, ReportCardEntryType] =
      pgbWithId[ReportCardEntryType](reportCardEntryType =>
        makeUri(ReportCardEntryType.generateUri(reportCardEntryType)))(entryType, bool, int, id)(ReportCardEntryType.apply, ReportCardEntryType.unapply) withClasses compositeClassUris.classUris
  }

  implicit lazy val RescheduledDescriptor: Descriptor[Rdf, Rescheduled] = new Descriptor[Rdf, Rescheduled] {
    override val clazz: Rdf#URI = lwm.Rescheduled

    override val compositeClassUris: CompositeClassUris[Rdf, Rescheduled] = compositeClass[Rdf, Rescheduled](clazz)

    private val date = property[LocalDate](lwm.date)
    private val start = property[LocalTime](lwm.start)
    private val end = property[LocalTime](lwm.end)
    private val room = property[UUID](lwm.room)(uuidRefBinder(Room.splitter))

    override val binder: PGBinder[Rdf, Rescheduled] =
      pgbWithId[Rescheduled](
        _ => innerUri)(date, start, end, room)(Rescheduled.apply, Rescheduled.unapply) withClasses compositeClassUris.classUris
  }

  implicit lazy val RescheduledAtomDescriptor: Descriptor[Rdf, RescheduledAtom] = new Descriptor[Rdf, RescheduledAtom] {
    override val clazz: Rdf#URI = lwm.Rescheduled

    override val compositeClassUris: CompositeClassUris[Rdf, RescheduledAtom] =
      compositeClass[Rdf, RescheduledAtom](clazz) #>
        RoomDescriptor.compositeClassUris

    private val date = property[LocalDate](lwm.date)
    private val start = property[LocalTime](lwm.start)
    private val end = property[LocalTime](lwm.end)
    private val room = property[Room](lwm.room)(RoomDescriptor.binder)

    override val binder: PGBinder[Rdf, RescheduledAtom] =
      pgbWithId[RescheduledAtom](
        _ => innerUri)(date, start, end, room)(RescheduledAtom.apply, RescheduledAtom.unapply) withClasses compositeClassUris.classUris
  }

  implicit lazy val AnnotationDescriptor: Descriptor[Rdf, Annotation] = new Descriptor[Rdf, Annotation] {
    override val clazz: Rdf#URI = lwm.Annotation

    override val compositeClassUris: CompositeClassUris[Rdf, Annotation] = compositeClass[Rdf, Annotation](clazz)

    private val student = property[UUID](lwm.student)(uuidRefBinder(User.splitter))
    private val labwork = property[UUID](lwm.labwork)(uuidRefBinder(Labwork.splitter))
    private val reportCardEntry = property[UUID](lwm.reportCardEntry)(uuidRefBinder(ReportCardEntry.splitter))
    private val message = property[String](lwm.message)
    private val timestamp = property[DateTime](lwm.timestamp)

    override val binder: PGBinder[Rdf, Annotation] =
      pgbWithId[Annotation](annotation =>
        makeUri(Annotation.generateUri(annotation)))(student, labwork, reportCardEntry, message, timestamp, id)(Annotation.apply, Annotation.unapply) withClasses compositeClassUris.classUris

  }

  implicit lazy val AnnotationAtomDescriptor: Descriptor[Rdf, AnnotationAtom] = new Descriptor[Rdf, AnnotationAtom] {
    override val clazz: Rdf#URI = lwm.Annotation

    override val compositeClassUris: CompositeClassUris[Rdf, AnnotationAtom] =
      compositeClass[Rdf, AnnotationAtom](clazz) #>
        StudentDescriptor.compositeClassUris #>
        LabworkDescriptor.compositeClassUris #>
        ReportCardEntryDescriptor.compositeClassUris

    private val student = property[Student](lwm.student)(StudentDescriptor.binder)
    private val labwork = property[Labwork](lwm.labwork)(LabworkDescriptor.binder)
    private val reportCardEntry = property[ReportCardEntry](lwm.reportCardEntry)(ReportCardEntryDescriptor.binder)
    private val message = property[String](lwm.message)
    private val timestamp = property[DateTime](lwm.timestamp)

    override val binder: PGBinder[Rdf, AnnotationAtom] =
      pgbWithId[AnnotationAtom](annotation =>
        makeUri(Annotation.generateUri(annotation.id)))(student, labwork, reportCardEntry, message, timestamp, id)(AnnotationAtom.apply, AnnotationAtom.unapply) withClasses compositeClassUris.classUris

  }

  implicit lazy val ReportCardEvaluationDescriptor: Descriptor[Rdf, ReportCardEvaluation] = new Descriptor[Rdf, ReportCardEvaluation] {
    override val clazz: Rdf#URI = lwm.ReportCardEvaluation

    override val compositeClassUris: CompositeClassUris[Rdf, ReportCardEvaluation] = compositeClass[Rdf, ReportCardEvaluation](clazz)

    private val student = property[UUID](lwm.student)(uuidRefBinder(User.splitter))
    private val labwork = property[UUID](lwm.labwork)(uuidRefBinder(Labwork.splitter))
    private val label = property[String](lwm.label)
    private val bool = property[Boolean](lwm.bool)
    private val int = property[Int](lwm.int)

    override val binder: PGBinder[Rdf, ReportCardEvaluation] =
      pgbWithId[ReportCardEvaluation](eval =>
        makeUri(ReportCardEvaluation.generateUri(eval)))(student, labwork, label, bool, int, id)(ReportCardEvaluation.apply, ReportCardEvaluation.unapply) withClasses compositeClassUris.classUris

  }

  implicit lazy val ReportCardEvaluationAtomDescriptor: Descriptor[Rdf, ReportCardEvaluationAtom] = new Descriptor[Rdf, ReportCardEvaluationAtom] {
    override val clazz: Rdf#URI = lwm.ReportCardEvaluation

    override val compositeClassUris: CompositeClassUris[Rdf, ReportCardEvaluationAtom] =
      compositeClass[Rdf, ReportCardEvaluationAtom](clazz) #>
        StudentDescriptor.compositeClassUris #>
        LabworkDescriptor.compositeClassUris

    private val student = property[Student](lwm.student)(StudentDescriptor.binder)
    private val labwork = property[Labwork](lwm.labwork)(LabworkDescriptor.binder)
    private val label = property[String](lwm.label)
    private val bool = property[Boolean](lwm.bool)
    private val int = property[Int](lwm.int)

    override val binder: PGBinder[Rdf, ReportCardEvaluationAtom] =
      pgbWithId[ReportCardEvaluationAtom](eval =>
        makeUri(ReportCardEvaluation.generateUri(eval.id)))(student, labwork, label, bool, int, id)(ReportCardEvaluationAtom.apply, ReportCardEvaluationAtom.unapply) withClasses compositeClassUris.classUris

  }

}

object Bindings {
  def apply[Rdf <: RDF](base: Namespace)(implicit ops: RDFOps[Rdf], recordBinder: RecordBinder[Rdf]) = new Bindings[Rdf]()(base, ops, recordBinder)
}