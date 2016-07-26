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

object PropertyEnhancer {

  def enhance[Rdf <: RDF, T](pgb: RecordBinder[Rdf]#PGB[T])(implicit ops: RDFOps[Rdf]) = new Records[Rdf, T](pgb)

  class Records[Rdf <: RDF, T](pgb: RecordBinder[Rdf]#PGB[T])(implicit ops: RDFOps[Rdf]) {
    def apply[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11](p1: Property[Rdf, T1], p2: Property[Rdf, T2], p3: Property[Rdf, T3], p4: Property[Rdf, T4], p5: Property[Rdf, T5], p6: Property[Rdf, T6], p7: Property[Rdf, T7], p8: Property[Rdf, T8], p9: Property[Rdf, T9], p10: Property[Rdf, T10], p11: Property[Rdf, T11])(apply: (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11) => T, unapply: T => Option[(T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11)]): PGBinder[Rdf, T] = new PGBinder[Rdf, T] {

      def toPG(t: T): PointedGraph[Rdf] = {
        val Some((t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11)) = unapply(t)
        pgb.make(t, p1.pos(t1), p2.pos(t2), p3.pos(t3), p4.pos(t4), p5.pos(t5), p6.pos(t6), p7.pos(t7), p8.pos(t8), p9.pos(t9), p10.pos(t10), p11.pos(t11))
      }

      def fromPG(pointed: PointedGraph[Rdf]): Try[T] = {
        def v1 = p1.extract(pointed)
        def v2 = p2.extract(pointed)
        def v3 = p3.extract(pointed)
        def v4 = p4.extract(pointed)
        def v5 = p5.extract(pointed)
        def v6 = p6.extract(pointed)
        def v7 = p7.extract(pointed)
        def v8 = p8.extract(pointed)
        def v9 = p9.extract(pointed)
        def v10 = p10.extract(pointed)
        def v11 = p11.extract(pointed)
        for (t1 <- v1; t2 <- v2; t3 <- v3; t4 <- v4; t5 <- v5; t6 <- v6; t7 <- v7; t8 <- v8; t9 <- v9; t10 <- v10; t11 <- v11) yield apply(t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11)
      }
    }
  }

}

object Descriptor {

  final case class Ref[+A](root: A, references: List[Ref[A]] = Nil, referenced: List[Ref[A]] = Nil) {
    def pointsAt[AA >: A](that: Ref[AA]): Ref[AA] = Ref(root, that :: references, referenced)

    def pointedAt[AA >: A](by: Ref[AA]): Ref[AA] = Ref(root, references, by :: referenced)

    def pointsAt[AA >: A](that: AA): Ref[AA] = pointsAt(Ref(that))

    def pointedAt[AA >: A](that: AA): Ref[AA] = pointedAt(Ref(that))

    def zipWith[AA >: A, B](refs: List[Ref[AA]])(f: AA => List[B]): List[(B, Ref[AA])] = refs flatMap { ref => f(ref.root) map ((_, ref)) }

    def deref[B](b: B)(f: (B, A) => List[B])(g: (B, A) => List[B]): List[B] = {
      @annotation.tailrec
      def go(values: List[(B, Ref[A])], rem: List[(B, Ref[A])]): List[B] = rem match {
        case (cb, ref) :: tail =>
          val rs = zipWith(ref.references)(a => f(cb, a))
          val cors = zipWith(ref.referenced)(a => g(cb, a))
          val tail2 = rs ++ cors
          go(((cb, ref) :: tail2) ++ values, tail ++ tail2)
        case Nil => values map (_._1)
      }
      go(Nil, List((b, this)))
    }

    def leftDeref[B](b: B)(f: (B, A) => List[B]): List[B] = deref(b)(f)((_, _) => Nil)

    def rightDeref[B](b: B)(f: (B, A) => List[B]): List[B] = deref(b)((_, _) => Nil)(f)
  }

  trait Descriptor[Rdf <: RDF, T] {
    def clazz: Rdf#URI

    def classUris: ClassUrisFor[Rdf, T]

    def references: Ref[Rdf#URI] = Ref(clazz)

    def branching: Ref[Rdf#URI] = Ref(clazz)

    def binder: PGBinder[Rdf, T]
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
  val invalidated = optional[DateTime](lwm.invalidated)

  implicit lazy val UserDescriptor: Descriptor[Rdf, User] = new Descriptor[Rdf, User] {
    override val clazz: Rdf#URI = lwm.User

    override val classUris: ClassUrisFor[Rdf, User] = classUrisFor[User](clazz)


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

    override val classUris: ClassUrisFor[Rdf, Student] = classUrisFor[Student](clazz)

    private val lastname = property[String](lwm.lastname)
    private val firstname = property[String](lwm.firstname)
    private val registrationId = property[String](lwm.registrationId)
    private val systemId = property[String](lwm.systemId)
    private val enrollment = property[UUID](lwm.enrollment)(uuidRefBinder(Degree.splitter))
    private val email = property[String](lwm.email)

    override val binder: PGBinder[Rdf, Student] =
      pgbWithId[Student](student =>
        makeUri(User.generateUri(student)))(systemId, lastname, firstname, email, registrationId, enrollment, invalidated, id)(Student.apply, Student.unapply) withClasses classUris
  }

  implicit lazy val StudentAtomDescriptor: Descriptor[Rdf, StudentAtom] = new Descriptor[Rdf, StudentAtom] {
    override val clazz: Rdf#URI = lwm.User

    override val classUris: ClassUrisFor[Rdf, StudentAtom] = classUrisFor[StudentAtom](clazz)

    override val references: Ref[Rdf#URI] = Ref(clazz) pointsAt DegreeDescriptor.references

    private val lastname = property[String](lwm.lastname)
    private val firstname = property[String](lwm.firstname)
    private val registrationId = property[String](lwm.registrationId)
    private val enrollment = property[Degree](lwm.enrollment)(DegreeDescriptor.binder)
    private val systemId = property[String](lwm.systemId)
    private val email = property[String](lwm.email)

    override val binder: PGBinder[Rdf, StudentAtom] =
      pgbWithId[StudentAtom](student =>
        makeUri(User.generateUri(student.id)))(systemId, lastname, firstname, email, registrationId, enrollment, invalidated, id)(StudentAtom.apply, StudentAtom.unapply) withClasses classUris
  }

  implicit lazy val EmployeeDescriptor: Descriptor[Rdf, Employee] = new Descriptor[Rdf, Employee] {
    override val clazz: Rdf#URI = lwm.User

    override val classUris: ClassUrisFor[Rdf, Employee] = classUrisFor[Employee](clazz)

    private val lastname = property[String](lwm.lastname)
    private val firstname = property[String](lwm.firstname)
    private val systemId = property[String](lwm.systemId)
    private val email = property[String](lwm.email)
    private val status = property[String](lwm.status)

    override val binder: PGBinder[Rdf, Employee] =
      pgbWithId[Employee](employee =>
        makeUri(User.generateUri(employee)))(systemId, lastname, firstname, email, status, invalidated, id)(Employee.apply, Employee.unapply) withClasses classUris

  }

  implicit lazy val LabworkApplicationDescriptor: Descriptor[Rdf, LabworkApplication] = new Descriptor[Rdf, LabworkApplication] {
    override val clazz: Rdf#URI = lwm.LabworkApplication

    override val classUris: ClassUrisFor[Rdf, LabworkApplication] = classUrisFor[LabworkApplication](clazz)

    private val labwork = property[UUID](lwm.labwork)(uuidRefBinder(Labwork.splitter))
    private val applicant = property[UUID](lwm.applicant)(uuidRefBinder(User.splitter))
    private val timestamp = property[DateTime](lwm.timestamp)
    private val friends = set[UUID](lwm.friends)(uuidRefBinder(User.splitter))

    override val binder: PGBinder[Rdf, LabworkApplication] =
      pgbWithId[LabworkApplication](application =>
        makeUri(LabworkApplication.generateUri(application)))(labwork, applicant, friends, timestamp, invalidated, id)(LabworkApplication.apply, LabworkApplication.unapply) withClasses classUris
  }

  implicit lazy val LabworkApplicationAtomDescriptor: Descriptor[Rdf, LabworkApplicationAtom] = new Descriptor[Rdf, LabworkApplicationAtom] {
    override val clazz: Rdf#URI = lwm.LabworkApplication

    override val classUris: ClassUrisFor[Rdf, LabworkApplicationAtom] = classUrisFor[LabworkApplicationAtom](clazz)

    override val references: Ref[Rdf#URI] =
      Ref(clazz)
        .pointsAt(LabworkDescriptor.references)
        .pointsAt(StudentDescriptor.references)

    private val labwork = property[Labwork](lwm.labwork)(LabworkDescriptor.binder)
    private val applicant = property[Student](lwm.applicant)(StudentDescriptor.binder)
    private val friends = set[Student](lwm.friends)(StudentDescriptor.binder)
    private val timestamp = property[DateTime](lwm.timestamp)

    override val binder: PGBinder[Rdf, LabworkApplicationAtom] =
      pgbWithId[LabworkApplicationAtom](application =>
        makeUri(LabworkApplication.generateUri(application.id)))(labwork, applicant, friends, timestamp, invalidated, id)(LabworkApplicationAtom.apply, LabworkApplicationAtom.unapply) withClasses classUris

  }

  implicit lazy val RoleDescriptor: Descriptor[Rdf, Role] = new Descriptor[Rdf, Role] {
    override val clazz: Rdf#URI = lwm.Role

    override val classUris: ClassUrisFor[Rdf, Role] = classUrisFor[Role](clazz)

    private val label = property[String](lwm.label)
    private val permissions = set[Permission](lwm.permissions)

    override val binder: PGBinder[Rdf, Role] =
      pgbWithId[Role](role =>
        makeUri(Role.generateUri(role)))(label, permissions, invalidated, id)(Role.apply, Role.unapply) withClasses classUris

  }

  // TODO KILL
  implicit lazy val RefRoleDescriptor: Descriptor[Rdf, RefRole] = new Descriptor[Rdf, RefRole] {
    override val clazz: Rdf#URI = lwm.RefRole

    override val classUris: ClassUrisFor[Rdf, RefRole] = classUrisFor[RefRole](clazz)

    private val course = optional[UUID](lwm.course)(uuidRefBinder(Course.splitter))
    private val role = property[UUID](lwm.role)(uuidRefBinder(Role.splitter))

    override val binder: PGBinder[Rdf, RefRole] =
      pgbWithId[RefRole](refRole =>
        makeUri(RefRole.generateUri(refRole)))(course, role, invalidated, id)(RefRole.apply, RefRole.unapply) withClasses classUris
  }

  // TODO KILL
  implicit lazy val RefRoleAtomDescriptor: Descriptor[Rdf, RefRoleAtom] = new Descriptor[Rdf, RefRoleAtom] {
    override val clazz: Rdf#URI = lwm.RefRole

    override val classUris: ClassUrisFor[Rdf, RefRoleAtom] = classUrisFor[RefRoleAtom](clazz)

    override val references: Ref[Rdf#URI] =
      Ref(clazz)
        .pointsAt(CourseAtomDescriptor.references)
        .pointsAt(RoleDescriptor.references)

    private val course = optional[CourseAtom](lwm.course)(CourseAtomDescriptor.binder)
    private val role = property[Role](lwm.role)(RoleDescriptor.binder)

    override val binder: PGBinder[Rdf, RefRoleAtom] =
      pgbWithId[RefRoleAtom](refRole =>
        makeUri(RefRole.generateUri(refRole.id)))(course, role, invalidated, id)(RefRoleAtom.apply, RefRoleAtom.unapply) withClasses classUris
  }

  // TODO KILL
  implicit lazy val AuthorityDescriptor: Descriptor[Rdf, Authority] = new Descriptor[Rdf, Authority] {
    override val clazz: Rdf#URI = lwm.Authority

    override val classUris: ClassUrisFor[Rdf, Authority] = classUrisFor[Authority](clazz)

    private val privileged = property[UUID](lwm.privileged)(uuidRefBinder(User.splitter))
    private val refroles = set[UUID](lwm.refroles)(uuidRefBinder(RefRole.splitter))

    override val binder: PGBinder[Rdf, Authority] =
      pgbWithId[Authority](auth =>
        makeUri(Authority.generateUri(auth)))(privileged, refroles, invalidated, id)(Authority.apply, Authority.unapply) withClasses classUris
  }

  // TOOD KILL
  implicit lazy val AuthorityAtomDescriptor: Descriptor[Rdf, AuthorityAtom] = new Descriptor[Rdf, AuthorityAtom] {
    override val clazz: Rdf#URI = lwm.Authority

    override val classUris: ClassUrisFor[Rdf, AuthorityAtom] = classUrisFor[AuthorityAtom](clazz)

    override val references: Ref[Rdf#URI] =
      Ref(clazz)
        .pointsAt(UserDescriptor.references)
        .pointsAt(RefRoleAtomDescriptor.references)

    private val privileged = property[User](lwm.privileged)(UserDescriptor.binder)
    private val refrole = set[RefRoleAtom](lwm.refroles)(RefRoleAtomDescriptor.binder)

    override val binder: PGBinder[Rdf, AuthorityAtom] =
      pgbWithId[AuthorityAtom](
        auth => makeUri(Authority.generateUri(auth.id)))(privileged, refrole, invalidated, id)(AuthorityAtom.apply, AuthorityAtom.unapply) withClasses classUris
  }

  implicit lazy val Authority2Descriptor: Descriptor[Rdf, Authority2] = new Descriptor[Rdf, Authority2] {
    override val clazz: Rdf#URI = lwm.Authority

    override val classUris: ClassUrisFor[Rdf, Authority2] = classUrisFor[Authority2](clazz)

    private val privileged = property[UUID](lwm.privileged)(uuidRefBinder(User.splitter))
    private val course = optional[UUID](lwm.course)(uuidRefBinder(Course.splitter))
    private val role = property[UUID](lwm.role)(uuidRefBinder(Role.splitter))

    override val binder: PGBinder[Rdf, Authority2] =
      pgbWithId[Authority2](auth =>
        makeUri(Authority2.generateUri(auth)))(privileged, role, course, invalidated, id)(Authority2.apply, Authority2.unapply) withClasses classUris
  }

  implicit lazy val AuthorityAtom2Descriptor: Descriptor[Rdf, AuthorityAtom2] = new Descriptor[Rdf, AuthorityAtom2] {
    override val clazz: Rdf#URI = lwm.Authority

    override val classUris: ClassUrisFor[Rdf, AuthorityAtom2] = classUrisFor[AuthorityAtom2](clazz)

    override val references: Ref[Rdf#URI] =
      Ref(clazz)
        .pointsAt(UserDescriptor.references)
        .pointsAt(CourseAtomDescriptor.references)
        .pointsAt(RoleDescriptor.references)

    private val privileged = property[User](lwm.privileged)(UserDescriptor.binder)
    private val course = optional[CourseAtom](lwm.course)(CourseAtomDescriptor.binder)
    private val role = property[Role](lwm.role)(RoleDescriptor.binder)

    override val binder: PGBinder[Rdf, AuthorityAtom2] =
      pgbWithId[AuthorityAtom2](
        auth => makeUri(Authority2.generateUri(auth.id)))(privileged, role, course, invalidated, id)(AuthorityAtom2.apply, AuthorityAtom2.unapply) withClasses classUris
  }

  implicit lazy val AssignmentEntryDescriptor: Descriptor[Rdf, AssignmentEntry] = new Descriptor[Rdf, AssignmentEntry] {
    override val clazz: Rdf#URI = lwm.AssignmentEntry

    override val classUris: ClassUrisFor[Rdf, AssignmentEntry] = classUrisFor[AssignmentEntry](clazz)

    override val references: Ref[Rdf#URI] = Ref(clazz) pointsAt AssignmentEntryTypeDescriptor.references

    override val branching: Ref[Rdf#URI] = references

    private val index = property[Int](lwm.index)
    private val label = property[String](lwm.label)
    private val duration = property[Int](lwm.duration)
    private val types = set[AssignmentEntryType](lwm.entryTypes)(AssignmentEntryTypeDescriptor.binder)

    override val binder: PGBinder[Rdf, AssignmentEntry] =
      pgbWithId[AssignmentEntry](_ => innerUri)(index, label, types, duration)(AssignmentEntry.apply, AssignmentEntry.unapply) withClasses classUris
  }

  implicit lazy val AssignmentEntryTypeDescriptor: Descriptor[Rdf, AssignmentEntryType] = new Descriptor[Rdf, AssignmentEntryType] {
    override val clazz: Rdf#URI = lwm.AssignmentEntryType

    override val classUris: ClassUrisFor[Rdf, AssignmentEntryType] = classUrisFor[AssignmentEntryType](clazz)

    private val entryType = property[String](lwm.entryType)
    private val bool = property[Boolean](lwm.bool)
    private val int = property[Int](lwm.int)

    override val binder: PGBinder[Rdf, AssignmentEntryType] =
      pgbWithId[AssignmentEntryType](_ => innerUri)(entryType, bool, int)(AssignmentEntryType.apply, AssignmentEntryType.unapply) withClasses classUris
  }


  implicit lazy val AssignmentPlanDescriptor: Descriptor[Rdf, AssignmentPlan] = new Descriptor[Rdf, AssignmentPlan] {
    override val clazz: Rdf#URI = lwm.AssignmentPlan

    override val classUris: ClassUrisFor[Rdf, AssignmentPlan] = classUrisFor[AssignmentPlan](clazz)

    override val references: Ref[Rdf#URI] = Ref(clazz) pointsAt AssignmentEntryDescriptor.references

    override val branching: Ref[Rdf#URI] = references

    private val labwork = property[UUID](lwm.labwork)(uuidRefBinder(Labwork.splitter))
    private val attendance = property[Int](lwm.attendance)
    private val mandatory = property[Int](lwm.mandatory)
    private val entries = set[AssignmentEntry](lwm.entries)(AssignmentEntryDescriptor.binder)

    override val binder: PGBinder[Rdf, AssignmentPlan] =
      pgbWithId[AssignmentPlan](aPlan =>
        makeUri(AssignmentPlan.generateUri(aPlan)))(labwork, attendance, mandatory, entries, invalidated, id)(AssignmentPlan.apply, AssignmentPlan.unapply) withClasses classUris
  }

  implicit lazy val AssignmentPlanAtomDescriptor: Descriptor[Rdf, AssignmentPlanAtom] = new Descriptor[Rdf, AssignmentPlanAtom] {
    override val clazz: Rdf#URI = lwm.AssignmentPlan

    override val classUris: ClassUrisFor[Rdf, AssignmentPlanAtom] = classUrisFor[AssignmentPlanAtom](clazz)

    override val references: Ref[Rdf#URI] =
      Ref(clazz)
        .pointsAt(LabworkDescriptor.references)
        .pointsAt(AssignmentEntryDescriptor.references)

    private val labwork = property[Labwork](lwm.labwork)(LabworkDescriptor.binder)
    private val attendance = property[Int](lwm.attendance)
    private val mandatory = property[Int](lwm.mandatory)
    private val entries = set[AssignmentEntry](lwm.entries)(AssignmentEntryDescriptor.binder)

    override val binder: PGBinder[Rdf, AssignmentPlanAtom] =
      pgbWithId[AssignmentPlanAtom](aPlan =>
        makeUri(AssignmentPlan.generateUri(aPlan.id)))(labwork, attendance, mandatory, entries, invalidated, id)(AssignmentPlanAtom.apply, AssignmentPlanAtom.unapply) withClasses classUris
  }

  implicit lazy val LabworkDescriptor: Descriptor[Rdf, Labwork] = new Descriptor[Rdf, Labwork] {
    override val clazz: Rdf#URI = lwm.Labwork

    override val classUris: ClassUrisFor[Rdf, Labwork] = classUrisFor[Labwork](clazz)

    override val branching: Ref[Rdf#URI] =
      Ref(clazz)
        .pointedAt(AssignmentPlanDescriptor.branching)
        .pointedAt(GroupDescriptor.branching)
        .pointedAt(ScheduleDescriptor.branching)
        .pointedAt(ReportCardEntryDescriptor.branching)
        .pointedAt(ReportCardEvaluationDescriptor.branching)
        .pointedAt(TimetableDescriptor.branching)
        .pointedAt(LabworkApplicationDescriptor.branching)
        .pointedAt(AnnotationDescriptor.branching)

    private val label = property[String](lwm.label)
    private val description = property[String](lwm.description)
    private val semester = property[UUID](lwm.semester)(uuidRefBinder(Semester.splitter))
    private val course = property[UUID](lwm.course)(uuidRefBinder(Course.splitter))
    private val degree = property[UUID](lwm.degree)(uuidRefBinder(Degree.splitter))
    private val subscribable = property[Boolean](lwm.subscribable)
    private val published = property[Boolean](lwm.published)

    override val binder: PGBinder[Rdf, Labwork] =
      pgbWithId[Labwork](labwork =>
        makeUri(Labwork.generateUri(labwork)))(label, description, semester, course, degree, subscribable, published, invalidated, id)(Labwork.apply, Labwork.unapply) withClasses classUris
  }

  implicit lazy val LabworkAtomDescriptor: Descriptor[Rdf, LabworkAtom] = new Descriptor[Rdf, LabworkAtom] {
    override val clazz: Rdf#URI = lwm.Labwork

    override val classUris: ClassUrisFor[Rdf, LabworkAtom] = classUrisFor[LabworkAtom](clazz)

    override val references: Ref[Rdf#URI] =
      Ref(clazz)
        .pointsAt(SemesterDescriptor.references)
        .pointsAt(CourseAtomDescriptor.references)
        .pointsAt(DegreeDescriptor.references)

    private val label = property[String](lwm.label)
    private val description = property[String](lwm.description)
    private val semester = property[Semester](lwm.semester)(SemesterDescriptor.binder)
    private val course = property[CourseAtom](lwm.course)(CourseAtomDescriptor.binder)
    private val degree = property[Degree](lwm.degree)(DegreeDescriptor.binder)
    private val subscribable = property[Boolean](lwm.subscribable)
    private val published = property[Boolean](lwm.published)

    override val binder: PGBinder[Rdf, LabworkAtom] =
      pgbWithId[LabworkAtom](
        atom => makeUri(Labwork.generateUri(atom.id)))(label, description, semester, course, degree, subscribable, published, invalidated, id)(LabworkAtom.apply, LabworkAtom.unapply) withClasses classUris

  }

  implicit lazy val CourseDescriptor: Descriptor[Rdf, Course] = new Descriptor[Rdf, Course] {
    override val clazz: Rdf#URI = lwm.Course

    override val classUris: ClassUrisFor[Rdf, Course] = classUrisFor[Course](clazz)

    override val branching: Ref[Rdf#URI] =
      Ref(clazz)
        .pointedAt(LabworkDescriptor.branching)
        .pointedAt(RefRoleDescriptor.branching)

    private val label = property[String](lwm.label)
    private val description = property[String](lwm.description)
    private val abbreviation = property[String](lwm.abbreviation)
    private val lecturer = property[UUID](lwm.lecturer)(uuidRefBinder(User.splitter))
    private val semesterIndex = property[Int](lwm.semesterIndex)

    override val binder: PGBinder[Rdf, Course] =
      pgbWithId[Course](course =>
        makeUri(Course.generateUri(course)))(label, description, abbreviation, lecturer, semesterIndex, invalidated, id)(Course.apply, Course.unapply) withClasses classUris
  }

  implicit lazy val CourseAtomDescriptor: Descriptor[Rdf, CourseAtom] = new Descriptor[Rdf, CourseAtom] {
    override val clazz: Rdf#URI = lwm.Course

    override val classUris: ClassUrisFor[Rdf, CourseAtom] = classUrisFor[CourseAtom](clazz)

    override val references: Ref[Rdf#URI] = Ref(clazz) pointsAt EmployeeDescriptor.references

    private val label = property[String](lwm.label)
    private val description = property[String](lwm.description)
    private val abbreviation = property[String](lwm.abbreviation)
    private val lecturer = property[Employee](lwm.lecturer)(EmployeeDescriptor.binder)
    private val semesterIndex = property[Int](lwm.semesterIndex)

    override val binder: PGBinder[Rdf, CourseAtom] =
      pgbWithId[CourseAtom](atom =>
        makeUri(Course.generateUri(atom.id)))(label, description, abbreviation, lecturer, semesterIndex, invalidated, id)(CourseAtom.apply, CourseAtom.unapply) withClasses classUris
  }

  implicit lazy val DegreeDescriptor: Descriptor[Rdf, Degree] = new Descriptor[Rdf, Degree] {
    override val clazz: Rdf#URI = lwm.Degree

    override val classUris: ClassUrisFor[Rdf, Degree] = classUrisFor[Degree](clazz)

    override val branching: Ref[Rdf#URI] =
      Ref(clazz)
        .pointedAt(TimetableEntryDescriptor.branching)
        .pointedAt(LabworkDescriptor.branching)

    private val label = property[String](lwm.label)
    private val abbreviation = property[String](lwm.abbreviation)

    override val binder: PGBinder[Rdf, Degree] =
      pgbWithId[Degree](degree => makeUri(Degree.generateUri(degree)))(label, abbreviation, invalidated, id)(Degree.apply, Degree.unapply) withClasses classUris
  }

  implicit lazy val GroupDescriptor: Descriptor[Rdf, Group] = new Descriptor[Rdf, Group] {
    override val clazz: Rdf#URI = lwm.Group

    override val classUris: ClassUrisFor[Rdf, Group] = classUrisFor[Group](clazz)

    private val label = property[String](lwm.label)
    private val labwork = property[UUID](lwm.labwork)(uuidRefBinder(Labwork.splitter))
    private val members = set[UUID](lwm.members)(uuidRefBinder(User.splitter))

    override val binder: PGBinder[Rdf, Group] =
      pgbWithId[Group](group =>
        makeUri(Group.generateUri(group)))(label, labwork, members, invalidated, id)(Group.apply, Group.unapply) withClasses classUris
  }


  implicit lazy val GroupAtomDescriptor: Descriptor[Rdf, GroupAtom] = new Descriptor[Rdf, GroupAtom] {
    override val clazz: Rdf#URI = lwm.Group

    override val classUris: ClassUrisFor[Rdf, GroupAtom] = classUrisFor[GroupAtom](clazz)

    override val references: Ref[Rdf#URI] =
      Ref(clazz)
        .pointsAt(LabworkDescriptor.references)
        .pointsAt(StudentDescriptor.references)

    private val label = property[String](lwm.label)
    private val labwork = property[Labwork](lwm.labwork)(LabworkDescriptor.binder)
    private val members = set[Student](lwm.members)(StudentDescriptor.binder)

    override val binder: PGBinder[Rdf, GroupAtom] =
      pgbWithId[GroupAtom](group =>
        makeUri(Group.generateUri(group.id)))(label, labwork, members, invalidated, id)(GroupAtom.apply, GroupAtom.unapply) withClasses classUris
  }

  implicit lazy val RoomDescriptor: Descriptor[Rdf, Room] = new Descriptor[Rdf, Room] {
    override val clazz: Rdf#URI = lwm.Room

    override val classUris: ClassUrisFor[Rdf, Room] = classUrisFor[Room](clazz)

    override val branching: Ref[Rdf#URI] =
      Ref(clazz)
        .pointedAt(ScheduleEntryDescriptor.branching)
        .pointedAt(ReportCardEntryDescriptor.branching)
        .pointedAt(TimetableEntryDescriptor.branching)


    private val label = property[String](lwm.label)
    private val description = property[String](lwm.description)

    override val binder: PGBinder[Rdf, Room] =
      pgbWithId[Room](room =>
        makeUri(Room.generateUri(room)))(label, description, invalidated, id)(Room.apply, Room.unapply) withClasses classUris
  }

  implicit lazy val SemesterDescriptor: Descriptor[Rdf, Semester] = new Descriptor[Rdf, Semester] {
    override val clazz: Rdf#URI = lwm.Semester

    override val classUris: ClassUrisFor[Rdf, Semester] = classUrisFor[Semester](clazz)

    override val branching: Ref[Rdf#URI] = Ref(clazz) pointedAt LabworkDescriptor.branching

    private val label = property[String](lwm.label)
    private val abbreviation = property[String](lwm.abbreviation)
    private val start = property[LocalDate](lwm.start)
    private val end = property[LocalDate](lwm.end)
    private val examStart = property[LocalDate](lwm.examStart)

    override val binder: PGBinder[Rdf, Semester] =
      pgbWithId[Semester](semester =>
        makeUri(Semester.generateUri(semester)))(label, abbreviation, start, end, examStart, invalidated, id)(Semester.apply, Semester.unapply) withClasses classUris
  }

  implicit lazy val TimetableDescriptor: Descriptor[Rdf, Timetable] = new Descriptor[Rdf, Timetable] {
    override val clazz: Rdf#URI = lwm.Timetable

    override val classUris: ClassUrisFor[Rdf, Timetable] = classUrisFor[Timetable](clazz)

    override val references: Ref[Rdf#URI] = Ref(clazz) pointsAt TimetableEntryDescriptor.references

    override val branching: Ref[Rdf#URI] = references

    private val labwork = property[UUID](lwm.labwork)(uuidRefBinder(Labwork.splitter))
    private val entries = set[TimetableEntry](lwm.entries)(TimetableEntryDescriptor.binder)
    private val start = property[LocalDate](lwm.start)
    private val blacklist = set[DateTime](lwm.blacklist)

    override val binder: PGBinder[Rdf, Timetable] =
      pgbWithId[Timetable](timetable =>
        makeUri(Timetable.generateUri(timetable)))(labwork, entries, start, blacklist, invalidated, id)(Timetable.apply, Timetable.unapply) withClasses classUris
  }


  implicit lazy val TimetableAtomDescriptor: Descriptor[Rdf, TimetableAtom] = new Descriptor[Rdf, TimetableAtom] {
    override val clazz: Rdf#URI = lwm.Timetable

    override val classUris: ClassUrisFor[Rdf, TimetableAtom] = classUrisFor[TimetableAtom](clazz)

    override val references: Ref[Rdf#URI] =
      Ref(clazz)
        .pointsAt(LabworkDescriptor.references)
        .pointsAt(TimetableEntryAtomDescriptor.references)

    private val labwork = property[Labwork](lwm.labwork)(LabworkDescriptor.binder)
    private val entries = set[TimetableEntryAtom](lwm.entries)(TimetableEntryAtomDescriptor.binder)
    private val start = property[LocalDate](lwm.start)
    private val blacklist = set[DateTime](lwm.blacklist)

    override val binder: PGBinder[Rdf, TimetableAtom] =
      pgbWithId[TimetableAtom](timetable =>
        makeUri(Timetable.generateUri(timetable.id)))(labwork, entries, start, blacklist, invalidated, id)(TimetableAtom.apply, TimetableAtom.unapply) withClasses classUris

  }

  implicit lazy val TimetableEntryDescriptor: Descriptor[Rdf, TimetableEntry] = new Descriptor[Rdf, TimetableEntry] {
    override val clazz: Rdf#URI = lwm.TimetableEntry

    override val classUris: ClassUrisFor[Rdf, TimetableEntry] = classUrisFor[TimetableEntry](clazz)

    private val supervisor = property[UUID](lwm.supervisor)(uuidRefBinder(User.splitter))
    private val room = property[UUID](lwm.room)(uuidRefBinder(Room.splitter))
    private val degree = property[UUID](lwm.degree)(uuidRefBinder(Degree.splitter))
    private val dayIndex = property[Int](lwm.dayIndex)
    private val start = property[LocalTime](lwm.start)
    private val end = property[LocalTime](lwm.end)

    override val binder: PGBinder[Rdf, TimetableEntry] =
      pgbWithId[TimetableEntry](
        _ => innerUri)(supervisor, room, degree, dayIndex, start, end)(TimetableEntry.apply, TimetableEntry.unapply) withClasses classUris
  }

  implicit lazy val TimetableEntryAtomDescriptor: Descriptor[Rdf, TimetableEntryAtom] = new Descriptor[Rdf, TimetableEntryAtom] {
    override val clazz: Rdf#URI = lwm.TimetableEntry

    override val classUris: ClassUrisFor[Rdf, TimetableEntryAtom] = classUrisFor[TimetableEntryAtom](clazz)

    override val references: Ref[Rdf#URI] =
      Ref(clazz)
        .pointsAt(EmployeeDescriptor.references)
        .pointsAt(RoomDescriptor.references)
        .pointsAt(DegreeDescriptor.references)

    private val supervisor = property[Employee](lwm.supervisor)(EmployeeDescriptor.binder)
    private val room = property[Room](lwm.room)(RoomDescriptor.binder)
    private val degree = property[Degree](lwm.degree)(DegreeDescriptor.binder)
    private val dayIndex = property[Int](lwm.dayIndex)
    private val start = property[LocalTime](lwm.start)
    private val end = property[LocalTime](lwm.end)

    override val binder: PGBinder[Rdf, TimetableEntryAtom] =
      pgbWithId[TimetableEntryAtom](
        _ => innerUri)(supervisor, room, degree, dayIndex, start, end)(TimetableEntryAtom.apply, TimetableEntryAtom.unapply) withClasses classUris
  }

  implicit lazy val ScheduleDescriptor: Descriptor[Rdf, Schedule] = new Descriptor[Rdf, Schedule] {
    override val clazz: Rdf#URI = lwm.Schedule

    override val classUris: ClassUrisFor[Rdf, Schedule] = classUrisFor[Schedule](clazz)

    override val references: Ref[Rdf#URI] = Ref(clazz) pointsAt ScheduleEntryDescriptor.references

    override val branching: Ref[Rdf#URI] = references

    private val labwork = property[UUID](lwm.labwork)(uuidRefBinder(Labwork.splitter))
    private val entries = set[ScheduleEntry](lwm.entries)(ScheduleEntryDescriptor.binder)

    override val binder: PGBinder[Rdf, Schedule] =
      pgbWithId[Schedule](schedule =>
        makeUri(Schedule.generateUri(schedule)))(labwork, entries, invalidated, id)(Schedule.apply, Schedule.unapply) withClasses classUris
  }

  implicit lazy val ScheduleAtomDescriptor: Descriptor[Rdf, ScheduleAtom] = new Descriptor[Rdf, ScheduleAtom] {
    override val clazz: Rdf#URI = lwm.Schedule

    override val classUris: ClassUrisFor[Rdf, ScheduleAtom] = classUrisFor[ScheduleAtom](clazz)

    override val references: Ref[Rdf#URI] =
      Ref(clazz)
        .pointsAt(LabworkDescriptor.references)
        .pointsAt(ScheduleEntryAtomDescriptor.references)

    private val labwork = property[Labwork](lwm.labwork)(LabworkDescriptor.binder)
    private val entries = set[ScheduleEntryAtom](lwm.entries)(ScheduleEntryAtomDescriptor.binder)

    override val binder: PGBinder[Rdf, ScheduleAtom] =
      pgbWithId[ScheduleAtom](schedule =>
        makeUri(Schedule.generateUri(schedule.id)))(labwork, entries, invalidated, id)(ScheduleAtom.apply, ScheduleAtom.unapply) withClasses classUris
  }

  implicit lazy val ScheduleEntryDescriptor: Descriptor[Rdf, ScheduleEntry] = new Descriptor[Rdf, ScheduleEntry] {
    override val clazz: Rdf#URI = lwm.ScheduleEntry

    override val classUris: ClassUrisFor[Rdf, ScheduleEntry] = classUrisFor[ScheduleEntry](clazz)

    private val labwork = property[UUID](lwm.labwork)(uuidRefBinder(Labwork.splitter))
    private val start = property[LocalTime](lwm.start)
    private val end = property[LocalTime](lwm.end)
    private val date = property[LocalDate](lwm.date)
    private val room = property[UUID](lwm.room)(uuidRefBinder(Room.splitter))
    private val supervisor = property[UUID](lwm.supervisor)(uuidRefBinder(User.splitter))
    private val group = property[UUID](lwm.group)(uuidRefBinder(Group.splitter))

    override val binder: PGBinder[Rdf, ScheduleEntry] =
      pgbWithId[ScheduleEntry](sentry =>
        makeUri(ScheduleEntry.generateUri(sentry)))(labwork, start, end, date, room, supervisor, group, invalidated, id)(ScheduleEntry.apply, ScheduleEntry.unapply) withClasses classUris

  }

  implicit lazy val ScheduleEntryAtomDescriptor: Descriptor[Rdf, ScheduleEntryAtom] = new Descriptor[Rdf, ScheduleEntryAtom] {
    override val clazz: Rdf#URI = lwm.ScheduleEntry

    override val classUris: ClassUrisFor[Rdf, ScheduleEntryAtom] = classUrisFor[ScheduleEntryAtom](clazz)

    override val references: Ref[Rdf#URI] =
      Ref(clazz)
        .pointsAt(LabworkDescriptor.references)
        .pointsAt(RoomDescriptor.references)
        .pointsAt(EmployeeDescriptor.references)
        .pointsAt(GroupDescriptor.references)

    private val labwork = property[Labwork](lwm.labwork)(LabworkDescriptor.binder)
    private val room = property[Room](lwm.room)(RoomDescriptor.binder)
    private val supervisor = property[Employee](lwm.supervisor)(EmployeeDescriptor.binder)
    private val group = property[Group](lwm.group)(GroupDescriptor.binder)
    private val start = property[LocalTime](lwm.start)
    private val end = property[LocalTime](lwm.end)
    private val date = property[LocalDate](lwm.date)

    override val binder: PGBinder[Rdf, ScheduleEntryAtom] =
      pgbWithId[ScheduleEntryAtom](sentry =>
        makeUri(ScheduleEntry.generateUri(sentry.id)))(labwork, start, end, date, room, supervisor, group, invalidated, id)(ScheduleEntryAtom.apply, ScheduleEntryAtom.unapply) withClasses classUris

  }

  implicit lazy val BlacklistDescriptor: Descriptor[Rdf, Blacklist] = new Descriptor[Rdf, Blacklist] {
    override val clazz: Rdf#URI = lwm.Blacklist

    override val classUris: ClassUrisFor[Rdf, Blacklist] = classUrisFor[Blacklist](clazz)

    private val label = property[String](lwm.label)
    private val dates = set[DateTime](lwm.dates)

    override val binder: PGBinder[Rdf, Blacklist] =
      pgbWithId[Blacklist](blacklist => makeUri(Blacklist.generateUri(blacklist)))(label, dates, invalidated, id)(Blacklist.apply, Blacklist.unapply) withClasses classUris

  }

  implicit lazy val ReportCardEntryDescriptor: Descriptor[Rdf, ReportCardEntry] = new Descriptor[Rdf, ReportCardEntry] {

    import PropertyEnhancer._

    override val clazz: Rdf#URI = lwm.ReportCardEntry

    override val classUris: ClassUrisFor[Rdf, ReportCardEntry] = classUrisFor[ReportCardEntry](clazz)

    override val references: Ref[Rdf#URI] =
      Ref(clazz)
        .pointsAt(ReportCardEntryTypeDescriptor.references)
        .pointsAt(RescheduledDescriptor.references)

    override val branching: Ref[Rdf#URI] = references pointedAt AnnotationDescriptor.branching

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
      enhance(pgbWithId[ReportCardEntry](reportCardEntry =>
        makeUri(ReportCardEntry.generateUri(reportCardEntry))))(ops)(student, labwork, label, date, start, end, room, types, rescheduled, invalidated, id)(ReportCardEntry.apply, ReportCardEntry.unapply) withClasses classUris

  }

  implicit lazy val ReportCardEntryAtomDescriptor: Descriptor[Rdf, ReportCardEntryAtom] = new Descriptor[Rdf, ReportCardEntryAtom] {

    import PropertyEnhancer._

    override val clazz: Rdf#URI = lwm.ReportCardEntry

    override val classUris: ClassUrisFor[Rdf, ReportCardEntryAtom] = classUrisFor[ReportCardEntryAtom](clazz)

    override val references: Ref[Rdf#URI] =
      Ref(clazz)
        .pointsAt(StudentAtomDescriptor.references)
        .pointsAt(LabworkDescriptor.references)
        .pointsAt(RoomDescriptor.references)
        .pointsAt(ReportCardEntryTypeDescriptor.references)
        .pointsAt(RescheduledAtomDescriptor.references)

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
      enhance(pgbWithId[ReportCardEntryAtom](reportCardEntry =>
        makeUri(ReportCardEntry.generateUri(reportCardEntry.id))))(ops)(student, labwork, label, date, start, end, room, types, rescheduled, invalidated, id)(ReportCardEntryAtom.apply, ReportCardEntryAtom.unapply) withClasses classUris

  }

  implicit lazy val ReportCardEntryTypeDescriptor: Descriptor[Rdf, ReportCardEntryType] = new Descriptor[Rdf, ReportCardEntryType] {
    override val clazz: Rdf#URI = lwm.ReportCardEntryType

    override val classUris: ClassUrisFor[Rdf, ReportCardEntryType] = classUrisFor[ReportCardEntryType](clazz)

    private val entryType = property[String](lwm.entryType)
    private val bool = property[Boolean](lwm.bool)
    private val int = property[Int](lwm.int)

    override val binder: PGBinder[Rdf, ReportCardEntryType] =
      pgbWithId[ReportCardEntryType](reportCardEntryType =>
        makeUri(ReportCardEntryType.generateUri(reportCardEntryType)))(entryType, bool, int, invalidated, id)(ReportCardEntryType.apply, ReportCardEntryType.unapply) withClasses classUris
  }

  implicit lazy val RescheduledDescriptor: Descriptor[Rdf, Rescheduled] = new Descriptor[Rdf, Rescheduled] {
    override val clazz: Rdf#URI = lwm.Rescheduled

    override val classUris: ClassUrisFor[Rdf, Rescheduled] = classUrisFor[Rescheduled](clazz)

    private val date = property[LocalDate](lwm.date)
    private val start = property[LocalTime](lwm.start)
    private val end = property[LocalTime](lwm.end)
    private val room = property[UUID](lwm.room)(uuidRefBinder(Room.splitter))

    override val binder: PGBinder[Rdf, Rescheduled] =
      pgbWithId[Rescheduled](
        _ => innerUri)(date, start, end, room)(Rescheduled.apply, Rescheduled.unapply) withClasses classUris
  }

  implicit lazy val RescheduledAtomDescriptor: Descriptor[Rdf, RescheduledAtom] = new Descriptor[Rdf, RescheduledAtom] {
    override val clazz: Rdf#URI = lwm.Rescheduled

    override val classUris: ClassUrisFor[Rdf, RescheduledAtom] = classUrisFor[RescheduledAtom](clazz)

    override val references: Ref[Rdf#URI] = Ref(clazz) pointsAt RoomDescriptor.references

    private val date = property[LocalDate](lwm.date)
    private val start = property[LocalTime](lwm.start)
    private val end = property[LocalTime](lwm.end)
    private val room = property[Room](lwm.room)(RoomDescriptor.binder)

    override val binder: PGBinder[Rdf, RescheduledAtom] =
      pgbWithId[RescheduledAtom](
        _ => innerUri)(date, start, end, room)(RescheduledAtom.apply, RescheduledAtom.unapply) withClasses classUris
  }

  implicit lazy val AnnotationDescriptor: Descriptor[Rdf, Annotation] = new Descriptor[Rdf, Annotation] {
    override val clazz: Rdf#URI = lwm.Annotation

    override val classUris: ClassUrisFor[Rdf, Annotation] = classUrisFor[Annotation](clazz)

    override val references: Ref[Rdf#URI] = Ref(clazz)

    private val student = property[UUID](lwm.student)(uuidRefBinder(User.splitter))
    private val labwork = property[UUID](lwm.labwork)(uuidRefBinder(Labwork.splitter))
    private val reportCardEntry = property[UUID](lwm.reportCardEntry)(uuidRefBinder(ReportCardEntry.splitter))
    private val message = property[String](lwm.message)
    private val timestamp = property[DateTime](lwm.timestamp)

    override val binder: PGBinder[Rdf, Annotation] =
      pgbWithId[Annotation](annotation =>
        makeUri(Annotation.generateUri(annotation)))(student, labwork, reportCardEntry, message, timestamp, invalidated, id)(Annotation.apply, Annotation.unapply) withClasses classUris

  }

  implicit lazy val AnnotationAtomDescriptor: Descriptor[Rdf, AnnotationAtom] = new Descriptor[Rdf, AnnotationAtom] {
    override val clazz: Rdf#URI = lwm.Annotation

    override val classUris: ClassUrisFor[Rdf, AnnotationAtom] = classUrisFor[AnnotationAtom](clazz)

    override val references: Ref[Rdf#URI] =
      Ref(clazz)
        .pointsAt(StudentDescriptor.references)
        .pointsAt(LabworkDescriptor.references)
        .pointsAt(ReportCardEntryDescriptor.references)

    private val student = property[Student](lwm.student)(StudentDescriptor.binder)
    private val labwork = property[Labwork](lwm.labwork)(LabworkDescriptor.binder)
    private val reportCardEntry = property[ReportCardEntry](lwm.reportCardEntry)(ReportCardEntryDescriptor.binder)
    private val message = property[String](lwm.message)
    private val timestamp = property[DateTime](lwm.timestamp)

    override val binder: PGBinder[Rdf, AnnotationAtom] =
      pgbWithId[AnnotationAtom](annotation =>
        makeUri(Annotation.generateUri(annotation.id)))(student, labwork, reportCardEntry, message, timestamp, invalidated, id)(AnnotationAtom.apply, AnnotationAtom.unapply) withClasses classUris

  }

  implicit lazy val ReportCardEvaluationDescriptor: Descriptor[Rdf, ReportCardEvaluation] = new Descriptor[Rdf, ReportCardEvaluation] {
    override val clazz: Rdf#URI = lwm.ReportCardEvaluation

    override val classUris: ClassUrisFor[Rdf, ReportCardEvaluation] = classUrisFor[ReportCardEvaluation](clazz)

    private val student = property[UUID](lwm.student)(uuidRefBinder(User.splitter))
    private val labwork = property[UUID](lwm.labwork)(uuidRefBinder(Labwork.splitter))
    private val label = property[String](lwm.label)
    private val bool = property[Boolean](lwm.bool)
    private val int = property[Int](lwm.int)

    override val binder: PGBinder[Rdf, ReportCardEvaluation] =
      pgbWithId[ReportCardEvaluation](eval =>
        makeUri(ReportCardEvaluation.generateUri(eval)))(student, labwork, label, bool, int, invalidated, id)(ReportCardEvaluation.apply, ReportCardEvaluation.unapply) withClasses classUris

  }

  implicit lazy val ReportCardEvaluationAtomDescriptor: Descriptor[Rdf, ReportCardEvaluationAtom] = new Descriptor[Rdf, ReportCardEvaluationAtom] {
    override val clazz: Rdf#URI = lwm.ReportCardEvaluation

    override val classUris: ClassUrisFor[Rdf, ReportCardEvaluationAtom] = classUrisFor[ReportCardEvaluationAtom](clazz)

    override val references: Ref[Rdf#URI] =
      Ref(clazz)
        .pointsAt(StudentDescriptor.references)
        .pointsAt(LabworkDescriptor.references)

    private val student = property[Student](lwm.student)(StudentDescriptor.binder)
    private val labwork = property[Labwork](lwm.labwork)(LabworkDescriptor.binder)
    private val label = property[String](lwm.label)
    private val bool = property[Boolean](lwm.bool)
    private val int = property[Int](lwm.int)

    override val binder: PGBinder[Rdf, ReportCardEvaluationAtom] =
      pgbWithId[ReportCardEvaluationAtom](eval =>
        makeUri(ReportCardEvaluation.generateUri(eval.id)))(student, labwork, label, bool, int, invalidated, id)(ReportCardEvaluationAtom.apply, ReportCardEvaluationAtom.unapply) withClasses classUris

  }

}

object Bindings {
  def apply[Rdf <: RDF](base: Namespace)(implicit ops: RDFOps[Rdf], recordBinder: RecordBinder[Rdf]) = new Bindings[Rdf]()(base, ops, recordBinder)
}