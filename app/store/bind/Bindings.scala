package store.bind

import java.util.UUID

import models._
import org.joda.time.format.ISODateTimeFormat
import org.joda.time.{DateTime, LocalDate, LocalTime}
import org.w3.banana._
import org.w3.banana.binder.{ClassUrisFor, PGBinder, RecordBinder}
import store.Namespace
import store.Prefixes.LWMPrefix
import store.bind.Descriptor._

import scala.language.{implicitConversions, postfixOps}
import scala.util.Try

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

  trait Descriptor[Rdf <: RDF, T] {
    def clazz: Rdf#URI

    def classUris: ClassUrisFor[Rdf, T]

    def references: Ref[Rdf#URI] = Ref(clazz)

    def branching: Ref[Rdf#URI] = Ref(clazz)

    def binder: PGBinder[Rdf, T]
  }

  final case class Ref[+A](root: A, references: List[Ref[A]] = Nil, referenced: List[Ref[A]] = Nil) {
    def pointsAt[AA >: A](that: AA): Ref[AA] = pointsAt(Ref(that))

    def pointsAt[AA >: A](that: Ref[AA]): Ref[AA] = Ref(root, that :: references, referenced)

    def pointedAt[AA >: A](that: AA): Ref[AA] = pointedAt(Ref(that))

    def pointedAt[AA >: A](by: Ref[AA]): Ref[AA] = Ref(root, references, by :: referenced)

    def leftDeref[B](b: B)(f: (B, A) => List[B]): List[B] = deref(b)(f)((_, _) => Nil)

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

    def zipWith[AA >: A, B](refs: List[Ref[AA]])(f: AA => List[B]): List[(B, Ref[AA])] = refs flatMap { ref => f(ref.root) map ((_, ref)) }

    def rightDeref[B](b: B)(f: (B, A) => List[B]): List[B] = deref(b)((_, _) => Nil)(f)
  }

}

class Bindings[Rdf <: RDF](implicit baseNs: Namespace, ops: RDFOps[Rdf], recordBinder: RecordBinder[Rdf]) {

  import ops._
  import recordBinder._

  implicit lazy val UserDescriptor: Descriptor[Rdf, User] = new Descriptor[Rdf, User] {
    override val clazz: Rdf#URI = lwm.User

    override val classUris: ClassUrisFor[Rdf, User] = classUrisFor[User](clazz)


    override val binder: PGBinder[Rdf, User] = new PGBinder[Rdf, User] {
      override def fromPG(pointed: PointedGraph[Rdf]): Try[User] = {
        StudentDescriptor.binder.fromPG(pointed) orElse EmployeeDescriptor.binder.fromPG(pointed)
      }

      override def toPG(t: User): PointedGraph[Rdf] = t match {
        case s: SesameStudent => s.toPG(StudentDescriptor.binder)
        case e: SesameEmployee => e.toPG(EmployeeDescriptor.binder)
      }
    }
  }
  implicit lazy val StudentDescriptor: Descriptor[Rdf, SesameStudent] = new Descriptor[Rdf, SesameStudent] {
    override val clazz: Rdf#URI = lwm.User

    override val classUris: ClassUrisFor[Rdf, SesameStudent] = classUrisFor[SesameStudent](clazz)

    private val lastname = property[String](lwm.lastname)
    private val firstname = property[String](lwm.firstname)
    private val registrationId = property[String](lwm.registrationId)
    private val systemId = property[String](lwm.systemId)
    private val enrollment = property[UUID](lwm.enrollment)(uuidRefBinder(SesameDegree.splitter))
    private val email = property[String](lwm.email)

    override val binder: PGBinder[Rdf, SesameStudent] =
      pgbWithId[SesameStudent](student =>
        makeUri(User.generateUri(student)))(systemId, lastname, firstname, email, registrationId, enrollment, invalidated, id)(SesameStudent.apply, SesameStudent.unapply) withClasses classUris
  }
  implicit lazy val StudentAtomDescriptor: Descriptor[Rdf, SesameStudentAtom] = new Descriptor[Rdf, SesameStudentAtom] {
    override val clazz: Rdf#URI = lwm.User

    override val classUris: ClassUrisFor[Rdf, SesameStudentAtom] = classUrisFor[SesameStudentAtom](clazz)

    override val references: Ref[Rdf#URI] = Ref(clazz) pointsAt DegreeDescriptor.references

    private val lastname = property[String](lwm.lastname)
    private val firstname = property[String](lwm.firstname)
    private val registrationId = property[String](lwm.registrationId)
    private val enrollment = property[SesameDegree](lwm.enrollment)(DegreeDescriptor.binder)
    private val systemId = property[String](lwm.systemId)
    private val email = property[String](lwm.email)

    override val binder: PGBinder[Rdf, SesameStudentAtom] =
      pgbWithId[SesameStudentAtom](student =>
        makeUri(User.generateUri(student.id)))(systemId, lastname, firstname, email, registrationId, enrollment, invalidated, id)(SesameStudentAtom.apply, SesameStudentAtom.unapply) withClasses classUris
  }
  implicit lazy val EmployeeDescriptor: Descriptor[Rdf, SesameEmployee] = new Descriptor[Rdf, SesameEmployee] {
    override val clazz: Rdf#URI = lwm.User

    override val classUris: ClassUrisFor[Rdf, SesameEmployee] = classUrisFor[SesameEmployee](clazz)

    private val lastname = property[String](lwm.lastname)
    private val firstname = property[String](lwm.firstname)
    private val systemId = property[String](lwm.systemId)
    private val email = property[String](lwm.email)
    private val status = property[String](lwm.status)

    override val binder: PGBinder[Rdf, SesameEmployee] =
      pgbWithId[SesameEmployee](employee =>
        makeUri(User.generateUri(employee)))(systemId, lastname, firstname, email, status, invalidated, id)(SesameEmployee.apply, SesameEmployee.unapply) withClasses classUris

  }
  implicit lazy val LabworkApplicationDescriptor: Descriptor[Rdf, SesameLabworkApplication] = new Descriptor[Rdf, SesameLabworkApplication] {
    override val clazz: Rdf#URI = lwm.LabworkApplication

    override val classUris: ClassUrisFor[Rdf, SesameLabworkApplication] = classUrisFor[SesameLabworkApplication](clazz)

    private val labwork = property[UUID](lwm.labwork)(uuidRefBinder(SesameLabwork.splitter))
    private val applicant = property[UUID](lwm.applicant)(uuidRefBinder(User.splitter))
    private val timestamp = property[DateTime](lwm.timestamp)
    private val friends = set[UUID](lwm.friends)(uuidRefBinder(User.splitter))

    override val binder: PGBinder[Rdf, SesameLabworkApplication] =
      pgbWithId[SesameLabworkApplication](application =>
        makeUri(SesameLabworkApplication.generateUri(application)))(labwork, applicant, friends, timestamp, invalidated, id)(SesameLabworkApplication.apply, SesameLabworkApplication.unapply) withClasses classUris
  }
  implicit lazy val LabworkApplicationAtomDescriptor: Descriptor[Rdf, SesameLabworkApplicationAtom] = new Descriptor[Rdf, SesameLabworkApplicationAtom] {
    override val clazz: Rdf#URI = lwm.LabworkApplication

    override val classUris: ClassUrisFor[Rdf, SesameLabworkApplicationAtom] = classUrisFor[SesameLabworkApplicationAtom](clazz)

    override val references: Ref[Rdf#URI] =
      Ref(clazz)
        .pointsAt(LabworkAtomDescriptor.references)
        .pointsAt(StudentDescriptor.references)

    private val labwork = property[SesameLabworkAtom](lwm.labwork)(LabworkAtomDescriptor.binder)
    private val applicant = property[SesameStudent](lwm.applicant)(StudentDescriptor.binder)
    private val friends = set[SesameStudent](lwm.friends)(StudentDescriptor.binder)
    private val timestamp = property[DateTime](lwm.timestamp)

    override val binder: PGBinder[Rdf, SesameLabworkApplicationAtom] =
      pgbWithId[SesameLabworkApplicationAtom](application =>
        makeUri(SesameLabworkApplication.generateUri(application.id)))(labwork, applicant, friends, timestamp, invalidated, id)(SesameLabworkApplicationAtom.apply, SesameLabworkApplicationAtom.unapply) withClasses classUris

  }
  implicit lazy val RoleDescriptor: Descriptor[Rdf, SesameRole] = new Descriptor[Rdf, SesameRole] {
    override val clazz: Rdf#URI = lwm.Role

    override val classUris: ClassUrisFor[Rdf, SesameRole] = classUrisFor[SesameRole](clazz)

    private val label = property[String](lwm.label)
    private val permissions = set[SesamePermission](lwm.permissions)

    override val binder: PGBinder[Rdf, SesameRole] =
      pgbWithId[SesameRole](role =>
        makeUri(SesameRole.generateUri(role)))(label, permissions, invalidated, id)(SesameRole.apply, SesameRole.unapply) withClasses classUris

  }
  implicit lazy val AuthorityDescriptor: Descriptor[Rdf, SesameAuthority] = new Descriptor[Rdf, SesameAuthority] {
    override val clazz: Rdf#URI = lwm.Authority

    override val classUris: ClassUrisFor[Rdf, SesameAuthority] = classUrisFor[SesameAuthority](clazz)

    private val privileged = property[UUID](lwm.privileged)(uuidRefBinder(User.splitter))
    private val course = optional[UUID](lwm.course)(uuidRefBinder(SesameCourse.splitter))
    private val role = property[UUID](lwm.role)(uuidRefBinder(SesameRole.splitter))

    override val binder: PGBinder[Rdf, SesameAuthority] =
      pgbWithId[SesameAuthority](auth =>
        makeUri(SesameAuthority.generateUri(auth)))(privileged, role, course, invalidated, id)(SesameAuthority.apply, SesameAuthority.unapply) withClasses classUris
  }
  implicit lazy val AuthorityAtomDescriptor: Descriptor[Rdf, SesameAuthorityAtom] = new Descriptor[Rdf, SesameAuthorityAtom] {
    override val clazz: Rdf#URI = lwm.Authority

    override val classUris: ClassUrisFor[Rdf, SesameAuthorityAtom] = classUrisFor[SesameAuthorityAtom](clazz)

    override val references: Ref[Rdf#URI] =
      Ref(clazz)
        .pointsAt(UserDescriptor.references)
        .pointsAt(CourseAtomDescriptor.references)
        .pointsAt(RoleDescriptor.references)

    private val privileged = property[User](lwm.privileged)(UserDescriptor.binder)
    private val course = optional[SesameCourseAtom](lwm.course)(CourseAtomDescriptor.binder)
    private val role = property[SesameRole](lwm.role)(RoleDescriptor.binder)

    override val binder: PGBinder[Rdf, SesameAuthorityAtom] =
      pgbWithId[SesameAuthorityAtom](
        auth => makeUri(SesameAuthority.generateUri(auth.id)))(privileged, role, course, invalidated, id)(SesameAuthorityAtom.apply, SesameAuthorityAtom.unapply) withClasses classUris
  }
  implicit lazy val AssignmentEntryDescriptor: Descriptor[Rdf, SesameAssignmentEntry] = new Descriptor[Rdf, SesameAssignmentEntry] {
    override val clazz: Rdf#URI = lwm.AssignmentEntry

    override val classUris: ClassUrisFor[Rdf, SesameAssignmentEntry] = classUrisFor[SesameAssignmentEntry](clazz)

    override val references: Ref[Rdf#URI] = Ref(clazz) pointsAt AssignmentEntryTypeDescriptor.references

    override val branching: Ref[Rdf#URI] = references

    private val index = property[Int](lwm.index)
    private val label = property[String](lwm.label)
    private val duration = property[Int](lwm.duration)
    private val types = set[SesameAssignmentEntryType](lwm.entryTypes)(AssignmentEntryTypeDescriptor.binder)

    override val binder: PGBinder[Rdf, SesameAssignmentEntry] =
      pgbWithId[SesameAssignmentEntry](_ => innerUri)(index, label, types, duration)(SesameAssignmentEntry.apply, SesameAssignmentEntry.unapply) withClasses classUris
  }
  implicit lazy val AssignmentEntryTypeDescriptor: Descriptor[Rdf, SesameAssignmentEntryType] = new Descriptor[Rdf, SesameAssignmentEntryType] {
    override val clazz: Rdf#URI = lwm.AssignmentEntryType

    override val classUris: ClassUrisFor[Rdf, SesameAssignmentEntryType] = classUrisFor[SesameAssignmentEntryType](clazz)

    private val entryType = property[String](lwm.entryType)
    private val bool = property[Boolean](lwm.bool)
    private val int = property[Int](lwm.int)

    override val binder: PGBinder[Rdf, SesameAssignmentEntryType] =
      pgbWithId[SesameAssignmentEntryType](_ => innerUri)(entryType, bool, int)(SesameAssignmentEntryType.apply, SesameAssignmentEntryType.unapply) withClasses classUris
  }
  implicit lazy val AssignmentPlanDescriptor: Descriptor[Rdf, SesameAssignmentPlan] = new Descriptor[Rdf, SesameAssignmentPlan] {
    override val clazz: Rdf#URI = lwm.AssignmentPlan

    override val classUris: ClassUrisFor[Rdf, SesameAssignmentPlan] = classUrisFor[SesameAssignmentPlan](clazz)

    override val references: Ref[Rdf#URI] = Ref(clazz) pointsAt AssignmentEntryDescriptor.references

    override val branching: Ref[Rdf#URI] = references

    private val labwork = property[UUID](lwm.labwork)(uuidRefBinder(SesameLabwork.splitter))
    private val attendance = property[Int](lwm.attendance)
    private val mandatory = property[Int](lwm.mandatory)
    private val entries = set[SesameAssignmentEntry](lwm.entries)(AssignmentEntryDescriptor.binder)

    override val binder: PGBinder[Rdf, SesameAssignmentPlan] =
      pgbWithId[SesameAssignmentPlan](aPlan =>
        makeUri(SesameAssignmentPlan.generateUri(aPlan)))(labwork, attendance, mandatory, entries, invalidated, id)(SesameAssignmentPlan.apply, SesameAssignmentPlan.unapply) withClasses classUris
  }
  implicit lazy val AssignmentPlanAtomDescriptor: Descriptor[Rdf, SesameAssignmentPlanAtom] = new Descriptor[Rdf, SesameAssignmentPlanAtom] {
    override val clazz: Rdf#URI = lwm.AssignmentPlan

    override val classUris: ClassUrisFor[Rdf, SesameAssignmentPlanAtom] = classUrisFor[SesameAssignmentPlanAtom](clazz)

    override val references: Ref[Rdf#URI] =
      Ref(clazz)
        .pointsAt(LabworkDescriptor.references)
        .pointsAt(AssignmentEntryDescriptor.references)

    private val labwork = property[SesameLabwork](lwm.labwork)(LabworkDescriptor.binder)
    private val attendance = property[Int](lwm.attendance)
    private val mandatory = property[Int](lwm.mandatory)
    private val entries = set[SesameAssignmentEntry](lwm.entries)(AssignmentEntryDescriptor.binder)

    override val binder: PGBinder[Rdf, SesameAssignmentPlanAtom] =
      pgbWithId[SesameAssignmentPlanAtom](aPlan =>
        makeUri(SesameAssignmentPlan.generateUri(aPlan.id)))(labwork, attendance, mandatory, entries, invalidated, id)(SesameAssignmentPlanAtom.apply, SesameAssignmentPlanAtom.unapply) withClasses classUris
  }
  implicit lazy val LabworkDescriptor: Descriptor[Rdf, SesameLabwork] = new Descriptor[Rdf, SesameLabwork] {
    override val clazz: Rdf#URI = lwm.Labwork

    override val classUris: ClassUrisFor[Rdf, SesameLabwork] = classUrisFor[SesameLabwork](clazz)

    override val branching: Ref[Rdf#URI] =
      Ref(clazz)
        .pointedAt(AssignmentPlanDescriptor.branching)
        .pointedAt(GroupDescriptor.branching)
        .pointedAt(ScheduleDescriptor.branching)
        .pointedAt(ReportCardEntryDescriptor.branching)
        .pointedAt(ReportCardEvaluationDescriptor.branching)
        .pointedAt(TimetableDescriptor.branching)
        .pointedAt(LabworkApplicationDescriptor.branching)

    private val label = property[String](lwm.label)
    private val description = property[String](lwm.description)
    private val semester = property[UUID](lwm.semester)(uuidRefBinder(SesameSemester.splitter))
    private val course = property[UUID](lwm.course)(uuidRefBinder(SesameCourse.splitter))
    private val degree = property[UUID](lwm.degree)(uuidRefBinder(SesameDegree.splitter))
    private val subscribable = property[Boolean](lwm.subscribable)
    private val published = property[Boolean](lwm.published)

    override val binder: PGBinder[Rdf, SesameLabwork] =
      pgbWithId[SesameLabwork](labwork =>
        makeUri(SesameLabwork.generateUri(labwork)))(label, description, semester, course, degree, subscribable, published, invalidated, id)(SesameLabwork.apply, SesameLabwork.unapply) withClasses classUris
  }
  implicit lazy val LabworkAtomDescriptor: Descriptor[Rdf, SesameLabworkAtom] = new Descriptor[Rdf, SesameLabworkAtom] {
    override val clazz: Rdf#URI = lwm.Labwork

    override val classUris: ClassUrisFor[Rdf, SesameLabworkAtom] = classUrisFor[SesameLabworkAtom](clazz)

    override val references: Ref[Rdf#URI] =
      Ref(clazz)
        .pointsAt(SemesterDescriptor.references)
        .pointsAt(CourseAtomDescriptor.references)
        .pointsAt(DegreeDescriptor.references)

    private val label = property[String](lwm.label)
    private val description = property[String](lwm.description)
    private val semester = property[SesameSemester](lwm.semester)(SemesterDescriptor.binder)
    private val course = property[SesameCourseAtom](lwm.course)(CourseAtomDescriptor.binder)
    private val degree = property[SesameDegree](lwm.degree)(DegreeDescriptor.binder)
    private val subscribable = property[Boolean](lwm.subscribable)
    private val published = property[Boolean](lwm.published)

    override val binder: PGBinder[Rdf, SesameLabworkAtom] =
      pgbWithId[SesameLabworkAtom](
        atom => makeUri(SesameLabwork.generateUri(atom.id)))(label, description, semester, course, degree, subscribable, published, invalidated, id)(SesameLabworkAtom.apply, SesameLabworkAtom.unapply) withClasses classUris

  }
  implicit lazy val CourseDescriptor: Descriptor[Rdf, SesameCourse] = new Descriptor[Rdf, SesameCourse] {
    override val clazz: Rdf#URI = lwm.Course

    override val classUris: ClassUrisFor[Rdf, SesameCourse] = classUrisFor[SesameCourse](clazz)

    override val branching: Ref[Rdf#URI] =
      Ref(clazz)
        .pointedAt(LabworkDescriptor.branching)
        .pointedAt(AuthorityDescriptor.branching)

    private val label = property[String](lwm.label)
    private val description = property[String](lwm.description)
    private val abbreviation = property[String](lwm.abbreviation)
    private val lecturer = property[UUID](lwm.lecturer)(uuidRefBinder(User.splitter))
    private val semesterIndex = property[Int](lwm.semesterIndex)

    override val binder: PGBinder[Rdf, SesameCourse] =
      pgbWithId[SesameCourse](course =>
        makeUri(SesameCourse.generateUri(course)))(label, description, abbreviation, lecturer, semesterIndex, invalidated, id)(SesameCourse.apply, SesameCourse.unapply) withClasses classUris
  }
  implicit lazy val CourseAtomDescriptor: Descriptor[Rdf, SesameCourseAtom] = new Descriptor[Rdf, SesameCourseAtom] {
    override val clazz: Rdf#URI = lwm.Course

    override val classUris: ClassUrisFor[Rdf, SesameCourseAtom] = classUrisFor[SesameCourseAtom](clazz)

    override val references: Ref[Rdf#URI] = Ref(clazz) pointsAt EmployeeDescriptor.references

    private val label = property[String](lwm.label)
    private val description = property[String](lwm.description)
    private val abbreviation = property[String](lwm.abbreviation)
    private val lecturer = property[SesameEmployee](lwm.lecturer)(EmployeeDescriptor.binder)
    private val semesterIndex = property[Int](lwm.semesterIndex)

    override val binder: PGBinder[Rdf, SesameCourseAtom] =
      pgbWithId[SesameCourseAtom](atom =>
        makeUri(SesameCourse.generateUri(atom.id)))(label, description, abbreviation, lecturer, semesterIndex, invalidated, id)(SesameCourseAtom.apply, SesameCourseAtom.unapply) withClasses classUris
  }
  implicit lazy val DegreeDescriptor: Descriptor[Rdf, SesameDegree] = new Descriptor[Rdf, SesameDegree] {
    override val clazz: Rdf#URI = lwm.Degree

    override val classUris: ClassUrisFor[Rdf, SesameDegree] = classUrisFor[SesameDegree](clazz)

    override val branching: Ref[Rdf#URI] =
      Ref(clazz)
        .pointedAt(TimetableEntryDescriptor.branching)
        .pointedAt(LabworkDescriptor.branching)

    private val label = property[String](lwm.label)
    private val abbreviation = property[String](lwm.abbreviation)

    override val binder: PGBinder[Rdf, SesameDegree] =
      pgbWithId[SesameDegree](degree => makeUri(SesameDegree.generateUri(degree)))(label, abbreviation, invalidated, id)(SesameDegree.apply, SesameDegree.unapply) withClasses classUris
  }
  implicit lazy val GroupDescriptor: Descriptor[Rdf, SesameGroup] = new Descriptor[Rdf, SesameGroup] {
    override val clazz: Rdf#URI = lwm.Group

    override val classUris: ClassUrisFor[Rdf, SesameGroup] = classUrisFor[SesameGroup](clazz)

    private val label = property[String](lwm.label)
    private val labwork = property[UUID](lwm.labwork)(uuidRefBinder(SesameLabwork.splitter))
    private val members = set[UUID](lwm.members)(uuidRefBinder(User.splitter))

    override val binder: PGBinder[Rdf, SesameGroup] =
      pgbWithId[SesameGroup](group =>
        makeUri(SesameGroup.generateUri(group)))(label, labwork, members, invalidated, id)(SesameGroup.apply, SesameGroup.unapply) withClasses classUris
  }
  implicit lazy val GroupAtomDescriptor: Descriptor[Rdf, SesameGroupAtom] = new Descriptor[Rdf, SesameGroupAtom] {
    override val clazz: Rdf#URI = lwm.Group

    override val classUris: ClassUrisFor[Rdf, SesameGroupAtom] = classUrisFor[SesameGroupAtom](clazz)

    override val references: Ref[Rdf#URI] =
      Ref(clazz)
        .pointsAt(LabworkDescriptor.references)
        .pointsAt(StudentDescriptor.references)

    private val label = property[String](lwm.label)
    private val labwork = property[SesameLabwork](lwm.labwork)(LabworkDescriptor.binder)
    private val members = set[SesameStudent](lwm.members)(StudentDescriptor.binder)

    override val binder: PGBinder[Rdf, SesameGroupAtom] =
      pgbWithId[SesameGroupAtom](group =>
        makeUri(SesameGroup.generateUri(group.id)))(label, labwork, members, invalidated, id)(SesameGroupAtom.apply, SesameGroupAtom.unapply) withClasses classUris
  }
  implicit lazy val RoomDescriptor: Descriptor[Rdf, SesameRoom] = new Descriptor[Rdf, SesameRoom] {
    override val clazz: Rdf#URI = lwm.Room

    override val classUris: ClassUrisFor[Rdf, SesameRoom] = classUrisFor[SesameRoom](clazz)

    override val branching: Ref[Rdf#URI] =
      Ref(clazz)
        .pointedAt(ScheduleEntryDescriptor.branching)
        .pointedAt(ReportCardEntryDescriptor.branching)
        .pointedAt(TimetableEntryDescriptor.branching)


    private val label = property[String](lwm.label)
    private val description = property[String](lwm.description)

    override val binder: PGBinder[Rdf, SesameRoom] =
      pgbWithId[SesameRoom](room =>
        makeUri(SesameRoom.generateUri(room)))(label, description, invalidated, id)(SesameRoom.apply, SesameRoom.unapply) withClasses classUris
  }
  implicit lazy val SemesterDescriptor: Descriptor[Rdf, SesameSemester] = new Descriptor[Rdf, SesameSemester] {
    override val clazz: Rdf#URI = lwm.Semester

    override val classUris: ClassUrisFor[Rdf, SesameSemester] = classUrisFor[SesameSemester](clazz)

    override val branching: Ref[Rdf#URI] = Ref(clazz) pointedAt LabworkDescriptor.branching

    private val label = property[String](lwm.label)
    private val abbreviation = property[String](lwm.abbreviation)
    private val start = property[LocalDate](lwm.start)
    private val end = property[LocalDate](lwm.end)
    private val examStart = property[LocalDate](lwm.examStart)

    override val binder: PGBinder[Rdf, SesameSemester] =
      pgbWithId[SesameSemester](semester =>
        makeUri(SesameSemester.generateUri(semester)))(label, abbreviation, start, end, examStart, invalidated, id)(SesameSemester.apply, SesameSemester.unapply) withClasses classUris
  }
  implicit lazy val TimetableDescriptor: Descriptor[Rdf, SesameTimetable] = new Descriptor[Rdf, SesameTimetable] {
    override val clazz: Rdf#URI = lwm.Timetable

    override val classUris: ClassUrisFor[Rdf, SesameTimetable] = classUrisFor[SesameTimetable](clazz)

    override val references: Ref[Rdf#URI] = Ref(clazz) pointsAt TimetableEntryDescriptor.references

    override val branching: Ref[Rdf#URI] = references

    private val labwork = property[UUID](lwm.labwork)(uuidRefBinder(SesameLabwork.splitter))
    private val entries = set[SesameTimetableEntry](lwm.entries)(TimetableEntryDescriptor.binder)
    private val start = property[LocalDate](lwm.start)
    private val blacklist = set[DateTime](lwm.blacklist)

    override val binder: PGBinder[Rdf, SesameTimetable] =
      pgbWithId[SesameTimetable](timetable =>
        makeUri(SesameTimetable.generateUri(timetable)))(labwork, entries, start, blacklist, invalidated, id)(SesameTimetable.apply, SesameTimetable.unapply) withClasses classUris
  }
  implicit lazy val TimetableAtomDescriptor: Descriptor[Rdf, SesameTimetableAtom] = new Descriptor[Rdf, SesameTimetableAtom] {
    override val clazz: Rdf#URI = lwm.Timetable

    override val classUris: ClassUrisFor[Rdf, SesameTimetableAtom] = classUrisFor[SesameTimetableAtom](clazz)

    override val references: Ref[Rdf#URI] =
      Ref(clazz)
        .pointsAt(LabworkDescriptor.references)
        .pointsAt(TimetableEntryAtomDescriptor.references)

    private val labwork = property[SesameLabwork](lwm.labwork)(LabworkDescriptor.binder)
    private val entries = set[SesameTimetableEntryAtom](lwm.entries)(TimetableEntryAtomDescriptor.binder)
    private val start = property[LocalDate](lwm.start)
    private val blacklist = set[DateTime](lwm.blacklist)

    override val binder: PGBinder[Rdf, SesameTimetableAtom] =
      pgbWithId[SesameTimetableAtom](timetable =>
        makeUri(SesameTimetable.generateUri(timetable.id)))(labwork, entries, start, blacklist, invalidated, id)(SesameTimetableAtom.apply, SesameTimetableAtom.unapply) withClasses classUris

  }
  implicit lazy val TimetableEntryDescriptor: Descriptor[Rdf, SesameTimetableEntry] = new Descriptor[Rdf, SesameTimetableEntry] {
    override val clazz: Rdf#URI = lwm.TimetableEntry

    override val classUris: ClassUrisFor[Rdf, SesameTimetableEntry] = classUrisFor[SesameTimetableEntry](clazz)

    private val supervisor = set[UUID](lwm.supervisor)(uuidRefBinder(User.splitter))
    private val room = property[UUID](lwm.room)(uuidRefBinder(SesameRoom.splitter))
    private val dayIndex = property[Int](lwm.dayIndex)
    private val start = property[LocalTime](lwm.start)
    private val end = property[LocalTime](lwm.end)

    override val binder: PGBinder[Rdf, SesameTimetableEntry] =
      pgbWithId[SesameTimetableEntry](
        _ => innerUri)(supervisor, room, dayIndex, start, end)(SesameTimetableEntry.apply, SesameTimetableEntry.unapply) withClasses classUris
  }
  implicit lazy val TimetableEntryAtomDescriptor: Descriptor[Rdf, SesameTimetableEntryAtom] = new Descriptor[Rdf, SesameTimetableEntryAtom] {
    override val clazz: Rdf#URI = lwm.TimetableEntry

    override val classUris: ClassUrisFor[Rdf, SesameTimetableEntryAtom] = classUrisFor[SesameTimetableEntryAtom](clazz)

    override val references: Ref[Rdf#URI] =
      Ref(clazz)
        .pointsAt(UserDescriptor.references)
        .pointsAt(RoomDescriptor.references)

    private val supervisor = set[User](lwm.supervisor)(UserDescriptor.binder)
    private val room = property[SesameRoom](lwm.room)(RoomDescriptor.binder)
    private val dayIndex = property[Int](lwm.dayIndex)
    private val start = property[LocalTime](lwm.start)
    private val end = property[LocalTime](lwm.end)

    override val binder: PGBinder[Rdf, SesameTimetableEntryAtom] =
      pgbWithId[SesameTimetableEntryAtom](
        _ => innerUri)(supervisor, room, dayIndex, start, end)(SesameTimetableEntryAtom.apply, SesameTimetableEntryAtom.unapply) withClasses classUris
  }
  implicit lazy val ScheduleDescriptor: Descriptor[Rdf, SesameSchedule] = new Descriptor[Rdf, SesameSchedule] {
    override val clazz: Rdf#URI = lwm.Schedule

    override val classUris: ClassUrisFor[Rdf, SesameSchedule] = classUrisFor[SesameSchedule](clazz)

    override val references: Ref[Rdf#URI] = Ref(clazz) pointsAt ScheduleEntryDescriptor.references

    override val branching: Ref[Rdf#URI] = references

    private val labwork = property[UUID](lwm.labwork)(uuidRefBinder(SesameLabwork.splitter))
    private val entries = set[SesameScheduleEntry](lwm.entries)(ScheduleEntryDescriptor.binder)

    override val binder: PGBinder[Rdf, SesameSchedule] =
      pgbWithId[SesameSchedule](schedule =>
        makeUri(SesameSchedule.generateUri(schedule)))(labwork, entries, invalidated, id)(SesameSchedule.apply, SesameSchedule.unapply) withClasses classUris
  }
  implicit lazy val ScheduleAtomDescriptor: Descriptor[Rdf, SesameScheduleAtom] = new Descriptor[Rdf, SesameScheduleAtom] {
    override val clazz: Rdf#URI = lwm.Schedule

    override val classUris: ClassUrisFor[Rdf, SesameScheduleAtom] = classUrisFor[SesameScheduleAtom](clazz)

    override val references: Ref[Rdf#URI] =
      Ref(clazz)
        .pointsAt(LabworkAtomDescriptor.references)
        .pointsAt(ScheduleEntryAtomDescriptor.references)

    private val labwork = property[SesameLabworkAtom](lwm.labwork)(LabworkAtomDescriptor.binder)
    private val entries = set[SesameScheduleEntryAtom](lwm.entries)(ScheduleEntryAtomDescriptor.binder)

    override val binder: PGBinder[Rdf, SesameScheduleAtom] =
      pgbWithId[SesameScheduleAtom](schedule =>
        makeUri(SesameSchedule.generateUri(schedule.id)))(labwork, entries, invalidated, id)(SesameScheduleAtom.apply, SesameScheduleAtom.unapply) withClasses classUris
  }
  implicit lazy val ScheduleEntryDescriptor: Descriptor[Rdf, SesameScheduleEntry] = new Descriptor[Rdf, SesameScheduleEntry] {
    override val clazz: Rdf#URI = lwm.ScheduleEntry

    override val classUris: ClassUrisFor[Rdf, SesameScheduleEntry] = classUrisFor[SesameScheduleEntry](clazz)

    private val labwork = property[UUID](lwm.labwork)(uuidRefBinder(SesameLabwork.splitter))
    private val start = property[LocalTime](lwm.start)
    private val end = property[LocalTime](lwm.end)
    private val date = property[LocalDate](lwm.date)
    private val room = property[UUID](lwm.room)(uuidRefBinder(SesameRoom.splitter))
    private val supervisor = set[UUID](lwm.supervisor)(uuidRefBinder(User.splitter))
    private val group = property[UUID](lwm.group)(uuidRefBinder(SesameGroup.splitter))

    override val binder: PGBinder[Rdf, SesameScheduleEntry] =
      pgbWithId[SesameScheduleEntry](sentry =>
        makeUri(SesameScheduleEntry.generateUri(sentry)))(labwork, start, end, date, room, supervisor, group, invalidated, id)(SesameScheduleEntry.apply, SesameScheduleEntry.unapply) withClasses classUris

  }
  implicit lazy val ScheduleEntryAtomDescriptor: Descriptor[Rdf, SesameScheduleEntryAtom] = new Descriptor[Rdf, SesameScheduleEntryAtom] {
    override val clazz: Rdf#URI = lwm.ScheduleEntry

    override val classUris: ClassUrisFor[Rdf, SesameScheduleEntryAtom] = classUrisFor[SesameScheduleEntryAtom](clazz)

    override val references: Ref[Rdf#URI] =
      Ref(clazz)
        .pointsAt(LabworkAtomDescriptor.references)
        .pointsAt(RoomDescriptor.references)
        .pointsAt(UserDescriptor.references)
        .pointsAt(GroupDescriptor.references)

    private val labwork = property[SesameLabworkAtom](lwm.labwork)(LabworkAtomDescriptor.binder)
    private val room = property[SesameRoom](lwm.room)(RoomDescriptor.binder)
    private val supervisor = set[User](lwm.supervisor)(UserDescriptor.binder)
    private val group = property[SesameGroup](lwm.group)(GroupDescriptor.binder)
    private val start = property[LocalTime](lwm.start)
    private val end = property[LocalTime](lwm.end)
    private val date = property[LocalDate](lwm.date)

    override val binder: PGBinder[Rdf, SesameScheduleEntryAtom] =
      pgbWithId[SesameScheduleEntryAtom](sentry =>
        makeUri(SesameScheduleEntry.generateUri(sentry.id)))(labwork, start, end, date, room, supervisor, group, invalidated, id)(SesameScheduleEntryAtom.apply, SesameScheduleEntryAtom.unapply) withClasses classUris

  }
  implicit lazy val BlacklistDescriptor: Descriptor[Rdf, SesameBlacklist] = new Descriptor[Rdf, SesameBlacklist] {
    override val clazz: Rdf#URI = lwm.Blacklist

    override val classUris: ClassUrisFor[Rdf, SesameBlacklist] = classUrisFor[SesameBlacklist](clazz)

    private val label = property[String](lwm.label)
    private val dates = set[DateTime](lwm.dates)

    override val binder: PGBinder[Rdf, SesameBlacklist] =
      pgbWithId[SesameBlacklist](blacklist => makeUri(SesameBlacklist.generateUri(blacklist)))(label, dates, invalidated, id)(SesameBlacklist.apply, SesameBlacklist.unapply) withClasses classUris

  }
  implicit lazy val ReportCardEntryDescriptor: Descriptor[Rdf, SesameReportCardEntry] = new Descriptor[Rdf, SesameReportCardEntry] {
    import store.bind.PropertyEnhancer._

    override val clazz: Rdf#URI = lwm.ReportCardEntry

    override val classUris: ClassUrisFor[Rdf, SesameReportCardEntry] = classUrisFor[SesameReportCardEntry](clazz)

    override val references: Ref[Rdf#URI] =
      Ref(clazz)
        .pointsAt(ReportCardEntryTypeDescriptor.references)
        .pointsAt(RescheduledDescriptor.references)

    override val branching: Ref[Rdf#URI] = references

    private val student = property[UUID](lwm.student)(uuidRefBinder(User.splitter))
    private val labwork = property[UUID](lwm.labwork)(uuidRefBinder(SesameLabwork.splitter))
    private val label = property[String](lwm.label)
    private val date = property[LocalDate](lwm.date)
    private val start = property[LocalTime](lwm.start)
    private val end = property[LocalTime](lwm.end)
    private val room = property[UUID](lwm.room)(uuidRefBinder(SesameRoom.splitter))
    private val rescheduled = optional[SesameRescheduled](lwm.rescheduled)(RescheduledDescriptor.binder)
    private val types = set[SesameReportCardEntryType](lwm.entryTypes)(ReportCardEntryTypeDescriptor.binder)

    override val binder: PGBinder[Rdf, SesameReportCardEntry] =
      enhance(pgbWithId[SesameReportCardEntry](reportCardEntry =>
        makeUri(SesameReportCardEntry.generateUri(reportCardEntry))))(ops)(student, labwork, label, date, start, end, room, types, rescheduled, invalidated, id)(SesameReportCardEntry.apply, SesameReportCardEntry.unapply) withClasses classUris

  }
  implicit lazy val ReportCardEntryAtomDescriptor: Descriptor[Rdf, SesameReportCardEntryAtom] = new Descriptor[Rdf, SesameReportCardEntryAtom] {
    import store.bind.PropertyEnhancer._

    override val clazz: Rdf#URI = lwm.ReportCardEntry

    override val classUris: ClassUrisFor[Rdf, SesameReportCardEntryAtom] = classUrisFor[SesameReportCardEntryAtom](clazz)

    override val references: Ref[Rdf#URI] =
      Ref(clazz)
        .pointsAt(StudentAtomDescriptor.references)
        .pointsAt(LabworkDescriptor.references)
        .pointsAt(RoomDescriptor.references)
        .pointsAt(ReportCardEntryTypeDescriptor.references)
        .pointsAt(RescheduledAtomDescriptor.references)

    private val student = property[SesameStudent](lwm.student)(StudentDescriptor.binder)
    private val labwork = property[SesameLabwork](lwm.labwork)(LabworkDescriptor.binder)
    private val room = property[SesameRoom](lwm.room)(RoomDescriptor.binder)
    private val rescheduled = optional[SesameRescheduledAtom](lwm.rescheduled)(RescheduledAtomDescriptor.binder)
    private val label = property[String](lwm.label)
    private val date = property[LocalDate](lwm.date)
    private val start = property[LocalTime](lwm.start)
    private val end = property[LocalTime](lwm.end)
    private val types = set[SesameReportCardEntryType](lwm.entryTypes)(ReportCardEntryTypeDescriptor.binder)

    override val binder: PGBinder[Rdf, SesameReportCardEntryAtom] =
      enhance(pgbWithId[SesameReportCardEntryAtom](reportCardEntry =>
        makeUri(SesameReportCardEntry.generateUri(reportCardEntry.id))))(ops)(student, labwork, label, date, start, end, room, types, rescheduled, invalidated, id)(SesameReportCardEntryAtom.apply, SesameReportCardEntryAtom.unapply) withClasses classUris

  }
  implicit lazy val ReportCardEntryTypeDescriptor: Descriptor[Rdf, SesameReportCardEntryType] = new Descriptor[Rdf, SesameReportCardEntryType] {
    override val clazz: Rdf#URI = lwm.ReportCardEntryType

    override val classUris: ClassUrisFor[Rdf, SesameReportCardEntryType] = classUrisFor[SesameReportCardEntryType](clazz)

    private val entryType = property[String](lwm.entryType)
    private val bool = property[Boolean](lwm.bool)
    private val int = property[Int](lwm.int)

    override val binder: PGBinder[Rdf, SesameReportCardEntryType] =
      pgbWithId[SesameReportCardEntryType](reportCardEntryType =>
        makeUri(SesameReportCardEntryType.generateUri(reportCardEntryType)))(entryType, bool, int, invalidated, id)(SesameReportCardEntryType.apply, SesameReportCardEntryType.unapply) withClasses classUris
  }
  implicit lazy val RescheduledDescriptor: Descriptor[Rdf, SesameRescheduled] = new Descriptor[Rdf, SesameRescheduled] {
    override val clazz: Rdf#URI = lwm.Rescheduled

    override val classUris: ClassUrisFor[Rdf, SesameRescheduled] = classUrisFor[SesameRescheduled](clazz)

    private val date = property[LocalDate](lwm.date)
    private val start = property[LocalTime](lwm.start)
    private val end = property[LocalTime](lwm.end)
    private val room = property[UUID](lwm.room)(uuidRefBinder(SesameRoom.splitter))

    override val binder: PGBinder[Rdf, SesameRescheduled] =
      pgbWithId[SesameRescheduled](
        _ => innerUri)(date, start, end, room)(SesameRescheduled.apply, SesameRescheduled.unapply) withClasses classUris
  }
  implicit lazy val RescheduledAtomDescriptor: Descriptor[Rdf, SesameRescheduledAtom] = new Descriptor[Rdf, SesameRescheduledAtom] {
    override val clazz: Rdf#URI = lwm.Rescheduled

    override val classUris: ClassUrisFor[Rdf, SesameRescheduledAtom] = classUrisFor[SesameRescheduledAtom](clazz)

    override val references: Ref[Rdf#URI] = Ref(clazz) pointsAt RoomDescriptor.references

    private val date = property[LocalDate](lwm.date)
    private val start = property[LocalTime](lwm.start)
    private val end = property[LocalTime](lwm.end)
    private val room = property[SesameRoom](lwm.room)(RoomDescriptor.binder)

    override val binder: PGBinder[Rdf, SesameRescheduledAtom] =
      pgbWithId[SesameRescheduledAtom](
        _ => innerUri)(date, start, end, room)(SesameRescheduledAtom.apply, SesameRescheduledAtom.unapply) withClasses classUris
  }

  implicit lazy val ReportCardEvaluationDescriptor: Descriptor[Rdf, SesameReportCardEvaluation] = new Descriptor[Rdf, SesameReportCardEvaluation] {
    override val clazz: Rdf#URI = lwm.ReportCardEvaluation

    override val classUris: ClassUrisFor[Rdf, SesameReportCardEvaluation] = classUrisFor[SesameReportCardEvaluation](clazz)

    private val student = property[UUID](lwm.student)(uuidRefBinder(User.splitter))
    private val labwork = property[UUID](lwm.labwork)(uuidRefBinder(SesameLabwork.splitter))
    private val label = property[String](lwm.label)
    private val bool = property[Boolean](lwm.bool)
    private val int = property[Int](lwm.int)
    private val timestamp = property[DateTime](lwm.timestamp)

    override val binder: PGBinder[Rdf, SesameReportCardEvaluation] =
      pgbWithId[SesameReportCardEvaluation](eval =>
        makeUri(SesameReportCardEvaluation.generateUri(eval)))(student, labwork, label, bool, int, timestamp, invalidated, id)(SesameReportCardEvaluation.apply, SesameReportCardEvaluation.unapply) withClasses classUris

  }
  implicit lazy val ReportCardEvaluationAtomDescriptor: Descriptor[Rdf, SesameReportCardEvaluationAtom] = new Descriptor[Rdf, SesameReportCardEvaluationAtom] {
    override val clazz: Rdf#URI = lwm.ReportCardEvaluation

    override val classUris: ClassUrisFor[Rdf, SesameReportCardEvaluationAtom] = classUrisFor[SesameReportCardEvaluationAtom](clazz)

    override val references: Ref[Rdf#URI] =
      Ref(clazz)
        .pointsAt(StudentDescriptor.references)
        .pointsAt(LabworkAtomDescriptor.references)

    private val student = property[SesameStudent](lwm.student)(StudentDescriptor.binder)
    private val labwork = property[SesameLabworkAtom](lwm.labwork)(LabworkAtomDescriptor.binder)
    private val label = property[String](lwm.label)
    private val bool = property[Boolean](lwm.bool)
    private val int = property[Int](lwm.int)
    private val timestamp = property[DateTime](lwm.timestamp)

    override val binder: PGBinder[Rdf, SesameReportCardEvaluationAtom] =
      pgbWithId[SesameReportCardEvaluationAtom](eval =>
        makeUri(SesameReportCardEvaluation.generateUri(eval.id)))(student, labwork, label, bool, int, timestamp, invalidated, id)(SesameReportCardEvaluationAtom.apply, SesameReportCardEvaluationAtom.unapply) withClasses classUris

  }
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
  implicit val permissionBinder = new PGBinder[Rdf, SesamePermission] {
    override def toPG(t: SesamePermission): PointedGraph[Rdf] = {
      PointedGraph(ops.makeLiteral(t.value, xsd.string))
    }

    override def fromPG(pointed: PointedGraph[Rdf]): Try[SesamePermission] = {
      pointed.pointer.as[String].map(SesamePermission.apply)
    }
  }
  val id = property[UUID](lwm.id)
  val invalidated = optional[DateTime](lwm.invalidated)

  def innerUri: Rdf#URI = makeUri(s"$baseNs/${newUri("#")}")

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
}

object Bindings {
  def apply[Rdf <: RDF](base: Namespace)(implicit ops: RDFOps[Rdf], recordBinder: RecordBinder[Rdf]) = new Bindings[Rdf]()(base, ops, recordBinder)
}