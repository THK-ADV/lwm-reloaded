package bind

import base.SesameDbSpec
import models.security._
import models.users.{Employee, Student, User}
import models.{Course, CourseAtom, Degree}
import org.w3.banana.PointedGraph

import scala.util.{Failure, Success}

class AuthorityBindingSpec extends SesameDbSpec {

  import bindings.{AuthorityDescriptor, dateTimeBinder, uuidBinder, uuidRefBinder}
  import ops._

  implicit val authorityBinder = AuthorityDescriptor.binder

  val student = Student("mi1234", "Doe", "John", "11234567", "mi1234@gm.fh-koeln.de", Degree.randomUUID)

  val authWithCourse1 = Authority(student.id, Role.randomUUID, Some(Course.randomUUID))
  val authWithCourse2 = Authority(student.id, Role.randomUUID, Some(Course.randomUUID))
  val authWithoutCourse = Authority(student.id, Role.randomUUID)

  val authorityGraph = URI(Authority.generateUri(authWithCourse1)).a(lwm.Authority)
    .--(lwm.privileged).->-(authWithCourse1.user)(ops, uuidRefBinder(User.splitter))
    .--(lwm.role).->-(authWithCourse1.role)(ops, uuidRefBinder(Role.splitter))
    .--(lwm.course).->-(authWithCourse1.course)(ops, uuidRefBinder(Course.splitter))
    .--(lwm.invalidated).->-(authWithCourse1.invalidated)
    .--(lwm.id).->-(authWithCourse1.id).
    graph

  "An authority" should {

    "return a RDF graph representation of an Authority" in {
      val graph = authWithCourse1.toPG.graph

      graph isIsomorphicWith authorityGraph shouldBe true
    }

    "return an Authority representation of an RDF graph" in {
      val graph = authWithCourse2.toPG.graph
      val authConverted = PointedGraph[Rdf](URI(Authority.generateUri(authWithCourse2)), graph).as[Authority]

      authConverted match {
        case Success(auth) => auth shouldBe authWithCourse2
        case _ => fail("Graph -> Authority morphism failed")
      }
    }

    "return a RDF graph representation of an Authority with empty authorization" in {
      val graph = authWithoutCourse.toPG.graph
      val authConverted = PointedGraph[Rdf](URI(Authority.generateUri(authWithoutCourse)), graph).as[Authority]

      authConverted match {
        case Success(des) =>
          des shouldBe authWithoutCourse
        case _ => fail("Graph -> Authority morphism failed")
      }
    }

    "return an authority atom based on an RDF representation" in {
      import bindings.{AuthorityAtomDescriptor, AuthorityDescriptor, CourseDescriptor, EmployeeDescriptor, RoleDescriptor, StudentDescriptor}

      val lecturer = Employee("lecturer", "lastname", "firstname", "email", "lecturer")
      val course1 = Course("course1", "description", "abbrev", lecturer.id, 3)
      val course2 = Course("course2", "description", "abbrev", lecturer.id, 2)
      val role1 = Role("role1", Set(Permission("perm1"), Permission("perm2")))
      val role2 = Role("role2", Set(Permission("perm3")))
      val authorities = Set(
        Authority(student.id, role1.id, Some(course1.id)),
        Authority(student.id, role2.id)
      )

      val courseAtom = CourseAtom(course1.label, course1.description, course1.abbreviation, lecturer, course1.semesterIndex, course1.invalidated, course1.id)
      val authorityAtoms = Set(
        AuthorityAtom(student, role1, Some(courseAtom), role1.invalidated, authorities.head.id),
        AuthorityAtom(student, role2, None, role2.invalidated, authorities.last.id)
      )

      repo addMany authorities
      repo addMany List(role1, role2)
      repo add lecturer
      repo add student
      repo addMany List(course1, course2)

      repo.getMany[AuthorityAtom](authorities.map(auth => Authority.generateUri(auth))) match {
        case Success(atoms) =>
          atoms shouldEqual authorityAtoms
        case Failure(e) =>
          fail(s"AuthorityAtom could not be deserialised: $e")
      }
    }
  }

}
