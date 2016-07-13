package bind.security

import base.SesameDbSpec
import models.{Course, CourseAtom, Degree}
import models.security._
import models.users.{Employee, Student, User}
import org.w3.banana.PointedGraph
import org.w3.banana.sesame.Sesame
import store.bind.Bindings

import scala.util.{Failure, Success}

class AuthorityBindingSpec extends SesameDbSpec {

  import ops._
  import bindings.{
  AuthorityDescriptor,
  uuidBinder,
  uuidRefBinder}

  implicit val authorityBinder = AuthorityDescriptor.binder

  val student = Student("mi1234", "Doe", "John", "11234567", "mi1234@gm.fh-koeln.de", Degree.randomUUID)

  val authorityForCourse1 = RefRole(Some(Course.randomUUID), Role.randomUUID, RefRole.randomUUID)

  val authorityForCourse2 = RefRole(Some(Course.randomUUID), Role.randomUUID, RefRole.randomUUID)

  val authWith = Authority(student.id, Set(authorityForCourse1.id, authorityForCourse2.id), Authority.randomUUID)
  val authWithout = Authority(student.id, Set.empty, Authority.randomUUID)

  val authorityGraph = URI(Authority.generateUri(authWith)).a(lwm.Authority)
    .--(lwm.id).->-(authWith.id)
    .--(lwm.privileged).->-(authWith.user)(ops, uuidRefBinder(User.splitter))
    .--(lwm.refroles).->-(authWith.refRoles)(ops, uuidRefBinder(RefRole.splitter)).graph

  "An authority" should {
    "return a RDF graph representation of an Authority" in {
      val graph = authWith.toPG.graph

      graph isIsomorphicWith authorityGraph shouldBe true
    }

    "return an Authority representation of an RDF graph" in {
      val graph = authWith.toPG.graph
      val authConverted = PointedGraph[Rdf](URI(Authority.generateUri(authWith)), graph).as[Authority]

      authConverted match {
        case Success(des) => des shouldBe authWith
        case _ => fail("Graph -> Authority morphism failed")
      }
    }

    "return a RDF graph representation of an Authority with empty authorization" in {
      val graph = authWithout.toPG.graph
      val authConverted = PointedGraph[Rdf](URI(Authority.generateUri(authWithout)), graph).as[Authority]

      authConverted match {
        case Success(des) =>
          des shouldBe authWithout
          des.refRoles.isEmpty shouldBe true
        case _ => fail("Graph -> Authority morphism failed")
      }
    }

    "return an authority atom based on an RDF representation" in {
      import bindings.{
      EmployeeDescriptor,
      StudentDescriptor,
      CourseDescriptor,
      RoleDescriptor,
      RefRoleDescriptor,
      AuthorityDescriptor,
      AuthorityAtomDescriptor
      }

      val lecturer = Employee("lecturer", "lastname", "firstname", "email", "lecturer")
      val course1 = Course("course1", "description", "abbrev", lecturer.id, 3)
      val course2 = Course("course2", "description", "abbrev", lecturer.id, 2)
      val role1 = Role("role1", Set(Permission("perm1"), Permission("perm2")))
      val role2 = Role("role2", Set(Permission("perm3")))
      val refrole1 = RefRole(Some(course1.id), role1.id)
      val refrole2 = RefRole(None, role2.id)
      val authority = Authority(student.id, Set(refrole1.id, refrole2.id))

      val courseAtom = CourseAtom(course1.label, course1.description, course1.abbreviation, lecturer, course1.semesterIndex, course1.id)
      val authorityAtom = AuthorityAtom(student, Set(
        RefRoleAtom(Some(courseAtom), role1, refrole1.id),
        RefRoleAtom(None, role2, refrole2.id)
      ), authority.id)

      repo add authority
      repo addMany List(role1, role2)
      repo addMany List(refrole1, refrole2)
      repo add lecturer
      repo add student
      repo addMany List(course1, course2)

      repo.get[AuthorityAtom](Authority.generateUri(authority)) match {
        case Success(Some(atom)) =>
          atom shouldEqual authorityAtom
        case Success(None) =>
          fail("There should exist one authority")
        case Failure(e) =>
          fail(s"AuthorityAtom could not be deserialised: $e")
      }
    }
  }

}
