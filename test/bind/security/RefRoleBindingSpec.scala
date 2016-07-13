package bind.security

import base.SesameDbSpec
import models.{Course, CourseAtom}
import models.security.{Permission, RefRole, RefRoleAtom, Role}
import models.users.Employee
import org.w3.banana.PointedGraph
import org.w3.banana.sesame.Sesame
import store.bind.Bindings

import scala.util.{Failure, Success}

class RefRoleBindingSpec extends SesameDbSpec {
  import bindings.{
  RefRoleDescriptor,
  uuidBinder,
  uuidRefBinder}
  import ops._

  implicit val refRoleBinder = RefRoleDescriptor.binder

  val refRoleWithCourse = RefRole(Some(Course.randomUUID), Role.randomUUID, RefRole.randomUUID)

  val refRoleWithoutCourse = RefRole(None, Role.randomUUID, RefRole.randomUUID)

  val refRoleGraphWithCourse = URI(RefRole.generateUri(refRoleWithCourse)).a(lwm.RefRole)
    .--(lwm.course).->-(refRoleWithCourse.course)(ops, uuidRefBinder(Course.splitter))
    .--(lwm.role).->-(refRoleWithCourse.role)(ops, uuidRefBinder(Role.splitter))
    .--(lwm.id).->-(refRoleWithCourse.id).graph

  val refRoleGraphWithoutCourse = URI(RefRole.generateUri(refRoleWithoutCourse)).a(lwm.RefRole)
    .--(lwm.course).->-(refRoleWithoutCourse.course)(ops, uuidRefBinder(Course.splitter))
    .--(lwm.role).->-(refRoleWithoutCourse.role)(ops, uuidRefBinder(Role.splitter))
    .--(lwm.id).->-(refRoleWithoutCourse.id).graph

  "A RefRoleBindingSpec" should {

    "return a RDF graph representation of a refRole" in {
      val graph = refRoleWithCourse.toPG.graph

      graph isIsomorphicWith refRoleGraphWithCourse shouldBe true
    }

    "return a refRole based on a RDF graph representation" in {
      val expectedRefRole = PointedGraph[Rdf](URI(RefRole.generateUri(refRoleWithCourse)), refRoleGraphWithCourse).as[RefRole]

      expectedRefRole match {
        case Success(s) =>
          s shouldEqual refRoleWithCourse
        case Failure(e) =>
          fail(s"Unable to deserialise refRole graph: $e")
      }
    }

    "return a RDF graph representation of a refRole without course association" in {
      val graph = refRoleWithoutCourse.toPG.graph

      graph isIsomorphicWith refRoleGraphWithoutCourse shouldBe true
    }

    "return a refRole based on a RDF graph representation without course association" in {
      val expectedRefRole = PointedGraph[Rdf](URI(RefRole.generateUri(refRoleWithoutCourse)), refRoleGraphWithoutCourse).as[RefRole]

      expectedRefRole match {
        case Success(s) =>
          s shouldEqual refRoleWithoutCourse
        case Failure(e) =>
          fail(s"Unable to deserialise refRole graph: $e")
      }
    }

    "return a refRole atom based on an RDF graph representation" in {
      import bindings.{
      EmployeeDescriptor,
      CourseDescriptor,
      RoleDescriptor,
      RefRoleDescriptor,
      RefRoleAtomDescriptor}

      val lecturer = Employee("systemId1", "lastname1", "firstname1", "email1", "lecturer")
      val course = Course("label", "description", "abbr", lecturer.id, 1)
      val role1 = Role("role1", Set(Permission("1"), Permission("2")))
      val role2 = Role("role2", Set(Permission("3")))
      val refrole1 = RefRole(Some(course.id), role1.id)
      val refrole2 = RefRole(None, role2.id)

      val courseAtom = CourseAtom(course.label, course.description, course.abbreviation, lecturer, course.semesterIndex, course.id)

      val refroleAtom1 = RefRoleAtom(Some(courseAtom), role1, refrole1.id)
      val refroleAtom2 = RefRoleAtom(None, role2, refrole2.id)

      repo add lecturer
      repo add course
      repo addMany List(role1, role2)
      repo addMany List(refrole1, refrole2)

      repo.get[RefRoleAtom](RefRole.generateUri(refrole1)) match {
        case Success(Some(atom)) =>
          atom shouldEqual refroleAtom1
        case Success(None) =>
          fail("There should exist one refrole")
        case Failure(e) =>
          fail(s"RefRoleAtom1 could not be deserialised: $e")
      }

      repo.get[RefRoleAtom](RefRole.generateUri(refrole2)) match {
        case Success(Some(atom)) =>
          atom shouldEqual refroleAtom2
        case Success(None) =>
          fail("There should exist one refrole")
        case Failure(e) =>
          fail(s"RefRoleAtom2 could not be deserialised: $e")
      }
    }
  }
}
