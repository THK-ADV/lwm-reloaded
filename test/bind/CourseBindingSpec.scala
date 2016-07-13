package bind

import base.SesameDbSpec
import models.{Course, CourseAtom}
import models.users.{Employee, User}
import store.bind.Bindings
import org.w3.banana.PointedGraph
import org.w3.banana.sesame.Sesame

import scala.util.{Failure, Success}

class CourseBindingSpec extends SesameDbSpec {

  import bindings.{
  CourseDescriptor,
  uuidBinder,
  uuidRefBinder}
  import ops._

  implicit val courseBinder = CourseDescriptor.binder

  val course = Course("Algorithmen und Programmierung", "AP Victor", "AP", User.randomUUID, 1, Course.randomUUID)
  val courseGraph = URI(Course.generateUri(course)).a(lwm.Course)
    .--(lwm.label).->-(course.label)
    .--(lwm.description).->-(course.description)
    .--(lwm.abbreviation).->-(course.abbreviation)
    .--(lwm.lecturer).->-(course.lecturer)(ops, uuidRefBinder(User.splitter))
    .--(lwm.semesterIndex).->-(course.semesterIndex)
    .--(lwm.id).->-(course.id).graph

  "A CourseBindingSpec" should {
    "return a RDF graph representation of a course" in {
      val graph = course.toPG.graph

      graph isIsomorphicWith courseGraph shouldBe true
    }

    "return a course based on a RDF graph representation" in {
      val expectedCourse = PointedGraph[Rdf](URI(Course.generateUri(course)), courseGraph).as[Course]

      expectedCourse match {
        case Success(s) =>
          s shouldEqual course
        case Failure(e) =>
          fail(s"Unable to deserialise course graph: $e")
      }
    }

    "return a course atom based on an RDF graph representation" in {
      import bindings.{
      EmployeeDescriptor,
      CourseDescriptor,
      CourseAtomDescriptor
      }

      val lecturer = Employee("systemid", "lastname", "firstname", "email", "lecturer")
      val course = Course("course", "description", "abbr", lecturer.id, 2)
      val courseAtom = CourseAtom(course.label, course.description, course.abbreviation, lecturer, course.semesterIndex, course.id)

      repo add lecturer
      repo add course

      repo.get[CourseAtom](Course.generateUri(course)) match {
        case Success(Some(atom)) =>
          atom shouldEqual courseAtom
        case Success(None) =>
          fail("There should exist one course")
        case Failure(e) =>
          fail(s"CourseAtom could not be deserialised: $e")
      }
    }
  }
}
