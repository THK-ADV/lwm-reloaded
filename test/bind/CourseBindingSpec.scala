package bind

import base.SesameDbSpec
import models.Course
import models.users.{Employee, User}
import store.Namespace
import store.bind.Bindings
import org.w3.banana.PointedGraph
import org.w3.banana.sesame.Sesame

import scala.util.{Failure, Success}

class CourseBindingSpec extends SesameDbSpec {
  import ops._
  implicit val ns = Namespace("http://lwm.gm.fh-koeln.de/")

  val bindings = Bindings[Sesame](ns)
  import bindings.CourseBinding.courseBinder
  import bindings.uuidBinder
  import bindings.uuidRefBinder

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
    "return a student based on a RDF graph representation" in {
      val expectedCourse = PointedGraph[Rdf](URI(Course.generateUri(course)), courseGraph).as[Course]

      expectedCourse match {
        case Success(s) =>
          s shouldEqual course
        case Failure(e) =>
          fail(s"Unable to deserialise course graph: $e")
      }
    }
  }
}
