package bind

import base.SesameDbSpec
import models.Course
import models.users.User
import store.bind.Bindings
import org.w3.banana.PointedGraph
import org.w3.banana.sesame.Sesame

import scala.util.{Failure, Success}

class CourseBindingSpec extends SesameDbSpec {

  val bindings = Bindings[Sesame](namespace)
  import bindings.CourseBinding.courseBinder
  import bindings.{uuidBinder, uuidRefBinder}
  import ops._

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
