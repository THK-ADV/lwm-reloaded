package controllers.crud

import java.util.UUID

import models.users.{Employee, User}
import models.{Course, CourseProtocol}
import org.mockito.Matchers._
import org.mockito.Mockito._
import org.w3.banana.PointedGraph
import org.w3.banana.sesame.Sesame
import play.api.libs.json.{JsValue, Json, Writes}
import play.api.mvc.{Action, Result, AnyContent, Request}
import play.api.test.FakeRequest
import play.api.test.Helpers._
import utils.LWMActions.ContentTypedAction
import utils.LwmMimeType

import scala.util.Success
class CourseCRUDControllerSpec extends AbstractCRUDControllerSpec[CourseProtocol, Course] {

  override val entityToPass: Course = Course("label to pass", "abbreviation to pass", User.randomUUID, Course.randomUUID)

  override val controller: AbstractCRUDController[CourseProtocol, Course] = new CourseCRUDController(repository, namespace, roleService) {

    override protected def fromInput(input: CourseProtocol, id: Option[UUID]) = entityToPass
  }

  override val entityToFail: Course = Course("label to fail", "abbreviation to fail", User.randomUUID, Course.randomUUID)

  override implicit val jsonWrites: Writes[Course] = Course.writes

  override val mimeType: LwmMimeType = LwmMimeType.courseV1Json

  override val inputJson: JsValue = Json.obj(
    "label" -> "label input",
    "abbreviation" -> "abbreviation input",
    "lecturer" -> User.randomUUID.toString
  )

  override def entityTypeName: String = "course"

  import bindings.CourseBinding._
  import ops._

  override def pointedGraph: PointedGraph[Sesame] = entityToPass.toPG

  "A CourseCRUDControllerSpec also " should {
    "return the corresponding course for a given lecturer" in {
      val lecturer = Employee("systemId", "last name", "first name", "email", Employee.randomUUID)

      val first = Course("label1", "abbreviation1", Employee.randomUUID, Course.randomUUID)
      val second = Course("label2", "abbreviation2", lecturer.id, Course.randomUUID)
      val third = Course("label3", "abbreviation3", Employee.randomUUID, Course.randomUUID)
      val fourth = Course("label4", "abbreviation4", Employee.randomUUID, Course.randomUUID)

      val courses = Set(first, second, third, fourth)

      when(repository.get[Course](anyObject(), anyObject())).thenReturn(Success(courses))

      val request = FakeRequest(
        GET,
        s"/${entityTypeName.toLowerCase}s?${CourseCRUDController.lecturerAttribute}=${lecturer.id.toString}"
      )

      val result = controller.asInstanceOf[CourseCRUDController].all()(request)

      status(result) shouldBe OK
      contentType(result) shouldBe Some[String](mimeType)
      contentAsString(result) shouldBe Json.toJson(Set(second)).toString
    }

    "return all corresponding courses for a given lecturer" in {
      val lecturer = Employee("systemId", "last name", "first name", "email", Employee.randomUUID)

      val first = Course("label1", "abbreviation1", lecturer.id, Course.randomUUID)
      val second = Course("label2", "abbreviation2", Employee.randomUUID, Course.randomUUID)
      val third = Course("label3", "abbreviation3", lecturer.id, Course.randomUUID)
      val fourth = Course("label4", "abbreviation4", Employee.randomUUID, Course.randomUUID)

      val courses = Set(first, second, third, fourth)

      when(repository.get[Course](anyObject(), anyObject())).thenReturn(Success(courses))

      val request = FakeRequest(
        GET,
        s"/${entityTypeName.toLowerCase}s?${CourseCRUDController.lecturerAttribute}=${lecturer.id.toString}"
      )

      val result = controller.asInstanceOf[CourseCRUDController].all()(request)

      status(result) shouldBe OK
      contentType(result) shouldBe Some[String](mimeType)
      contentAsString(result) shouldBe Json.toJson(Set(first, third)).toString
    }

    "not return courses for a lecturer when there is no match" in {
      val lecturer = Employee("systemId", "last name", "first name", "email", Employee.randomUUID)
      val expectedMessage = s"""{"status":"KO","message":"No such element..."}"""

      val first = Course("label1", "abbreviation1", Employee.randomUUID, Course.randomUUID)
      val second = Course("label2", "abbreviation2", Employee.randomUUID, Course.randomUUID)
      val third = Course("label3", "abbreviation3", Employee.randomUUID, Course.randomUUID)
      val fourth = Course("label4", "abbreviation4", Employee.randomUUID, Course.randomUUID)

      val courses = Set(first, second, third, fourth)

      when(repository.get[Course](anyObject(), anyObject())).thenReturn(Success(courses))

      val request = FakeRequest(
        GET,
        s"/${entityTypeName.toLowerCase}s?${CourseCRUDController.lecturerAttribute}=${lecturer.id.toString}"
      )

      val result = controller.asInstanceOf[CourseCRUDController].all()(request)

      status(result) shouldBe NOT_FOUND
      contentType(result) shouldBe Some("application/json")
      contentAsString(result) shouldBe expectedMessage
    }

    "not return courses when there is an invalid query attribute" in {
      val expectedErrorMessage = s"""{"status":"KO","message":"Unknown attribute"}"""

      val first = Course("label1", "abbreviation1", Employee.randomUUID, Course.randomUUID)
      val second = Course("label2", "abbreviation2", Employee.randomUUID, Course.randomUUID)
      val third = Course("label3", "abbreviation3", Employee.randomUUID, Course.randomUUID)
      val fourth = Course("label4", "abbreviation4", Employee.randomUUID, Course.randomUUID)

      val courses = Set(first, second, third, fourth)

      when(repository.get[Course](anyObject(), anyObject())).thenReturn(Success(courses))

      val request = FakeRequest(
        GET,
        s"/${entityTypeName.toLowerCase}s?invalidAttribute=${first.lecturer.toString}"
      )

      val result = controller.asInstanceOf[CourseCRUDController].all()(request)

      status(result) shouldBe BAD_REQUEST
      contentType(result) shouldBe Some("application/json")
      contentAsString(result) shouldBe expectedErrorMessage
    }

    "not return courses when there is an invalid query parameter value" in {
      val invalidParameter = "invalidParameterValue"
      val expectedErrorMessage = s"""{"status":"KO","message":"Invalid UUID string: $invalidParameter"}"""

      val first = Course("label1", "abbreviation1", Employee.randomUUID, Course.randomUUID)
      val second = Course("label2", "abbreviation2", Employee.randomUUID, Course.randomUUID)
      val third = Course("label3", "abbreviation3", Employee.randomUUID, Course.randomUUID)
      val fourth = Course("label4", "abbreviation4", Employee.randomUUID, Course.randomUUID)

      val courses = Set(first, second, third, fourth)

      when(repository.get[Course](anyObject(), anyObject())).thenReturn(Success(courses))

      val request = FakeRequest(
        GET,
        s"/${entityTypeName.toLowerCase}invalidParameter?${CourseCRUDController.lecturerAttribute}=$invalidParameter"
      )

      val result = controller.asInstanceOf[CourseCRUDController].all()(request)

      status(result) shouldBe BAD_REQUEST
      contentType(result) shouldBe Some("application/json")
      contentAsString(result) shouldBe expectedErrorMessage
    }
  }
}
