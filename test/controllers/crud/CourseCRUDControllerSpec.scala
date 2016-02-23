package controllers.crud

import java.util.UUID

import models.users.{Employee, User}
import models.{Course, CourseProtocol}
import org.mockito.Matchers
import org.mockito.Matchers._
import org.mockito.Mockito._
import org.w3.banana.PointedGraph
import org.w3.banana.sesame.Sesame
import play.api.http.HeaderNames
import play.api.libs.json.{JsValue, Json, Writes}
import play.api.test.{FakeHeaders, FakeRequest}
import play.api.test.Helpers._
import utils.LwmMimeType

import scala.util.Success
class CourseCRUDControllerSpec extends AbstractCRUDControllerSpec[CourseProtocol, Course] {

  override val entityToPass: Course = Course("label to pass", "description to pass", "abbreviation to pass", User.randomUUID, 1, Course.randomUUID)

  override val controller: AbstractCRUDController[CourseProtocol, Course] = new CourseCRUDController(repository, namespace, roleService) {

    override protected def fromInput(input: CourseProtocol, id: Option[UUID]) = entityToPass
  }

  override val entityToFail: Course = Course("label to fail", "description to fail", "abbreviation to fail", User.randomUUID, 1, Course.randomUUID)

  override implicit val jsonWrites: Writes[Course] = Course.writes

  override val mimeType: LwmMimeType = LwmMimeType.courseV1Json

  override val inputJson: JsValue = Json.obj(
    "label" -> entityToPass.label,
    "description" -> entityToPass.description,
    "abbreviation" -> entityToPass.abbreviation,
    "lecturer" -> entityToPass.lecturer,
    "semesterIndex" -> entityToPass.semesterIndex
  )

  override def entityTypeName: String = "course"

  import bindings.CourseBinding.courseBinder
  import ops._

  override def pointedGraph: PointedGraph[Sesame] = entityToPass.toPG

  override val updateJson: JsValue = Json.obj(
    "label" -> entityToPass.label,
    "description" -> (entityToPass.description + " updated"),
    "abbreviation" -> (entityToPass.abbreviation + " updated"),
    "lecturer" -> entityToPass.lecturer,
    "semesterIndex" -> entityToPass.semesterIndex
  )

  "A CourseCRUDControllerSpec also " should {
    "return the corresponding course for a given lecturer" in {
      val lecturer = Employee("systemId", "last name", "first name", "email", Employee.randomUUID)

      val first = Course("label1", "desc1", "abbreviation1", Employee.randomUUID, 1, Course.randomUUID)
      val second = Course("label2", "desc2", "abbreviation2", lecturer.id, 1, Course.randomUUID)
      val third = Course("label3", "desc3", "abbreviation3", Employee.randomUUID, 1, Course.randomUUID)
      val fourth = Course("label4", "desc4", "abbreviation4", Employee.randomUUID, 1, Course.randomUUID)

      val courses = Set(first, second, third, fourth)

      when(repository.get[Course](anyObject(), anyObject())).thenReturn(Success(courses))

      val request = FakeRequest(
        GET,
        s"/${entityTypeName.toLowerCase}s?${CourseCRUDController.lecturerAttribute}=${lecturer.id.toString}"
      )

      val result = controller.asInstanceOf[CourseCRUDController].all()(request)

      status(result) shouldBe OK
      contentType(result) shouldBe Some[String](mimeType)
      contentAsJson(result) shouldBe Json.toJson(Set(second))
    }

    "return all corresponding courses for a given lecturer" in {
      val lecturer = Employee("systemId", "last name", "first name", "email", Employee.randomUUID)

      val first = Course("label1", "desc1", "abbreviation1", lecturer.id, 1, Course.randomUUID)
      val second = Course("label2", "desc2", "abbreviation2", Employee.randomUUID, 1, Course.randomUUID)
      val third = Course("label3", "desc3", "abbreviation3", lecturer.id, 1, Course.randomUUID)
      val fourth = Course("label4", "desc4", "abbreviation4", Employee.randomUUID, 1, Course.randomUUID)

      val courses = Set(first, second, third, fourth)

      when(repository.get[Course](anyObject(), anyObject())).thenReturn(Success(courses))

      val request = FakeRequest(
        GET,
        s"/${entityTypeName.toLowerCase}s?${CourseCRUDController.lecturerAttribute}=${lecturer.id.toString}"
      )

      val result = controller.asInstanceOf[CourseCRUDController].all()(request)

      status(result) shouldBe OK
      contentType(result) shouldBe Some[String](mimeType)
      contentAsJson(result) shouldBe Json.toJson(Set(first, third))
    }

    "not return courses for a lecturer when there is no match" in {
      val lecturer = Employee("systemId", "last name", "first name", "email", Employee.randomUUID)

      val first = Course("label1", "desc1", "abbreviation1", Employee.randomUUID, 1, Course.randomUUID)
      val second = Course("label2", "desc2", "abbreviation2", Employee.randomUUID, 1, Course.randomUUID)
      val third = Course("label3", "desc3", "abbreviation3", Employee.randomUUID, 1, Course.randomUUID)
      val fourth = Course("label4", "desc4", "abbreviation4", Employee.randomUUID, 1, Course.randomUUID)

      val courses = Set(first, second, third, fourth)

      when(repository.get[Course](anyObject(), anyObject())).thenReturn(Success(courses))

      val request = FakeRequest(
        GET,
        s"/${entityTypeName.toLowerCase}s?${CourseCRUDController.lecturerAttribute}=${lecturer.id.toString}"
      )

      val result = controller.asInstanceOf[CourseCRUDController].all()(request)

      status(result) shouldBe NOT_FOUND
      contentType(result) shouldBe Some("application/json")
      contentAsJson(result) shouldBe Json.obj(
        "status" -> "KO",
        "message" -> "No such element..."
      )
    }

    "not return courses when there is an invalid query attribute" in {
      val first = Course("label1", "desc1", "abbreviation1", Employee.randomUUID, 1, Course.randomUUID)
      val second = Course("label2", "desc2", "abbreviation2", Employee.randomUUID, 1, Course.randomUUID)
      val third = Course("label3", "desc3", "abbreviation3", Employee.randomUUID, 1, Course.randomUUID)
      val fourth = Course("label4", "desc4", "abbreviation4", Employee.randomUUID, 1, Course.randomUUID)

      val courses = Set(first, second, third, fourth)

      when(repository.get[Course](anyObject(), anyObject())).thenReturn(Success(courses))

      val request = FakeRequest(
        GET,
        s"/${entityTypeName.toLowerCase}s?invalidAttribute=${first.lecturer.toString}"
      )

      val result = controller.asInstanceOf[CourseCRUDController].all()(request)

      status(result) shouldBe BAD_REQUEST
      contentType(result) shouldBe Some("application/json")
      contentAsJson(result) shouldBe Json.obj(
        "status" -> "KO",
        "message" -> "Unknown attribute"
      )
    }

    "not return courses when there is an invalid query parameter value" in {
      val invalidParameter = "invalidParameterValue"

      val first = Course("label1", "desc1", "abbreviation1", Employee.randomUUID, 1, Course.randomUUID)
      val second = Course("label2", "desc2", "abbreviation2", Employee.randomUUID, 1, Course.randomUUID)
      val third = Course("label3", "desc3", "abbreviation3", Employee.randomUUID, 1, Course.randomUUID)
      val fourth = Course("label4", "desc4", "abbreviation4", Employee.randomUUID, 1, Course.randomUUID)

      val courses = Set(first, second, third, fourth)

      when(repository.get[Course](anyObject(), anyObject())).thenReturn(Success(courses))

      val request = FakeRequest(
        GET,
        s"/${entityTypeName.toLowerCase}invalidParameter?${CourseCRUDController.lecturerAttribute}=$invalidParameter"
      )

      val result = controller.asInstanceOf[CourseCRUDController].all()(request)

      status(result) shouldBe BAD_REQUEST
      contentType(result) shouldBe Some("application/json")
      contentAsJson(result) shouldBe Json.obj(
        "status" -> "KO",
        "message" -> s"Invalid UUID string: $invalidParameter"
      )
    }

    s"handle this model issue when creating a new $entityTypeName which already exists" in {
      when(repository.query(anyObject())).thenReturn(Some(Map(
        "id" -> List(factory.createLiteral(entityToPass.id.toString))
      )))

      val request = FakeRequest(
        POST,
        s"/${entityTypeName}s",
        FakeHeaders(Seq(HeaderNames.CONTENT_TYPE -> mimeType)),
        inputJson
      )
      val result = controller.create()(request)

      status(result) shouldBe ACCEPTED
      contentType(result) shouldBe Some("application/json")
      contentAsJson(result) shouldBe Json.obj(
        "status" -> "KO",
        "message" -> "model already exists",
        "id" -> entityToPass.id
      )
    }

    s"neither create or update an existing $entityTypeName when resource does not exists although body would lead to duplication" in {
      when(repository.get[Course](anyObject())(anyObject())).thenReturn(Success(None))
      when(repository.query(Matchers.anyObject())).thenReturn(Some(Map(
        "id" -> List(factory.createLiteral(entityToPass.id.toString))
      )))

      val request = FakeRequest(
        PUT,
        s"/${entityTypeName}s/${entityToPass.id}",
        FakeHeaders(Seq(HeaderNames.CONTENT_TYPE -> mimeType)),
        updateJson
      )
      val result = controller.update(entityToPass.id.toString)(request)

      status(result) shouldBe ACCEPTED
      contentType(result) shouldBe Some("application/json")
      contentAsJson(result) shouldBe Json.obj(
        "status" -> "KO",
        "message" -> "model already exists",
        "id" -> entityToPass.id
      )
    }
  }
}
