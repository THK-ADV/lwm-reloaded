package controllers.crud

import java.net.URLEncoder
import java.util.UUID

import models._
import models.applications.{LabworkApplication, LabworkApplicationProtocol}
import models.semester.Semester
import models.users.Student
import org.joda.time.DateTime
import org.w3.banana.PointedGraph
import org.w3.banana.sesame.Sesame
import play.api.libs.json.{Json, Writes, JsValue}
import play.api.mvc.{Action, Result, AnyContent, Request}
import play.api.test.FakeRequest
import store.SesameRepository
import utils.LWMActions.ContentTypedAction
import utils.LwmMimeType
import play.api.test.Helpers._
import org.mockito.Matchers._
import org.mockito.Mockito._
import LabworkApplicationCRUDController._

import scala.concurrent.Future
import scala.util.{Try, Success}

class LabworkApplicationCRUDControllerSpec extends AbstractCRUDControllerSpec[LabworkApplicationProtocol, LabworkApplication] {

  import bindings.LabworkApplicationBinding._
  import bindings.uuidBinder
  import bindings.jodaDateTimeBinder
  import ops._

  override val mimeType: LwmMimeType = LwmMimeType.labworkApplicationV1Json

  override val controller: AbstractCRUDController[LabworkApplicationProtocol, LabworkApplication] = new LabworkApplicationCRUDController(repository, namespace, roleService) {

    override protected def fromInput(input: LabworkApplicationProtocol, id: Option[UUID]): LabworkApplication = entityToPass
  }

  override val entityToFail: LabworkApplication = LabworkApplication(Labwork.randomUUID, Student.randomUUID, Set(Student.randomUUID))

  override val entityToPass: LabworkApplication = LabworkApplication(Labwork.randomUUID, Student.randomUUID, Set(Student.randomUUID))

  override val pointedGraph: PointedGraph[Sesame] = entityToPass.toPG

  override implicit val jsonWrites: Writes[LabworkApplication] = LabworkApplication.writes

  override val inputJson: JsValue = Json.obj(
    "labwork" -> entityToPass.labwork,
    "applicant" -> entityToPass.applicant,
    "friends" -> entityToPass.friends
  )

  override val updateJson: JsValue = Json.obj(
    "labwork" -> entityToPass.labwork,
    "applicant" -> entityToPass.applicant,
    "friends" -> (entityToPass.friends + Student.randomUUID + Student.randomUUID)
  )

  override def entityTypeName: String = "labworkApplication"

  def encode(s: String) = URLEncoder.encode(s, "UTF-8")

  "A LabworkApplicationCRUDControllerSpec" should {

    "return the corresponding labworkApplication for a given labwork" in {
      val plan = AssignmentPlan(1, Set(AssignmentEntry(1, Set(EntryType("type")))))
      val labwork = Labwork("label 1", "description 1", Semester.randomUUID, Course.randomUUID, Degree.randomUUID, plan, Labwork.randomUUID)

      val first = LabworkApplication(labwork.id, Student.randomUUID, Set(Student.randomUUID))
      val second = LabworkApplication(Labwork.randomUUID, Student.randomUUID, Set(Student.randomUUID))
      val third = LabworkApplication(Labwork.randomUUID, Student.randomUUID, Set(Student.randomUUID))
      val fourth = LabworkApplication(Labwork.randomUUID, Student.randomUUID, Set(Student.randomUUID))

      val applications = Set(first, second, third, fourth)

      when(repository.get[LabworkApplication](anyObject(), anyObject())).thenReturn(Try(applications))

      val request = FakeRequest(
        GET,
        s"/${entityTypeName}s?$labworkAttribute=${labwork.id.toString}"
      )

      val result = controller.asInstanceOf[LabworkApplicationCRUDController].all()(request)

      status(result) shouldBe OK
      contentType(result) shouldBe Some[String](mimeType)
      contentAsString(result) shouldBe Json.toJson(Set(first)).toString
    }
  }

  "return all corresponding applications for a given labwork" in {
    val plan = AssignmentPlan(1, Set(AssignmentEntry(1, Set(EntryType("type")))))
    val labwork = Labwork("label 1", "description 1", Semester.randomUUID, Course.randomUUID, Degree.randomUUID, plan, Labwork.randomUUID)

    val first = LabworkApplication(labwork.id, Student.randomUUID, Set(Student.randomUUID))
    val second = LabworkApplication(Labwork.randomUUID, Student.randomUUID, Set(Student.randomUUID))
    val third = LabworkApplication(labwork.id, Student.randomUUID, Set(Student.randomUUID))
    val fourth = LabworkApplication(Labwork.randomUUID, Student.randomUUID, Set(Student.randomUUID))

    val applications = Set(first, second, third, fourth)

    when(repository.get[LabworkApplication](anyObject(), anyObject())).thenReturn(Try(applications))

    val request = FakeRequest(
      GET,
      s"/${entityTypeName}s?$labworkAttribute=${labwork.id.toString}"
    )

    val result = controller.asInstanceOf[LabworkApplicationCRUDController].all()(request)

    status(result) shouldBe OK
    contentType(result) shouldBe Some[String](mimeType)
    contentAsString(result) shouldBe Json.toJson(Set(first, third)).toString
  }

  "not return applications for a labwork when there is no match" in {
    val expectedMessage = s"""{"status":"KO","message":"No such element..."}"""
    val plan = AssignmentPlan(1, Set(AssignmentEntry(1, Set(EntryType("type")))))
    val labwork = Labwork("label 1", "description 1", Semester.randomUUID, Course.randomUUID, Degree.randomUUID, plan, Labwork.randomUUID)

    val first = LabworkApplication(Labwork.randomUUID, Student.randomUUID, Set(Student.randomUUID))
    val second = LabworkApplication(Labwork.randomUUID, Student.randomUUID, Set(Student.randomUUID))
    val third = LabworkApplication(Labwork.randomUUID, Student.randomUUID, Set(Student.randomUUID))
    val fourth = LabworkApplication(Labwork.randomUUID, Student.randomUUID, Set(Student.randomUUID))

    val applications = Set(first, second, third, fourth)

    when(repository.get[LabworkApplication](anyObject(), anyObject())).thenReturn(Try(applications))

    val request = FakeRequest(
      GET,
      s"/${entityTypeName}s?$labworkAttribute=${labwork.id.toString}"
    )

    val result = controller.asInstanceOf[LabworkApplicationCRUDController].all()(request)

    status(result) shouldBe NOT_FOUND
    contentType(result) shouldBe Some("application/json")
    contentAsString(result) shouldBe expectedMessage
  }

  "not return applications when there is an invalid query attribute" in {
    val expectedMessage = s"""{"status":"KO","message":"Unknown attribute"}"""
    val plan = AssignmentPlan(1, Set(AssignmentEntry(1, Set(EntryType("type")))))
    val labwork = Labwork("label 1", "description 1", Semester.randomUUID, Course.randomUUID, Degree.randomUUID, plan, Labwork.randomUUID)

    val first = LabworkApplication(Labwork.randomUUID, Student.randomUUID, Set(Student.randomUUID))
    val second = LabworkApplication(Labwork.randomUUID, Student.randomUUID, Set(Student.randomUUID))
    val third = LabworkApplication(Labwork.randomUUID, Student.randomUUID, Set(Student.randomUUID))
    val fourth = LabworkApplication(Labwork.randomUUID, Student.randomUUID, Set(Student.randomUUID))

    val applications = Set(first, second, third, fourth)

    when(repository.get[LabworkApplication](anyObject(), anyObject())).thenReturn(Try(applications))

    val request = FakeRequest(
      GET,
      s"/${entityTypeName}s?unknownAttribute=attributeValue"
    )

    val result = controller.asInstanceOf[LabworkApplicationCRUDController].all()(request)

    status(result) shouldBe BAD_REQUEST
    contentType(result) shouldBe Some("application/json")
    contentAsString(result) shouldBe expectedMessage
  }

  "not return applications when there is an invalid query parameter value" in {
    val invalidParameter = "invalidParameterValue"
    val expectedErrorMessage = s"""{"status":"KO","message":"Invalid UUID string: $invalidParameter"}"""

    val plan = AssignmentPlan(1, Set(AssignmentEntry(1, Set(EntryType("type")))))
    val labwork = Labwork("label 1", "description 1", Semester.randomUUID, Course.randomUUID, Degree.randomUUID, plan, Labwork.randomUUID)

    val first = LabworkApplication(Labwork.randomUUID, Student.randomUUID, Set(Student.randomUUID))
    val second = LabworkApplication(Labwork.randomUUID, Student.randomUUID, Set(Student.randomUUID))
    val third = LabworkApplication(Labwork.randomUUID, Student.randomUUID, Set(Student.randomUUID))
    val fourth = LabworkApplication(Labwork.randomUUID, Student.randomUUID, Set(Student.randomUUID))

    val applications = Set(first, second, third, fourth)

    when(repository.get[LabworkApplication](anyObject(), anyObject())).thenReturn(Try(applications))

    val request = FakeRequest(
      GET,
      s"/${entityTypeName}s?$labworkAttribute=invalidParameterValue"
    )

    val result = controller.asInstanceOf[LabworkApplicationCRUDController].all()(request)

    status(result) shouldBe BAD_REQUEST
    contentType(result) shouldBe Some("application/json")
    contentAsString(result) shouldBe expectedErrorMessage
  }

  "return the corresponding application for a given applicant" in {
    val applicant = Student("id", "lastname", "firstname", "email", "registrationId", Degree.randomUUID, Student.randomUUID)

    val first = LabworkApplication(Labwork.randomUUID, applicant.id, Set(Student.randomUUID))
    val second = LabworkApplication(Labwork.randomUUID, Student.randomUUID, Set(Student.randomUUID))
    val third = LabworkApplication(Labwork.randomUUID, Student.randomUUID, Set(Student.randomUUID))
    val fourth = LabworkApplication(Labwork.randomUUID, Student.randomUUID, Set(Student.randomUUID))

    val applications = Set(first, second, third, fourth)

    when(repository.get[LabworkApplication](anyObject(), anyObject())).thenReturn(Try(applications))

    val request = FakeRequest(
      GET,
      s"/${entityTypeName}s?$applicantAttribute=${applicant.id.toString}"
    )

    val result = controller.asInstanceOf[LabworkApplicationCRUDController].all()(request)

    status(result) shouldBe OK
    contentType(result) shouldBe Some[String](mimeType)
    contentAsString(result) shouldBe Json.toJson(Set(first)).toString
  }
  "return all corresponding applications for a given applicant" in {
    val applicant = Student("id", "lastname", "firstname", "email", "registrationId", Degree.randomUUID, Student.randomUUID)

    val first = LabworkApplication(Labwork.randomUUID, applicant.id, Set(Student.randomUUID))
    val second = LabworkApplication(Labwork.randomUUID, Student.randomUUID, Set(Student.randomUUID))
    val third = LabworkApplication(Labwork.randomUUID, applicant.id, Set(Student.randomUUID))
    val fourth = LabworkApplication(Labwork.randomUUID, Student.randomUUID, Set(Student.randomUUID))

    val applications = Set(first, second, third, fourth)

    when(repository.get[LabworkApplication](anyObject(), anyObject())).thenReturn(Try(applications))

    val request = FakeRequest(
      GET,
      s"/${entityTypeName}s?$applicantAttribute=${applicant.id.toString}"
    )

    val result = controller.asInstanceOf[LabworkApplicationCRUDController].all()(request)

    status(result) shouldBe OK
    contentType(result) shouldBe Some[String](mimeType)
    contentAsString(result) shouldBe Json.toJson(Set(first, third)).toString
  }

  "not return applications for an applicant when there is no match" in {
    val expectedMessage = s"""{"status":"KO","message":"No such element..."}"""
    val applicant = Student("id", "lastname", "firstname", "email", "registrationId", Degree.randomUUID, Student.randomUUID)

    val first = LabworkApplication(Labwork.randomUUID, Student.randomUUID, Set(Student.randomUUID))
    val second = LabworkApplication(Labwork.randomUUID, Student.randomUUID, Set(Student.randomUUID))
    val third = LabworkApplication(Labwork.randomUUID, Student.randomUUID, Set(Student.randomUUID))
    val fourth = LabworkApplication(Labwork.randomUUID, Student.randomUUID, Set(Student.randomUUID))

    val applications = Set(first, second, third, fourth)

    when(repository.get[LabworkApplication](anyObject(), anyObject())).thenReturn(Try(applications))

    val request = FakeRequest(
      GET,
      s"/${entityTypeName}s?$applicantAttribute=${applicant.id.toString}"
    )

    val result = controller.asInstanceOf[LabworkApplicationCRUDController].all()(request)

    status(result) shouldBe NOT_FOUND
    contentType(result) shouldBe Some("application/json")
    contentAsString(result) shouldBe expectedMessage
  }

  "return the corresponding application for a given friend" in {
    val friend = Student("id", "lastname", "firstname", "email", "registrationId", Degree.randomUUID, Student.randomUUID)

    val first = LabworkApplication(Labwork.randomUUID, Student.randomUUID, Set(friend.id))
    val second = LabworkApplication(Labwork.randomUUID, Student.randomUUID, Set(Student.randomUUID))
    val third = LabworkApplication(Labwork.randomUUID, Student.randomUUID, Set(Student.randomUUID))
    val fourth = LabworkApplication(Labwork.randomUUID, Student.randomUUID, Set(Student.randomUUID))

    val applications = Set(first, second, third, fourth)

    when(repository.get[LabworkApplication](anyObject(), anyObject())).thenReturn(Try(applications))

    val request = FakeRequest(
      GET,
      s"/${entityTypeName}s?$friendAttribute=${friend.id.toString}"
    )

    val result = controller.asInstanceOf[LabworkApplicationCRUDController].all()(request)

    status(result) shouldBe OK
    contentType(result) shouldBe Some[String](mimeType)
    contentAsString(result) shouldBe Json.toJson(Set(first)).toString
  }
  "return all corresponding applications for a given friend" in {
    val friend = Student("id", "lastname", "firstname", "email", "registrationId", Degree.randomUUID, Student.randomUUID)

    val first = LabworkApplication(Labwork.randomUUID, Student.randomUUID, Set(friend.id, Student.randomUUID))
    val second = LabworkApplication(Labwork.randomUUID, Student.randomUUID, Set(Student.randomUUID))
    val third = LabworkApplication(Labwork.randomUUID, Student.randomUUID, Set(friend.id))
    val fourth = LabworkApplication(Labwork.randomUUID, Student.randomUUID, Set(Student.randomUUID))

    val applications = Set(first, second, third, fourth)

    when(repository.get[LabworkApplication](anyObject(), anyObject())).thenReturn(Try(applications))

    val request = FakeRequest(
      GET,
      s"/${entityTypeName}s?$friendAttribute=${friend.id.toString}"
    )

    val result = controller.asInstanceOf[LabworkApplicationCRUDController].all()(request)

    status(result) shouldBe OK
    contentType(result) shouldBe Some[String](mimeType)
    contentAsString(result) shouldBe Json.toJson(Set(first, third)).toString
  }

  "not return applications for a friend  when there is no match" in {
    val expectedMessage = s"""{"status":"KO","message":"No such element..."}"""
    val friend = Student("id", "lastname", "firstname", "email", "registrationId", Degree.randomUUID, Student.randomUUID)

    val first = LabworkApplication(Labwork.randomUUID, Student.randomUUID, Set(Student.randomUUID))
    val second = LabworkApplication(Labwork.randomUUID, Student.randomUUID, Set(Student.randomUUID))
    val third = LabworkApplication(Labwork.randomUUID, Student.randomUUID, Set(Student.randomUUID))
    val fourth = LabworkApplication(Labwork.randomUUID, Student.randomUUID, Set(Student.randomUUID))

    val applications = Set(first, second, third, fourth)

    when(repository.get[LabworkApplication](anyObject(), anyObject())).thenReturn(Try(applications))

    val request = FakeRequest(
      GET,
      s"/${entityTypeName}s?$friendAttribute=${friend.id.toString}"
    )

    val result = controller.asInstanceOf[LabworkApplicationCRUDController].all()(request)

    status(result) shouldBe NOT_FOUND
    contentType(result) shouldBe Some("application/json")
    contentAsString(result) shouldBe expectedMessage
  }

  "return the corresponding application for a given date" in {
    val time = DateTime.now
    val tthen = DateTime.now.plusDays(2)

    val first = LabworkApplication(Labwork.randomUUID, Student.randomUUID, Set(Student.randomUUID), time)
    val second = LabworkApplication(Labwork.randomUUID, Student.randomUUID, Set(Student.randomUUID), tthen)
    val third = LabworkApplication(Labwork.randomUUID, Student.randomUUID, Set(Student.randomUUID), tthen)
    val fourth = LabworkApplication(Labwork.randomUUID, Student.randomUUID, Set(Student.randomUUID), tthen)
    val applications = Set(first, second, third, fourth)

    when(repository.get[LabworkApplication](anyObject(), anyObject())).thenReturn(Try(applications))

    val request = FakeRequest(
      GET,
      s"/${entityTypeName}s?$dateAttribute=${time.toString("yyyy-MM-dd")}"
    )

    val result = controller.asInstanceOf[LabworkApplicationCRUDController].all()(request)

    status(result) shouldBe OK
    contentType(result) shouldBe Some[String](mimeType)
    contentAsString(result) shouldBe Json.toJson(Set(first)).toString
  }

  "return all corresponding applications for a given date" in {
    val time = DateTime.now
    val tthen = DateTime.now.plusDays(2)

    val first = LabworkApplication(Labwork.randomUUID, Student.randomUUID, Set(Student.randomUUID), time)
    val second = LabworkApplication(Labwork.randomUUID, Student.randomUUID, Set(Student.randomUUID), tthen)
    val third = LabworkApplication(Labwork.randomUUID, Student.randomUUID, Set(Student.randomUUID), time)
    val fourth = LabworkApplication(Labwork.randomUUID, Student.randomUUID, Set(Student.randomUUID), tthen)

    val applications = Set(first, second, third, fourth)

    when(repository.get[LabworkApplication](anyObject(), anyObject())).thenReturn(Try(applications))

    val request = FakeRequest(
      GET,
      s"/${entityTypeName}s?$dateAttribute=${time.toString("yyyy-MM-dd")}"
    )

    val result = controller.asInstanceOf[LabworkApplicationCRUDController].all()(request)


    status(result) shouldBe OK
    contentType(result) shouldBe Some[String](mimeType)
    contentAsString(result) shouldBe Json.toJson(Set(first, third)).toString
  }

  "not return applications for a date when there is no match" in {
    val expectedMessage = s"""{"status":"KO","message":"No such element..."}"""
    val time = DateTime.now.plusDays(2)

    val first = LabworkApplication(Labwork.randomUUID, Student.randomUUID, Set(Student.randomUUID))
    val second = LabworkApplication(Labwork.randomUUID, Student.randomUUID, Set(Student.randomUUID))
    val third = LabworkApplication(Labwork.randomUUID, Student.randomUUID, Set(Student.randomUUID))
    val fourth = LabworkApplication(Labwork.randomUUID, Student.randomUUID, Set(Student.randomUUID))

    val applications = Set(first, second, third, fourth)

    when(repository.get[LabworkApplication](anyObject(), anyObject())).thenReturn(Try(applications))

    val request = FakeRequest(
      GET,
      s"/${entityTypeName}s?$dateAttribute=${time.toString("yyyy-MM-dd")}"
    )

    val result = controller.asInstanceOf[LabworkApplicationCRUDController].all()(request)

    status(result) shouldBe NOT_FOUND
    contentType(result) shouldBe Some("application/json")
    contentAsString(result) shouldBe expectedMessage
  }


  "return the corresponding application for a given dateTime range" in {
    val now = DateTime.now
    val shortly = DateTime.now.plusDays(1)
    val after = DateTime.now.plusDays(2)

    val first = LabworkApplication(Labwork.randomUUID, Student.randomUUID, Set(Student.randomUUID), now)
    val second = LabworkApplication(Labwork.randomUUID, Student.randomUUID, Set(Student.randomUUID), after)
    val third = LabworkApplication(Labwork.randomUUID, Student.randomUUID, Set(Student.randomUUID), after)
    val fourth = LabworkApplication(Labwork.randomUUID, Student.randomUUID, Set(Student.randomUUID), after)
    val applications = Set(first, second, third, fourth)

    when(repository.get[LabworkApplication](anyObject(), anyObject())).thenReturn(Try(applications))

    val request = FakeRequest(
      GET,
      s"/${entityTypeName}s?$minTime=${encode(now.toString("yyyy-MM-dd'T'HH:mm"))}&$maxTime=${encode(shortly.toString("yyyy-MM-dd'T'HH:mm"))}"
    )

    val result = controller.asInstanceOf[LabworkApplicationCRUDController].all()(request)

    status(result) shouldBe OK
    contentType(result) shouldBe Some[String](mimeType)
    contentAsString(result) shouldBe Json.toJson(Set(first)).toString
  }

  "return all corresponding applications for a given dateTime range" in {
    val now = DateTime.now
    val shortly = DateTime.now.plusDays(1)
    val after = DateTime.now.plusDays(2)

    val first = LabworkApplication(Labwork.randomUUID, Student.randomUUID, Set(Student.randomUUID), now)
    val second = LabworkApplication(Labwork.randomUUID, Student.randomUUID, Set(Student.randomUUID), after)
    val third = LabworkApplication(Labwork.randomUUID, Student.randomUUID, Set(Student.randomUUID), now.plusHours(8))
    val fourth = LabworkApplication(Labwork.randomUUID, Student.randomUUID, Set(Student.randomUUID), after)

    val applications = Set(first, second, third, fourth)

    when(repository.get[LabworkApplication](anyObject(), anyObject())).thenReturn(Try(applications))

    val request = FakeRequest(
      GET,
      s"/${entityTypeName}s?$minTime=${encode(now.toString("yyyy-MM-dd'T'HH:mm"))}&$maxTime=${encode(shortly.toString("yyyy-MM-dd'T'HH:mm"))}"
    )

    val result = controller.asInstanceOf[LabworkApplicationCRUDController].all()(request)


    status(result) shouldBe OK
    contentType(result) shouldBe Some[String](mimeType)
    contentAsString(result) shouldBe Json.toJson(Set(first, third)).toString
  }

  "not return applications for a dateTime range when there is no match" in {
    val expectedMessage = s"""{"status":"KO","message":"No such element..."}"""
    val now = DateTime.now.plusDays(2)
    val shortly = DateTime.now.plusDays(3)

    val first = LabworkApplication(Labwork.randomUUID, Student.randomUUID, Set(Student.randomUUID))
    val second = LabworkApplication(Labwork.randomUUID, Student.randomUUID, Set(Student.randomUUID))
    val third = LabworkApplication(Labwork.randomUUID, Student.randomUUID, Set(Student.randomUUID))
    val fourth = LabworkApplication(Labwork.randomUUID, Student.randomUUID, Set(Student.randomUUID))

    val applications = Set(first, second, third, fourth)

    when(repository.get[LabworkApplication](anyObject(), anyObject())).thenReturn(Try(applications))

    val request = FakeRequest(
      GET,
      s"/${entityTypeName}s?$minTime=${encode(now.toString("yyyy-MM-dd'T'HH:mm"))}&$maxTime=${encode(shortly.toString("yyyy-MM-dd'T'HH:mm"))}"
    )

    val result = controller.asInstanceOf[LabworkApplicationCRUDController].all()(request)

    status(result) shouldBe NOT_FOUND
    contentType(result) shouldBe Some("application/json")
    contentAsString(result) shouldBe expectedMessage
  }

  "return the corresponding application for a given min or max time bound" in {
    val now = DateTime.now
    val before = DateTime.now.minusDays(2)

    val first = LabworkApplication(Labwork.randomUUID, Student.randomUUID, Set(Student.randomUUID), now)
    val second = LabworkApplication(Labwork.randomUUID, Student.randomUUID, Set(Student.randomUUID), before)
    val third = LabworkApplication(Labwork.randomUUID, Student.randomUUID, Set(Student.randomUUID), before)
    val fourth = LabworkApplication(Labwork.randomUUID, Student.randomUUID, Set(Student.randomUUID), before)
    val applications = Set(first, second, third, fourth)

    when(repository.get[LabworkApplication](anyObject(), anyObject())).thenReturn(Try(applications))

    val request1 = FakeRequest(
      GET,
      s"/${entityTypeName}s?$minTime=${encode(now.toString("yyyy-MM-dd'T'HH:mm"))}"
    )

    val request2 = FakeRequest(
      GET,
      s"/${entityTypeName}s?$maxTime=${encode(now.toString("yyyy-MM-dd'T'HH:mm"))}"
    )

    val result1 = controller.asInstanceOf[LabworkApplicationCRUDController].all()(request1)
    val result2 = controller.asInstanceOf[LabworkApplicationCRUDController].all()(request2)

    status(result1) shouldBe OK
    contentType(result1) shouldBe Some[String](mimeType)
    contentAsString(result1) shouldBe Json.toJson(Set(first)).toString

    status(result2) shouldBe OK
    contentType(result2) shouldBe Some[String](mimeType)
    contentAsString(result2) shouldBe Json.toJson(Set(second, third, fourth)).toString
  }

  "return all corresponding applications for a given min or max time bound" in {
    val now = DateTime.now
    val before = DateTime.now.minusDays(2)

    val first = LabworkApplication(Labwork.randomUUID, Student.randomUUID, Set(Student.randomUUID), now)
    val second = LabworkApplication(Labwork.randomUUID, Student.randomUUID, Set(Student.randomUUID), before)
    val third = LabworkApplication(Labwork.randomUUID, Student.randomUUID, Set(Student.randomUUID), now.plusHours(8))
    val fourth = LabworkApplication(Labwork.randomUUID, Student.randomUUID, Set(Student.randomUUID), before)

    val applications = Set(first, second, third, fourth)

    when(repository.get[LabworkApplication](anyObject(), anyObject())).thenReturn(Try(applications))

    val request1 = FakeRequest(
      GET,
      s"/${entityTypeName}s?$minTime=${encode(now.toString("yyyy-MM-dd'T'HH:mm"))}"
    )

    val request2 = FakeRequest(
      GET,
      s"/${entityTypeName}s?$maxTime=${encode(now.toString("yyyy-MM-dd'T'HH:mm"))}"
    )

    val result1 = controller.asInstanceOf[LabworkApplicationCRUDController].all()(request1)
    val result2 = controller.asInstanceOf[LabworkApplicationCRUDController].all()(request2)


    status(result1) shouldBe OK
    contentType(result1) shouldBe Some[String](mimeType)
    contentAsString(result1) shouldBe Json.toJson(Set(first, third)).toString

    status(result2) shouldBe OK
    contentType(result2) shouldBe Some[String](mimeType)
    contentAsString(result2) shouldBe Json.toJson(Set(second, fourth)).toString
  }

  "not return applications for a min or max bound when there is no match" in {
    val expectedMessage = s"""{"status":"KO","message":"No such element..."}"""
    val now = DateTime.now
    val before = DateTime.now.minusDays(2)

    val first = LabworkApplication(Labwork.randomUUID, Student.randomUUID, Set(Student.randomUUID), before)
    val second = LabworkApplication(Labwork.randomUUID, Student.randomUUID, Set(Student.randomUUID), before)
    val third = LabworkApplication(Labwork.randomUUID, Student.randomUUID, Set(Student.randomUUID), before)
    val fourth = LabworkApplication(Labwork.randomUUID, Student.randomUUID, Set(Student.randomUUID), before)

    val applications = Set(first, second, third, fourth)

    when(repository.get[LabworkApplication](anyObject(), anyObject())).thenReturn(Try(applications))

    val request1 = FakeRequest(
      GET,
      s"/${entityTypeName}s?$minTime=${encode(now.toString("yyyy-MM-dd'T'HH:mm"))}"
    )

    val request2 = FakeRequest(
      GET,
      s"/${entityTypeName}s?$maxTime=${encode(before.toString("yyyy-MM-dd'T'HH:mm"))}"
    )

    val result1 = controller.asInstanceOf[LabworkApplicationCRUDController].all()(request1)
    val result2 = controller.asInstanceOf[LabworkApplicationCRUDController].all()(request2)

    status(result1) shouldBe NOT_FOUND
    contentType(result1) shouldBe Some("application/json")
    contentAsString(result1) shouldBe expectedMessage

    status(result2) shouldBe NOT_FOUND
    contentType(result2) shouldBe Some("application/json")
    contentAsString(result2) shouldBe expectedMessage
  }
}
