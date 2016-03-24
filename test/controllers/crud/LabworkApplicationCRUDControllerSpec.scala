package controllers.crud

import java.net.URLEncoder
import java.util.UUID

import models._
import models.applications.{LabworkApplicationAtom, LabworkApplication, LabworkApplicationProtocol}
import models.semester.Semester
import models.users.{User, Student}
import org.joda.time.DateTime
import org.w3.banana.PointedGraph
import org.w3.banana.sesame.Sesame
import play.api.libs.json.{Json, Writes, JsValue}
import play.api.test.FakeRequest
import utils.LwmMimeType
import play.api.test.Helpers._
import org.mockito.Matchers._
import org.mockito.Mockito._
import LabworkApplicationCRUDController._

import scala.util.{Failure, Success, Try}

class LabworkApplicationCRUDControllerSpec extends AbstractCRUDControllerSpec[LabworkApplicationProtocol, LabworkApplication] {

  val labworkToPass = Labwork("label to pass", "desc to pass", UUID.randomUUID(), UUID.randomUUID(), UUID.randomUUID())
  val labworkToFail = Labwork("label to fail", "desc to fail", UUID.randomUUID(), UUID.randomUUID(), UUID.randomUUID())

  val applicantToPass = Student("systemId to pass", "last name to pass", "first name to pass", "email to pass", "regId to pass", UUID.randomUUID())
  val applicantToFail = Student("systemId to fail", "last name to fail", "first name to fail", "email to fail", "regId to fail", UUID.randomUUID())

  val friendsToPass = Set(
    Student("systemId to pass", "last name to pass", "first name to pass", "email to pass", "regId to pass", UUID.randomUUID()),
    Student("systemId to pass", "last name to pass", "first name to pass", "email to pass", "regId to pass", UUID.randomUUID())
  )
  val friendsToFail = Set(
    Student("systemId to pass", "last name to pass", "first name to pass", "email to pass", "regId to pass", UUID.randomUUID()),
    Student("systemId to pass", "last name to pass", "first name to pass", "email to pass", "regId to pass", UUID.randomUUID())
  )
  override val mimeType: LwmMimeType = LwmMimeType.labworkApplicationV1Json

  override val controller: AbstractCRUDController[LabworkApplicationProtocol, LabworkApplication] = new LabworkApplicationCRUDController(repository, sessionService, namespace, roleService) {

    override protected def fromInput(input: LabworkApplicationProtocol, existing: Option[LabworkApplication]): LabworkApplication = entityToPass

    override protected def contextFrom: PartialFunction[Rule, SecureContext] = {
      case _ => NonSecureBlock
    }
  }

  override val entityToFail: LabworkApplication = LabworkApplication(labworkToFail.id, applicantToFail.id, friendsToFail.map(_.id))

  override val entityToPass: LabworkApplication = LabworkApplication(labworkToPass.id, applicantToPass.id, friendsToPass.map(_.id))

  override implicit val jsonWrites: Writes[LabworkApplication] = LabworkApplication.writes

  import bindings.LabworkApplicationBinding._
  import bindings.uuidBinder
  import bindings.jodaDateTimeBinder
  import ops._

  override val pointedGraph: PointedGraph[Sesame] = entityToPass.toPG

  override val inputJson: JsValue = Json.obj(
    "labwork" -> entityToPass.labwork,
    "applicant" -> entityToPass.applicant,
    "friends" -> entityToPass.friends
  )

  override val updateJson: JsValue = Json.obj(
    "labwork" -> entityToPass.labwork,
    "applicant" -> entityToPass.applicant,
    "friends" -> (entityToPass.friends + User.randomUUID + User.randomUUID)
  )

  val atomizedEntityToPass = LabworkApplicationAtom(labworkToPass, applicantToPass, friendsToPass, entityToPass.timestamp, entityToPass.id)
  val atomizedEntityToFail = LabworkApplicationAtom(labworkToFail, applicantToFail, friendsToFail, entityToFail.timestamp, entityToFail.id)

  val emptyLabworkApps = Set.empty[LabworkApplication]

  override def entityTypeName: String = "labworkApplication"

  def encode(s: String) = URLEncoder.encode(s, "UTF-8")

  "A LabworkApplicationCRUDControllerSpec" should {

    "return the corresponding labworkApplication for a given labwork" in {
      val labwork = Labwork("label 1", "description 1", Semester.randomUUID, Course.randomUUID, Degree.randomUUID)

      val first = LabworkApplication(labwork.id, User.randomUUID, Set(User.randomUUID))
      val second = LabworkApplication(Labwork.randomUUID, User.randomUUID, Set(User.randomUUID))
      val third = LabworkApplication(Labwork.randomUUID, User.randomUUID, Set(User.randomUUID))
      val fourth = LabworkApplication(Labwork.randomUUID, User.randomUUID, Set(User.randomUUID))

      val applications = Set(first, second, third, fourth)

      when(repository.get[LabworkApplication](anyObject(), anyObject())).thenReturn(Try(applications))

      val request = FakeRequest(
        GET,
        s"/${entityTypeName}s?$labworkAttribute=${labwork.id.toString}"
      )

      val result = controller.asInstanceOf[LabworkApplicationCRUDController].all()(request)

      status(result) shouldBe OK
      contentType(result) shouldBe Some[String](mimeType)
      contentAsJson(result) shouldBe Json.toJson(Set(first))
    }
  }

  "return all corresponding applications for a given labwork" in {
    val labwork = Labwork("label 1", "description 1", Semester.randomUUID, Course.randomUUID, Degree.randomUUID)

    val first = LabworkApplication(labwork.id, User.randomUUID, Set(User.randomUUID))
    val second = LabworkApplication(Labwork.randomUUID, User.randomUUID, Set(User.randomUUID))
    val third = LabworkApplication(labwork.id, User.randomUUID, Set(User.randomUUID))
    val fourth = LabworkApplication(Labwork.randomUUID, User.randomUUID, Set(User.randomUUID))

    val applications = Set(first, second, third, fourth)

    when(repository.get[LabworkApplication](anyObject(), anyObject())).thenReturn(Try(applications))

    val request = FakeRequest(
      GET,
      s"/${entityTypeName}s?$labworkAttribute=${labwork.id.toString}"
    )

    val result = controller.asInstanceOf[LabworkApplicationCRUDController].all()(request)

    status(result) shouldBe OK
    contentType(result) shouldBe Some[String](mimeType)
    contentAsJson(result) shouldBe Json.toJson(Set(first, third))
  }

  "not return applications for a labwork when there is no match" in {
    val labwork = Labwork("label 1", "description 1", Semester.randomUUID, Course.randomUUID, Degree.randomUUID)

    val first = LabworkApplication(Labwork.randomUUID, User.randomUUID, Set(User.randomUUID))
    val second = LabworkApplication(Labwork.randomUUID, User.randomUUID, Set(User.randomUUID))
    val third = LabworkApplication(Labwork.randomUUID, User.randomUUID, Set(User.randomUUID))
    val fourth = LabworkApplication(Labwork.randomUUID, User.randomUUID, Set(User.randomUUID))

    val applications = Set(first, second, third, fourth)

    when(repository.get[LabworkApplication](anyObject(), anyObject())).thenReturn(Try(applications))

    val request = FakeRequest(
      GET,
      s"/${entityTypeName}s?$labworkAttribute=${labwork.id.toString}"
    )

    val result = controller.asInstanceOf[LabworkApplicationCRUDController].all()(request)

    status(result) shouldBe OK
    contentType(result) shouldBe Some[String](mimeType)
    contentAsJson(result) shouldBe Json.toJson(emptyLabworkApps)
  }

  "not return applications when there is an invalid query attribute" in {
    val first = LabworkApplication(Labwork.randomUUID, User.randomUUID, Set(User.randomUUID))
    val second = LabworkApplication(Labwork.randomUUID, User.randomUUID, Set(User.randomUUID))
    val third = LabworkApplication(Labwork.randomUUID, User.randomUUID, Set(User.randomUUID))
    val fourth = LabworkApplication(Labwork.randomUUID, User.randomUUID, Set(User.randomUUID))

    val applications = Set(first, second, third, fourth)

    when(repository.get[LabworkApplication](anyObject(), anyObject())).thenReturn(Try(applications))

    val request = FakeRequest(
      GET,
      s"/${entityTypeName}s?unknownAttribute=attributeValue"
    )

    val result = controller.asInstanceOf[LabworkApplicationCRUDController].all()(request)

    status(result) shouldBe SERVICE_UNAVAILABLE
    contentType(result) shouldBe Some("application/json")
    contentAsJson(result) shouldBe Json.obj(
      "status" -> "KO",
      "message" -> "Unknown attribute"
    )
  }

  "not return applications when there is an invalid query parameter value" in {
    val invalidParameter = "invalidParameterValue"

    val first = LabworkApplication(Labwork.randomUUID, User.randomUUID, Set(User.randomUUID))
    val second = LabworkApplication(Labwork.randomUUID, User.randomUUID, Set(User.randomUUID))
    val third = LabworkApplication(Labwork.randomUUID, User.randomUUID, Set(User.randomUUID))
    val fourth = LabworkApplication(Labwork.randomUUID, User.randomUUID, Set(User.randomUUID))

    val applications = Set(first, second, third, fourth)

    when(repository.get[LabworkApplication](anyObject(), anyObject())).thenReturn(Try(applications))

    val request = FakeRequest(
      GET,
      s"/${entityTypeName}s?$labworkAttribute=invalidParameterValue"
    )

    val result = controller.asInstanceOf[LabworkApplicationCRUDController].all()(request)

    status(result) shouldBe SERVICE_UNAVAILABLE
    contentType(result) shouldBe Some("application/json")
    contentAsJson(result) shouldBe Json.obj(
      "status" -> "KO",
      "message" -> s"Invalid UUID string: $invalidParameter"
    )
  }

  "return the corresponding application for a given applicant" in {
    val applicant = Student("id", "lastname", "firstname", "email", "registrationId", Degree.randomUUID)

    val first = LabworkApplication(Labwork.randomUUID, applicant.id, Set(User.randomUUID))
    val second = LabworkApplication(Labwork.randomUUID, User.randomUUID, Set(User.randomUUID))
    val third = LabworkApplication(Labwork.randomUUID, User.randomUUID, Set(User.randomUUID))
    val fourth = LabworkApplication(Labwork.randomUUID, User.randomUUID, Set(User.randomUUID))

    val applications = Set(first, second, third, fourth)

    when(repository.get[LabworkApplication](anyObject(), anyObject())).thenReturn(Try(applications))

    val request = FakeRequest(
      GET,
      s"/${entityTypeName}s?$applicantAttribute=${applicant.id.toString}"
    )

    val result = controller.asInstanceOf[LabworkApplicationCRUDController].all()(request)

    status(result) shouldBe OK
    contentType(result) shouldBe Some[String](mimeType)
    contentAsJson(result) shouldBe Json.toJson(Set(first))
  }
  "return all corresponding applications for a given applicant" in {
    val applicant = Student("id", "lastname", "firstname", "email", "registrationId", Degree.randomUUID)

    val first = LabworkApplication(Labwork.randomUUID, applicant.id, Set(User.randomUUID))
    val second = LabworkApplication(Labwork.randomUUID, User.randomUUID, Set(User.randomUUID))
    val third = LabworkApplication(Labwork.randomUUID, applicant.id, Set(User.randomUUID))
    val fourth = LabworkApplication(Labwork.randomUUID, User.randomUUID, Set(User.randomUUID))

    val applications = Set(first, second, third, fourth)

    when(repository.get[LabworkApplication](anyObject(), anyObject())).thenReturn(Try(applications))

    val request = FakeRequest(
      GET,
      s"/${entityTypeName}s?$applicantAttribute=${applicant.id.toString}"
    )

    val result = controller.asInstanceOf[LabworkApplicationCRUDController].all()(request)

    status(result) shouldBe OK
    contentType(result) shouldBe Some[String](mimeType)
    contentAsJson(result) shouldBe Json.toJson(Set(first, third))
  }

  "not return applications for an applicant when there is no match" in {
    val applicant = Student("id", "lastname", "firstname", "email", "registrationId", Degree.randomUUID)

    val first = LabworkApplication(Labwork.randomUUID, User.randomUUID, Set(User.randomUUID))
    val second = LabworkApplication(Labwork.randomUUID, User.randomUUID, Set(User.randomUUID))
    val third = LabworkApplication(Labwork.randomUUID, User.randomUUID, Set(User.randomUUID))
    val fourth = LabworkApplication(Labwork.randomUUID, User.randomUUID, Set(User.randomUUID))

    val applications = Set(first, second, third, fourth)

    when(repository.get[LabworkApplication](anyObject(), anyObject())).thenReturn(Try(applications))

    val request = FakeRequest(
      GET,
      s"/${entityTypeName}s?$applicantAttribute=${applicant.id.toString}"
    )

    val result = controller.asInstanceOf[LabworkApplicationCRUDController].all()(request)

    status(result) shouldBe OK
    contentType(result) shouldBe Some[String](mimeType)
    contentAsJson(result) shouldBe Json.toJson(emptyLabworkApps)
  }

  "return the corresponding application for a given friend" in {
    val friend = Student("id", "lastname", "firstname", "email", "registrationId", Degree.randomUUID)

    val first = LabworkApplication(Labwork.randomUUID, User.randomUUID, Set(friend.id))
    val second = LabworkApplication(Labwork.randomUUID, User.randomUUID, Set(User.randomUUID))
    val third = LabworkApplication(Labwork.randomUUID, User.randomUUID, Set(User.randomUUID))
    val fourth = LabworkApplication(Labwork.randomUUID, User.randomUUID, Set(User.randomUUID))

    val applications = Set(first, second, third, fourth)

    when(repository.get[LabworkApplication](anyObject(), anyObject())).thenReturn(Try(applications))

    val request = FakeRequest(
      GET,
      s"/${entityTypeName}s?$friendAttribute=${friend.id.toString}"
    )

    val result = controller.asInstanceOf[LabworkApplicationCRUDController].all()(request)

    status(result) shouldBe OK
    contentType(result) shouldBe Some[String](mimeType)
    contentAsJson(result) shouldBe Json.toJson(Set(first))
  }
  "return all corresponding applications for a given friend" in {
    val friend = Student("id", "lastname", "firstname", "email", "registrationId", Degree.randomUUID)

    val first = LabworkApplication(Labwork.randomUUID, User.randomUUID, Set(friend.id))
    val second = LabworkApplication(Labwork.randomUUID, User.randomUUID, Set(User.randomUUID))
    val third = LabworkApplication(Labwork.randomUUID, User.randomUUID, Set(friend.id))
    val fourth = LabworkApplication(Labwork.randomUUID, User.randomUUID, Set(User.randomUUID))

    val applications = Set(first, second, third, fourth)

    when(repository.get[LabworkApplication](anyObject(), anyObject())).thenReturn(Try(applications))

    val request = FakeRequest(
      GET,
      s"/${entityTypeName}s?$friendAttribute=${friend.id.toString}"
    )

    val result = controller.asInstanceOf[LabworkApplicationCRUDController].all()(request)

    status(result) shouldBe OK
    contentType(result) shouldBe Some[String](mimeType)
    contentAsJson(result) shouldBe Json.toJson(Set(first, third))
  }

  "not return applications for a friend  when there is no match" in {
    val friend = Student("id", "lastname", "firstname", "email", "registrationId", Degree.randomUUID)

    val first = LabworkApplication(Labwork.randomUUID, User.randomUUID, Set(User.randomUUID))
    val second = LabworkApplication(Labwork.randomUUID, User.randomUUID, Set(User.randomUUID))
    val third = LabworkApplication(Labwork.randomUUID, User.randomUUID, Set(User.randomUUID))
    val fourth = LabworkApplication(Labwork.randomUUID, User.randomUUID, Set(User.randomUUID))

    val applications = Set(first, second, third, fourth)

    when(repository.get[LabworkApplication](anyObject(), anyObject())).thenReturn(Try(applications))

    val request = FakeRequest(
      GET,
      s"/${entityTypeName}s?$friendAttribute=${friend.id.toString}"
    )

    val result = controller.asInstanceOf[LabworkApplicationCRUDController].all()(request)

    status(result) shouldBe OK
    contentType(result) shouldBe Some[String](mimeType)
    contentAsJson(result) shouldBe Json.toJson(emptyLabworkApps)
  }

  "return the corresponding application for a given date" in {
    val time = DateTime.now
    val tthen = DateTime.now.plusDays(2)

    val first = LabworkApplication(Labwork.randomUUID, User.randomUUID, Set(User.randomUUID), time)
    val second = LabworkApplication(Labwork.randomUUID, User.randomUUID, Set(User.randomUUID), tthen)
    val third = LabworkApplication(Labwork.randomUUID, User.randomUUID, Set(User.randomUUID), tthen)
    val fourth = LabworkApplication(Labwork.randomUUID, User.randomUUID, Set(User.randomUUID), tthen)
    val applications = Set(first, second, third, fourth)

    when(repository.get[LabworkApplication](anyObject(), anyObject())).thenReturn(Try(applications))

    val request = FakeRequest(
      GET,
      s"/${entityTypeName}s?$dateAttribute=${time.toString("yyyy-MM-dd")}"
    )

    val result = controller.asInstanceOf[LabworkApplicationCRUDController].all()(request)

    status(result) shouldBe OK
    contentType(result) shouldBe Some[String](mimeType)
    contentAsJson(result) shouldBe Json.toJson(Set(first))
  }

  "return all corresponding applications for a given date" in {
    val now = DateTime.now
    val tThen = now.plusDays(2)

    val first = LabworkApplication(Labwork.randomUUID, User.randomUUID, Set(User.randomUUID), now)
    val second = LabworkApplication(Labwork.randomUUID, User.randomUUID, Set(User.randomUUID), tThen)
    val third = LabworkApplication(Labwork.randomUUID, User.randomUUID, Set(User.randomUUID), now)
    val fourth = LabworkApplication(Labwork.randomUUID, User.randomUUID, Set(User.randomUUID), tThen)

    val applications = Set(first, second, third, fourth)

    when(repository.get[LabworkApplication](anyObject(), anyObject())).thenReturn(Try(applications))

    val request = FakeRequest(
      GET,
      s"/${entityTypeName}s?$dateAttribute=${now.toString("yyyy-MM-dd")}"
    )

    val result = controller.asInstanceOf[LabworkApplicationCRUDController].all()(request)


    status(result) shouldBe OK
    contentType(result) shouldBe Some[String](mimeType)
    contentAsJson(result) shouldBe Json.toJson(Set(first, third))
  }

  "not return applications for a date when there is no match" in {
    val time = DateTime.now.plusDays(2)

    val first = LabworkApplication(Labwork.randomUUID, User.randomUUID, Set(User.randomUUID))
    val second = LabworkApplication(Labwork.randomUUID, User.randomUUID, Set(User.randomUUID))
    val third = LabworkApplication(Labwork.randomUUID, User.randomUUID, Set(User.randomUUID))
    val fourth = LabworkApplication(Labwork.randomUUID, User.randomUUID, Set(User.randomUUID))

    val applications = Set(first, second, third, fourth)

    when(repository.get[LabworkApplication](anyObject(), anyObject())).thenReturn(Try(applications))

    val request = FakeRequest(
      GET,
      s"/${entityTypeName}s?$dateAttribute=${time.toString("yyyy-MM-dd")}"
    )

    val result = controller.asInstanceOf[LabworkApplicationCRUDController].all()(request)

    status(result) shouldBe OK
    contentType(result) shouldBe Some[String](mimeType)
    contentAsJson(result) shouldBe Json.toJson(emptyLabworkApps)
  }


  "return the corresponding application for a given dateTime range" in {
    val now = DateTime.now
    val shortly = now.plusDays(1)
    val after = now.plusDays(2)

    val first = LabworkApplication(Labwork.randomUUID, User.randomUUID, Set(User.randomUUID), now)
    val second = LabworkApplication(Labwork.randomUUID, User.randomUUID, Set(User.randomUUID), after)
    val third = LabworkApplication(Labwork.randomUUID, User.randomUUID, Set(User.randomUUID), after)
    val fourth = LabworkApplication(Labwork.randomUUID, User.randomUUID, Set(User.randomUUID), after)
    val applications = Set(first, second, third, fourth)

    when(repository.get[LabworkApplication](anyObject(), anyObject())).thenReturn(Try(applications))

    val request = FakeRequest(
      GET,
      s"/${entityTypeName}s?$minTime=${encode(now.toString("yyyy-MM-dd'T'HH:mm"))}&$maxTime=${encode(shortly.toString("yyyy-MM-dd'T'HH:mm"))}"
    )

    val result = controller.asInstanceOf[LabworkApplicationCRUDController].all()(request)

    status(result) shouldBe OK
    contentType(result) shouldBe Some[String](mimeType)
    contentAsJson(result) shouldBe Json.toJson(Set(first))
  }

  "return all corresponding applications for a given dateTime range" in {
    val now = DateTime.now
    val shortly = now.plusDays(1)
    val after = now.plusDays(2)

    val first = LabworkApplication(Labwork.randomUUID, User.randomUUID, Set(User.randomUUID), now)
    val second = LabworkApplication(Labwork.randomUUID, User.randomUUID, Set(User.randomUUID), after)
    val third = LabworkApplication(Labwork.randomUUID, User.randomUUID, Set(User.randomUUID), now.plusHours(8))
    val fourth = LabworkApplication(Labwork.randomUUID, User.randomUUID, Set(User.randomUUID), after)

    val applications = Set(first, second, third, fourth)

    when(repository.get[LabworkApplication](anyObject(), anyObject())).thenReturn(Try(applications))

    val request = FakeRequest(
      GET,
      s"/${entityTypeName}s?$minTime=${encode(now.toString("yyyy-MM-dd'T'HH:mm"))}&$maxTime=${encode(shortly.toString("yyyy-MM-dd'T'HH:mm"))}"
    )

    val result = controller.asInstanceOf[LabworkApplicationCRUDController].all()(request)


    status(result) shouldBe OK
    contentType(result) shouldBe Some[String](mimeType)
    contentAsJson(result) shouldBe Json.toJson(Set(first, third))
  }

  "not return applications for a dateTime range when there is no match" in {
    val time = DateTime.now.plusDays(2)
    val shortly = DateTime.now.plusDays(3)

    val first = LabworkApplication(Labwork.randomUUID, User.randomUUID, Set(User.randomUUID))
    val second = LabworkApplication(Labwork.randomUUID, User.randomUUID, Set(User.randomUUID))
    val third = LabworkApplication(Labwork.randomUUID, User.randomUUID, Set(User.randomUUID))
    val fourth = LabworkApplication(Labwork.randomUUID, User.randomUUID, Set(User.randomUUID))

    val applications = Set(first, second, third, fourth)

    when(repository.get[LabworkApplication](anyObject(), anyObject())).thenReturn(Try(applications))

    val request = FakeRequest(
      GET,
      s"/${entityTypeName}s?$minTime=${encode(time.toString("yyyy-MM-dd'T'HH:mm"))}&$maxTime=${encode(shortly.toString("yyyy-MM-dd'T'HH:mm"))}"
    )

    val result = controller.asInstanceOf[LabworkApplicationCRUDController].all()(request)

    status(result) shouldBe OK
    contentType(result) shouldBe Some[String](mimeType)
    contentAsJson(result) shouldBe Json.toJson(emptyLabworkApps)
  }

  "return the corresponding application for a given min or max time bound" in {
    val now = DateTime.now
    val before = now.minusDays(2)

    val first = LabworkApplication(Labwork.randomUUID, User.randomUUID, Set(User.randomUUID), now)
    val second = LabworkApplication(Labwork.randomUUID, User.randomUUID, Set(User.randomUUID), before)
    val third = LabworkApplication(Labwork.randomUUID, User.randomUUID, Set(User.randomUUID), before)
    val fourth = LabworkApplication(Labwork.randomUUID, User.randomUUID, Set(User.randomUUID), before)
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
    contentAsJson(result1) shouldBe Json.toJson(Set(first))

    status(result2) shouldBe OK
    contentType(result2) shouldBe Some[String](mimeType)
    contentAsJson(result2) shouldBe Json.toJson(Set(second, third, fourth))
  }

  "return all corresponding applications for a given min or max time bound" in {
    val now = DateTime.now
    val before = now.minusDays(2)

    val first = LabworkApplication(Labwork.randomUUID, User.randomUUID, Set(User.randomUUID), now)
    val second = LabworkApplication(Labwork.randomUUID, User.randomUUID, Set(User.randomUUID), before)
    val third = LabworkApplication(Labwork.randomUUID, User.randomUUID, Set(User.randomUUID), now.plusHours(8))
    val fourth = LabworkApplication(Labwork.randomUUID, User.randomUUID, Set(User.randomUUID), before)

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
    contentAsJson(result1) shouldBe Json.toJson(Set(first, third))

    status(result2) shouldBe OK
    contentType(result2) shouldBe Some[String](mimeType)
    contentAsJson(result2) shouldBe Json.toJson(Set(second, fourth))
  }

  "not return applications for a min or max bound when there is no match" in {
    val now = DateTime.now
    val before = now.minusDays(2)

    val first = LabworkApplication(Labwork.randomUUID, User.randomUUID, Set(User.randomUUID), before)
    val second = LabworkApplication(Labwork.randomUUID, User.randomUUID, Set(User.randomUUID), before)
    val third = LabworkApplication(Labwork.randomUUID, User.randomUUID, Set(User.randomUUID), before)
    val fourth = LabworkApplication(Labwork.randomUUID, User.randomUUID, Set(User.randomUUID), before)

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

    status(result1) shouldBe OK
    contentType(result1) shouldBe Some[String](mimeType)
    contentAsJson(result1) shouldBe Json.toJson(emptyLabworkApps)

    status(result2) shouldBe OK
    contentType(result2) shouldBe Some[String](mimeType)
    contentAsJson(result2) shouldBe Json.toJson(emptyLabworkApps)
  }

  s"successfully get a single $entityTypeName atomized" in {
    import LabworkApplication.atomicWrites

    doReturn(Success(Some(entityToPass))).
      doReturn(Success(Some(labworkToPass))).
      doReturn(Success(Some(applicantToPass))).
      when(repository).get(anyObject())(anyObject())
    when(repository.getMany[Student](anyObject())(anyObject())).thenReturn(Success(friendsToPass))

    val request = FakeRequest(
      GET,
      s"/${entityTypeName}s/${entityToPass.id}"
    )
    val result = controller.getAtomic(entityToPass.id.toString)(request)

    status(result) shouldBe OK
    contentType(result) shouldBe Some[String](mimeType)
    contentAsJson(result) shouldBe Json.toJson(atomizedEntityToPass)
  }

  s"not get a single $entityTypeName atomized when one of the atomized models is not found" in {
    doReturn(Success(Some(entityToPass))).
      doReturn(Success(Some(labworkToPass))).
      doReturn(Success(None)).
      when(repository).get(anyObject())(anyObject())
    when(repository.getMany[Student](anyObject())(anyObject())).thenReturn(Success(friendsToPass))

    val request = FakeRequest(
      GET,
      s"/${entityTypeName}s/${entityToPass.id}"
    )
    val result = controller.getAtomic(entityToPass.id.toString)(request)

    status(result) shouldBe NOT_FOUND
    contentType(result) shouldBe Some("application/json")
    contentAsJson(result) shouldBe Json.obj(
      "status" -> "KO",
      "message" -> "No such element..."
    )
  }

  s"not get a single $entityTypeName atomized when there is an exception" in {
    val errorMessage = s"Oops, cant get the desired $entityTypeName for some reason"

    doReturn(Success(Some(entityToPass))).
      doReturn(Success(Some(labworkToPass))).
      doReturn(Failure(new Exception(errorMessage))).
      when(repository).get(anyObject())(anyObject())
    when(repository.getMany[Student](anyObject())(anyObject())).thenReturn(Success(friendsToPass))

    val request = FakeRequest(
      GET,
      s"/${entityTypeName}s/${entityToPass.id}"
    )
    val result = controller.getAtomic(entityToPass.id.toString)(request)

    status(result) shouldBe INTERNAL_SERVER_ERROR
    contentType(result) shouldBe Some("application/json")
    contentAsJson(result) shouldBe Json.obj(
      "status" -> "KO",
      "errors" -> errorMessage
    )
  }

  s"successfully get all ${fgrammar(entityTypeName)} atomized" in {
    import LabworkApplication.atomicWrites

    val labworkApplications = Set(entityToPass, entityToFail)

    when(repository.get[LabworkApplication](anyObject(), anyObject())).thenReturn(Success(labworkApplications))

    doReturn(Success(Some(labworkToPass))).
      doReturn(Success(Some(applicantToPass))).
      doReturn(Success(Some(labworkToFail))).
      doReturn(Success(Some(applicantToFail))).
      when(repository).get(anyObject())(anyObject())

    doReturn(Success(friendsToPass)).
      doReturn(Success(friendsToFail)).
      when(repository).getMany(anyObject())(anyObject())

    val request = FakeRequest(
      GET,
      s"/${entityTypeName}s"
    )
    val result = controller.allAtomic()(request)

    status(result) shouldBe OK
    contentType(result) shouldBe Some[String](mimeType)
    contentAsJson(result) shouldBe Json.toJson(Set(atomizedEntityToPass, atomizedEntityToFail))
  }

  s"not get all ${fgrammar(entityTypeName)} atomized when there is an exception" in {
    val labworkApplications = Set(entityToPass, entityToFail)
    val errorMessage = s"Oops, cant get the desired $entityTypeName for some reason"

    when(repository.get[LabworkApplication](anyObject(), anyObject())).thenReturn(Success(labworkApplications))

    doReturn(Success(Some(labworkToPass))).
      doReturn(Success(Some(applicantToPass))).
      doReturn(Success(Some(labworkToFail))).
      doReturn(Success(Some(applicantToFail))).
      when(repository).get(anyObject())(anyObject())

    doReturn(Failure(new Exception(errorMessage))).
      doReturn(Success(friendsToFail)).
      when(repository).getMany(anyObject())(anyObject())

    val request = FakeRequest(
      GET,
      s"/${entityTypeName}s"
    )
    val result = controller.allAtomic()(request)

    status(result) shouldBe INTERNAL_SERVER_ERROR
    contentType(result) shouldBe Some("application/json")
    contentAsJson(result) shouldBe Json.obj(
      "status" -> "KO",
      "errors" -> errorMessage
    )
  }
}
