package controllers.crud.labwork

import java.net.URLEncoder
import java.util.UUID
import controllers.crud.labwork.LabworkApplicationCRUDController._
import controllers.crud.AbstractCRUDControllerSpec
import models._
import models.labwork.{Labwork, LabworkApplication, LabworkApplicationAtom, LabworkApplicationProtocol}
import models.semester.Semester
import models.users.{Student, User}
import org.joda.time.DateTime
import org.mockito.Matchers._
import org.mockito.Mockito._
import org.w3.banana.PointedGraph
import org.w3.banana.sesame.Sesame
import play.api.libs.json.{JsValue, Json, Writes}
import play.api.test.FakeRequest
import play.api.test.Helpers._
import utils.LwmMimeType
import scala.util.Try
import base.StreamHandler._

class LabworkApplicationCRUDControllerSpec extends AbstractCRUDControllerSpec[LabworkApplicationProtocol, LabworkApplication, LabworkApplicationAtom] {

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

  override val controller: LabworkApplicationCRUDController = new LabworkApplicationCRUDController(repository, sessionService, namespace, roleService) {

    override protected def fromInput(input: LabworkApplicationProtocol, existing: Option[LabworkApplication]): LabworkApplication = entityToPass

    override protected def contextFrom: PartialFunction[Rule, SecureContext] = {
      case _ => NonSecureBlock
    }
  }

  override val entityToFail: LabworkApplication = LabworkApplication(labworkToFail.id, applicantToFail.id, friendsToFail.map(_.id))

  override val entityToPass: LabworkApplication = LabworkApplication(labworkToPass.id, applicantToPass.id, friendsToPass.map(_.id))

  override implicit val jsonWrites: Writes[LabworkApplication] = LabworkApplication.writes

  override implicit def jsonWritesAtom: Writes[LabworkApplicationAtom] = LabworkApplication.writesAtom

  import bindings.LabworkApplicationDescriptor
  import ops._
  implicit val labworkApplicationBinder = LabworkApplicationDescriptor.binder

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
  override val atomizedEntityToPass = LabworkApplicationAtom(labworkToPass, applicantToPass, friendsToPass, entityToPass.timestamp, entityToPass.invalidated, entityToPass.id)

  override val atomizedEntityToFail = LabworkApplicationAtom(labworkToFail, applicantToFail, friendsToFail, entityToFail.timestamp, entityToPass.invalidated, entityToFail.id)

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

      when(repository.getAll[LabworkApplication](anyObject())).thenReturn(Try(applications))

      val request = FakeRequest(
        GET,
        s"/${entityTypeName}s?$labworkAttribute=${labwork.id.toString}"
      )

      val result = controller.all()(request)
      val expected = Set(Json.toJson(first))

      status(result) shouldBe OK
      contentType(result) shouldBe Some(mimeType.value)
      contentFromStream(result) shouldBe expected
    }
  }

  "return all corresponding applications for a given labwork" in {
    val labwork = Labwork("label 1", "description 1", Semester.randomUUID, Course.randomUUID, Degree.randomUUID)

    val first = LabworkApplication(labwork.id, User.randomUUID, Set(User.randomUUID))
    val second = LabworkApplication(Labwork.randomUUID, User.randomUUID, Set(User.randomUUID))
    val third = LabworkApplication(labwork.id, User.randomUUID, Set(User.randomUUID))
    val fourth = LabworkApplication(Labwork.randomUUID, User.randomUUID, Set(User.randomUUID))

    val applications = Set(first, second, third, fourth)

    when(repository.getAll[LabworkApplication](anyObject())).thenReturn(Try(applications))

    val request = FakeRequest(
      GET,
      s"/${entityTypeName}s?$labworkAttribute=${labwork.id.toString}"
    )

    val result = controller.all()(request)
    val expected = Set(Json.toJson(first), Json.toJson(third))

    status(result) shouldBe OK
    contentType(result) shouldBe Some(mimeType.value)
    contentFromStream(result) shouldBe expected
  }

  "not return applications for a labwork when there is no match" in {
    val labwork = Labwork("label 1", "description 1", Semester.randomUUID, Course.randomUUID, Degree.randomUUID)

    val first = LabworkApplication(Labwork.randomUUID, User.randomUUID, Set(User.randomUUID))
    val second = LabworkApplication(Labwork.randomUUID, User.randomUUID, Set(User.randomUUID))
    val third = LabworkApplication(Labwork.randomUUID, User.randomUUID, Set(User.randomUUID))
    val fourth = LabworkApplication(Labwork.randomUUID, User.randomUUID, Set(User.randomUUID))

    val applications = Set(first, second, third, fourth)

    when(repository.getAll[LabworkApplication](anyObject())).thenReturn(Try(applications))

    val request = FakeRequest(
      GET,
      s"/${entityTypeName}s?$labworkAttribute=${labwork.id.toString}"
    )

    val result = controller.all()(request)

    status(result) shouldBe OK
    contentType(result) shouldBe Some(mimeType.value)
    contentFromStream(result) shouldBe emptyJson
  }

  "not return applications when there is an invalid query attribute" in {
    val first = LabworkApplication(Labwork.randomUUID, User.randomUUID, Set(User.randomUUID))
    val second = LabworkApplication(Labwork.randomUUID, User.randomUUID, Set(User.randomUUID))
    val third = LabworkApplication(Labwork.randomUUID, User.randomUUID, Set(User.randomUUID))
    val fourth = LabworkApplication(Labwork.randomUUID, User.randomUUID, Set(User.randomUUID))

    val applications = Set(first, second, third, fourth)

    when(repository.getAll[LabworkApplication](anyObject())).thenReturn(Try(applications))

    val request = FakeRequest(
      GET,
      s"/${entityTypeName}s?unknownAttribute=attributeValue"
    )

    val result = controller.all()(request)

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

    when(repository.getAll[LabworkApplication](anyObject())).thenReturn(Try(applications))

    val request = FakeRequest(
      GET,
      s"/${entityTypeName}s?$labworkAttribute=invalidParameterValue"
    )

    val result = controller.all()(request)

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

    when(repository.getAll[LabworkApplication](anyObject())).thenReturn(Try(applications))

    val request = FakeRequest(
      GET,
      s"/${entityTypeName}s?$applicantAttribute=${applicant.id.toString}"
    )

    val result = controller.all()(request)
    val expected = Set(Json.toJson(first))

    status(result) shouldBe OK
    contentType(result) shouldBe Some(mimeType.value)
    contentFromStream(result) shouldBe expected
  }
  "return all corresponding applications for a given applicant" in {
    val applicant = Student("id", "lastname", "firstname", "email", "registrationId", Degree.randomUUID)

    val first = LabworkApplication(Labwork.randomUUID, applicant.id, Set(User.randomUUID))
    val second = LabworkApplication(Labwork.randomUUID, User.randomUUID, Set(User.randomUUID))
    val third = LabworkApplication(Labwork.randomUUID, applicant.id, Set(User.randomUUID))
    val fourth = LabworkApplication(Labwork.randomUUID, User.randomUUID, Set(User.randomUUID))

    val applications = Set(first, second, third, fourth)

    when(repository.getAll[LabworkApplication](anyObject())).thenReturn(Try(applications))

    val request = FakeRequest(
      GET,
      s"/${entityTypeName}s?$applicantAttribute=${applicant.id.toString}"
    )

    val result = controller.all()(request)
    val expected = Set(Json.toJson(first), Json.toJson(third))

    status(result) shouldBe OK
    contentType(result) shouldBe Some(mimeType.value)
    contentFromStream(result) shouldBe expected
  }

  "not return applications for an applicant when there is no match" in {
    val applicant = Student("id", "lastname", "firstname", "email", "registrationId", Degree.randomUUID)

    val first = LabworkApplication(Labwork.randomUUID, User.randomUUID, Set(User.randomUUID))
    val second = LabworkApplication(Labwork.randomUUID, User.randomUUID, Set(User.randomUUID))
    val third = LabworkApplication(Labwork.randomUUID, User.randomUUID, Set(User.randomUUID))
    val fourth = LabworkApplication(Labwork.randomUUID, User.randomUUID, Set(User.randomUUID))

    val applications = Set(first, second, third, fourth)

    when(repository.getAll[LabworkApplication](anyObject())).thenReturn(Try(applications))

    val request = FakeRequest(
      GET,
      s"/${entityTypeName}s?$applicantAttribute=${applicant.id.toString}"
    )

    val result = controller.all()(request)

    status(result) shouldBe OK
    contentType(result) shouldBe Some(mimeType.value)
    contentFromStream(result) shouldBe emptyJson
  }

  "return the corresponding application for a given friend" in {
    val friend = Student("id", "lastname", "firstname", "email", "registrationId", Degree.randomUUID)

    val first = LabworkApplication(Labwork.randomUUID, User.randomUUID, Set(friend.id))
    val second = LabworkApplication(Labwork.randomUUID, User.randomUUID, Set(User.randomUUID))
    val third = LabworkApplication(Labwork.randomUUID, User.randomUUID, Set(User.randomUUID))
    val fourth = LabworkApplication(Labwork.randomUUID, User.randomUUID, Set(User.randomUUID))

    val applications = Set(first, second, third, fourth)

    when(repository.getAll[LabworkApplication](anyObject())).thenReturn(Try(applications))

    val request = FakeRequest(
      GET,
      s"/${entityTypeName}s?$friendAttribute=${friend.id.toString}"
    )

    val result = controller.all()(request)
    val expected = Set(Json.toJson(first))

    status(result) shouldBe OK
    contentType(result) shouldBe Some(mimeType.value)
    contentFromStream(result) shouldBe expected
  }
  "return all corresponding applications for a given friend" in {
    val friend = Student("id", "lastname", "firstname", "email", "registrationId", Degree.randomUUID)

    val first = LabworkApplication(Labwork.randomUUID, User.randomUUID, Set(friend.id))
    val second = LabworkApplication(Labwork.randomUUID, User.randomUUID, Set(User.randomUUID))
    val third = LabworkApplication(Labwork.randomUUID, User.randomUUID, Set(friend.id))
    val fourth = LabworkApplication(Labwork.randomUUID, User.randomUUID, Set(User.randomUUID))

    val applications = Set(first, second, third, fourth)

    when(repository.getAll[LabworkApplication](anyObject())).thenReturn(Try(applications))

    val request = FakeRequest(
      GET,
      s"/${entityTypeName}s?$friendAttribute=${friend.id.toString}"
    )

    val result = controller.all()(request)
    val expected = Set(Json.toJson(first), Json.toJson(third))

    status(result) shouldBe OK
    contentType(result) shouldBe Some(mimeType.value)
    contentFromStream(result) shouldBe expected
  }

  "not return applications for a friend  when there is no match" in {
    val friend = Student("id", "lastname", "firstname", "email", "registrationId", Degree.randomUUID)

    val first = LabworkApplication(Labwork.randomUUID, User.randomUUID, Set(User.randomUUID))
    val second = LabworkApplication(Labwork.randomUUID, User.randomUUID, Set(User.randomUUID))
    val third = LabworkApplication(Labwork.randomUUID, User.randomUUID, Set(User.randomUUID))
    val fourth = LabworkApplication(Labwork.randomUUID, User.randomUUID, Set(User.randomUUID))

    val applications = Set(first, second, third, fourth)

    when(repository.getAll[LabworkApplication](anyObject())).thenReturn(Try(applications))

    val request = FakeRequest(
      GET,
      s"/${entityTypeName}s?$friendAttribute=${friend.id.toString}"
    )

    val result = controller.all()(request)

    status(result) shouldBe OK
    contentType(result) shouldBe Some(mimeType.value)
    contentFromStream(result) shouldBe emptyJson
  }

  "return the corresponding application for a given date" in {
    val time = DateTime.now
    val tthen = DateTime.now.plusDays(2)

    val first = LabworkApplication(Labwork.randomUUID, User.randomUUID, Set(User.randomUUID), time)
    val second = LabworkApplication(Labwork.randomUUID, User.randomUUID, Set(User.randomUUID), tthen)
    val third = LabworkApplication(Labwork.randomUUID, User.randomUUID, Set(User.randomUUID), tthen)
    val fourth = LabworkApplication(Labwork.randomUUID, User.randomUUID, Set(User.randomUUID), tthen)
    val applications = Set(first, second, third, fourth)

    when(repository.getAll[LabworkApplication](anyObject())).thenReturn(Try(applications))

    val request = FakeRequest(
      GET,
      s"/${entityTypeName}s?$dateAttribute=${time.toString("yyyy-MM-dd")}"
    )

    val result = controller.all()(request)
    val expected = Set(Json.toJson(first))

    status(result) shouldBe OK
    contentType(result) shouldBe Some(mimeType.value)
    contentFromStream(result) shouldBe expected
  }

  "return all corresponding applications for a given date" in {
    val now = DateTime.now
    val tThen = now.plusDays(2)

    val first = LabworkApplication(Labwork.randomUUID, User.randomUUID, Set(User.randomUUID), now)
    val second = LabworkApplication(Labwork.randomUUID, User.randomUUID, Set(User.randomUUID), tThen)
    val third = LabworkApplication(Labwork.randomUUID, User.randomUUID, Set(User.randomUUID), now)
    val fourth = LabworkApplication(Labwork.randomUUID, User.randomUUID, Set(User.randomUUID), tThen)

    val applications = Set(first, second, third, fourth)

    when(repository.getAll[LabworkApplication](anyObject())).thenReturn(Try(applications))

    val request = FakeRequest(
      GET,
      s"/${entityTypeName}s?$dateAttribute=${now.toString("yyyy-MM-dd")}"
    )

    val result = controller.all()(request)
    val expected = Set(Json.toJson(first), Json.toJson(third))


    status(result) shouldBe OK
    contentType(result) shouldBe Some(mimeType.value)
    contentFromStream(result) shouldBe expected
  }

  "not return applications for a date when there is no match" in {
    val time = DateTime.now.plusDays(2)

    val first = LabworkApplication(Labwork.randomUUID, User.randomUUID, Set(User.randomUUID))
    val second = LabworkApplication(Labwork.randomUUID, User.randomUUID, Set(User.randomUUID))
    val third = LabworkApplication(Labwork.randomUUID, User.randomUUID, Set(User.randomUUID))
    val fourth = LabworkApplication(Labwork.randomUUID, User.randomUUID, Set(User.randomUUID))

    val applications = Set(first, second, third, fourth)

    when(repository.getAll[LabworkApplication](anyObject())).thenReturn(Try(applications))

    val request = FakeRequest(
      GET,
      s"/${entityTypeName}s?$dateAttribute=${time.toString("yyyy-MM-dd")}"
    )

    val result = controller.all()(request)

    status(result) shouldBe OK
    contentType(result) shouldBe Some(mimeType.value)
    contentFromStream(result) shouldBe emptyJson
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

    when(repository.getAll[LabworkApplication](anyObject())).thenReturn(Try(applications))

    val request = FakeRequest(
      GET,
      s"/${entityTypeName}s?$minTime=${encode(now.toString("yyyy-MM-dd'T'HH:mm"))}&$maxTime=${encode(shortly.toString("yyyy-MM-dd'T'HH:mm"))}"
    )

    val result = controller.all()(request)
    val expected = Set(Json.toJson(first))

    status(result) shouldBe OK
    contentType(result) shouldBe Some(mimeType.value)
    contentFromStream(result) shouldBe expected
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

    when(repository.getAll[LabworkApplication](anyObject())).thenReturn(Try(applications))

    val request = FakeRequest(
      GET,
      s"/${entityTypeName}s?$minTime=${encode(now.toString("yyyy-MM-dd'T'HH:mm"))}&$maxTime=${encode(shortly.toString("yyyy-MM-dd'T'HH:mm"))}"
    )

    val result = controller.all()(request)
    val expected = Set(Json.toJson(first), Json.toJson(third))


    status(result) shouldBe OK
    contentType(result) shouldBe Some(mimeType.value)
    contentFromStream(result) shouldBe expected
  }

  "not return applications for a dateTime range when there is no match" in {
    val time = DateTime.now.plusDays(2)
    val shortly = DateTime.now.plusDays(3)

    val first = LabworkApplication(Labwork.randomUUID, User.randomUUID, Set(User.randomUUID))
    val second = LabworkApplication(Labwork.randomUUID, User.randomUUID, Set(User.randomUUID))
    val third = LabworkApplication(Labwork.randomUUID, User.randomUUID, Set(User.randomUUID))
    val fourth = LabworkApplication(Labwork.randomUUID, User.randomUUID, Set(User.randomUUID))

    val applications = Set(first, second, third, fourth)

    when(repository.getAll[LabworkApplication](anyObject())).thenReturn(Try(applications))

    val request = FakeRequest(
      GET,
      s"/${entityTypeName}s?$minTime=${encode(time.toString("yyyy-MM-dd'T'HH:mm"))}&$maxTime=${encode(shortly.toString("yyyy-MM-dd'T'HH:mm"))}"
    )

    val result = controller.all()(request)

    status(result) shouldBe OK
    contentType(result) shouldBe Some(mimeType.value)
    contentFromStream(result) shouldBe emptyJson
  }

  "return the corresponding application for a given min or max time bound" in {
    val now = DateTime.now
    val before = now.minusDays(2)

    val first = LabworkApplication(Labwork.randomUUID, User.randomUUID, Set(User.randomUUID), now)
    val second = LabworkApplication(Labwork.randomUUID, User.randomUUID, Set(User.randomUUID), before)
    val third = LabworkApplication(Labwork.randomUUID, User.randomUUID, Set(User.randomUUID), before)
    val fourth = LabworkApplication(Labwork.randomUUID, User.randomUUID, Set(User.randomUUID), before)
    val applications = Set(first, second, third, fourth)

    when(repository.getAll[LabworkApplication](anyObject())).thenReturn(Try(applications))

    val request1 = FakeRequest(
      GET,
      s"/${entityTypeName}s?$minTime=${encode(now.toString("yyyy-MM-dd'T'HH:mm"))}"
    )

    val request2 = FakeRequest(
      GET,
      s"/${entityTypeName}s?$maxTime=${encode(now.toString("yyyy-MM-dd'T'HH:mm"))}"
    )

    val result1 = controller.all()(request1)
    val result2 = controller.all()(request2)

    val expected1 = Set(Json.toJson(first))
    val expected2 = Set(Json.toJson(second), Json.toJson(third), Json.toJson(fourth))

    status(result1) shouldBe OK
    contentType(result1) shouldBe Some(mimeType.value)
    contentFromStream(result1) shouldBe expected1

    status(result2) shouldBe OK
    contentType(result2) shouldBe Some(mimeType.value)
    contentFromStream(result2) shouldBe expected2
  }

  "return all corresponding applications for a given min or max time bound" in {
    val now = DateTime.now
    val before = now.minusDays(2)

    val first = LabworkApplication(Labwork.randomUUID, User.randomUUID, Set(User.randomUUID), now)
    val second = LabworkApplication(Labwork.randomUUID, User.randomUUID, Set(User.randomUUID), before)
    val third = LabworkApplication(Labwork.randomUUID, User.randomUUID, Set(User.randomUUID), now.plusHours(8))
    val fourth = LabworkApplication(Labwork.randomUUID, User.randomUUID, Set(User.randomUUID), before)

    val applications = Set(first, second, third, fourth)

    when(repository.getAll[LabworkApplication](anyObject())).thenReturn(Try(applications))

    val request1 = FakeRequest(
      GET,
      s"/${entityTypeName}s?$minTime=${encode(now.toString("yyyy-MM-dd'T'HH:mm"))}"
    )

    val request2 = FakeRequest(
      GET,
      s"/${entityTypeName}s?$maxTime=${encode(now.toString("yyyy-MM-dd'T'HH:mm"))}"
    )

    val result1 = controller.all()(request1)
    val result2 = controller.all()(request2)

    val expected1 = Set(Json.toJson(first), Json.toJson(third))
    val expected2 = Set(Json.toJson(second), Json.toJson(fourth))


    status(result1) shouldBe OK
    contentType(result1) shouldBe Some(mimeType.value)
    contentFromStream(result1) shouldBe expected1

    status(result2) shouldBe OK
    contentType(result2) shouldBe Some(mimeType.value)
    contentFromStream(result2) shouldBe expected2
  }

  "not return applications for a min or max bound when there is no match" in {
    val now = DateTime.now
    val before = now.minusDays(2)

    val first = LabworkApplication(Labwork.randomUUID, User.randomUUID, Set(User.randomUUID), before)
    val second = LabworkApplication(Labwork.randomUUID, User.randomUUID, Set(User.randomUUID), before)
    val third = LabworkApplication(Labwork.randomUUID, User.randomUUID, Set(User.randomUUID), before)
    val fourth = LabworkApplication(Labwork.randomUUID, User.randomUUID, Set(User.randomUUID), before)

    val applications = Set(first, second, third, fourth)

    when(repository.getAll[LabworkApplication](anyObject())).thenReturn(Try(applications))

    val request1 = FakeRequest(
      GET,
      s"/${entityTypeName}s?$minTime=${encode(now.toString("yyyy-MM-dd'T'HH:mm"))}"
    )

    val request2 = FakeRequest(
      GET,
      s"/${entityTypeName}s?$maxTime=${encode(before.toString("yyyy-MM-dd'T'HH:mm"))}"
    )

    val result1 = controller.all()(request1)
    val result2 = controller.all()(request2)

    status(result1) shouldBe OK
    contentType(result1) shouldBe Some(mimeType.value)
    contentFromStream(result1) shouldBe emptyJson

    status(result2) shouldBe OK
    contentType(result2) shouldBe Some(mimeType.value)
    contentFromStream(result2) shouldBe emptyJson
  }
}
