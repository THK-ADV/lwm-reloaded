package controllers.crud

import java.util.UUID

import controllers.SessionController
import models.security.{RefRole, Authority, Roles, Permissions}
import models.{Semester, SemesterProtocol}
import org.joda.time.DateTime
import org.mockito.Matchers._
import org.mockito.Mockito._
import org.w3.banana.PointedGraph
import org.w3.banana.sesame.Sesame
import play.api.{Application, ApplicationLoader}
import play.api.ApplicationLoader.Context
import play.api.libs.json.{JsValue, Json, Writes}
import play.api.mvc._
import play.api.test.{FakeApplication, WithApplicationLoader, FakeRequest}
import play.api.test.Helpers._
import services.RoleService
import utils.LWMActions.ContentTypedAction
import utils.{DefaultLwmApplication, LwmMimeType}

import scala.util.{Failure, Success}

class SemesterCRUDControllerSpec extends AbstractCRUDControllerSpec[SemesterProtocol, Semester] {
  override val entityToPass: Semester = Semester("name to pass", "startDate to pass", "endDate to pass", "examPeriod to pass", Semester.randomUUID)

  override val entityToFail: Semester = Semester("name to fail", "startDate to fail", "endDate to fail", "examPeriod to fail", Semester.randomUUID)

  override val mimeType: LwmMimeType = LwmMimeType.semesterV1Json

  override val controller: AbstractCRUDController[SemesterProtocol, Semester] = new SemesterCRUDController(repository, namespace, roleService) {

    override protected def contextFrom: PartialFunction[Rule, SecureContext] = {
      case _ => NonSecureBlock
    }

    override protected def fromInput(input: SemesterProtocol, id: Option[UUID]) = entityToPass
  }

  override implicit val jsonWrites: Writes[Semester] = Semester.writes

  override def entityTypeName: String = "semester"

  override val inputJson: JsValue = Json.obj(
    "name" -> entityToPass.name,
    "startDate" -> entityToPass.startDate,
    "endDate" -> entityToPass.endDate,
    "examPeriod" -> entityToPass.examPeriod
  )

  import bindings.SemesterBinding._
  import ops._


  override def pointedGraph: PointedGraph[Sesame] = entityToPass.toPG

  "A SemesterCRUDController also" should {

    "successfully return all semesters for a year" in {
      val semesterWithDate = Semester("name to pass", DateTime.now.toString, "endDate to pass", "examPeriod to pass", Semester.randomUUID)
      val entitiesForYear = Set(semesterWithDate)
      val year = DateTime.now.getYear.toString

      when(repository.get[Semester](anyObject(), anyObject())).thenReturn(Success(entitiesForYear))

      val request = FakeRequest(
        GET,
        s"/${entityTypeName.toLowerCase}s?year=$year"
      )

      val result = controller.asInstanceOf[SemesterCRUDController].all()(request)

      status(result) shouldBe OK
      contentType(result) shouldBe Some[String](mimeType)
      contentAsJson(result) shouldBe Json.toJson(entitiesForYear)
    }

    "successfully return all semesters for many years" in {
      val first = Semester("name to pass", DateTime.now.toString, "endDate to pass", "examPeriod to pass", Semester.randomUUID)
      val second = Semester("name to pass", DateTime.now.plusYears(1).toString, "endDate to pass", "examPeriod to pass", Semester.randomUUID)
      val third = Semester("name to pass", DateTime.now.plusYears(2).toString, "endDate to pass", "examPeriod to pass", Semester.randomUUID)
      val fourth = Semester("name to pass", DateTime.now.plusYears(3).toString, "endDate to pass", "examPeriod to pass", Semester.randomUUID)

      val entitiesForYear = Set(first, second, third, fourth)
      val some = DateTime.parse(first.startDate).getYear.toString
      val other = DateTime.parse(third.startDate).getYear.toString

      when(repository.get[Semester](anyObject(), anyObject())).thenReturn(Success(entitiesForYear))

      val request = FakeRequest(
        GET,
        s"/${entityTypeName.toLowerCase}s?year=$some,$other"
      )
      val result = controller.asInstanceOf[SemesterCRUDController].all()(request)

      status(result) shouldBe OK
      contentType(result) shouldBe Some[String](mimeType)
      contentAsJson(result) shouldBe Json.toJson(Seq(first, third))
    }

    "not return semesters for a year when there is no match" in {
      val semesterWithDate = Semester("name to pass", DateTime.now.minusYears(1).getYear.toString, "endDate to pass", "examPeriod to pass", Semester.randomUUID)
      val anotherSemesterWithDate = Semester("name to pass", DateTime.now.minusYears(2).getYear.toString, "endDate to pass", "examPeriod to pass", Semester.randomUUID)
      val entitiesForYear = Set(semesterWithDate, anotherSemesterWithDate)
      val year = DateTime.now.getYear.toString

      when(repository.get[Semester](anyObject(), anyObject())).thenReturn(Success(entitiesForYear))

      val request = FakeRequest(
        GET,
        s"/${entityTypeName.toLowerCase}s?year=$year"
      )
      val result = controller.asInstanceOf[SemesterCRUDController].all()(request)

      status(result) shouldBe NOT_FOUND
      contentType(result) shouldBe Some("application/json")
      contentAsJson(result) shouldBe Json.obj(
        "status" -> "KO",
        "message" -> "No such element..."
      )
    }

    "not return semesters for a year when there is an exception" in {
      val errorMessage = s"Oops, cant get the desired semesters for a year for some reason"
      val year = DateTime.now.getYear.toString

      when(repository.get[Semester](anyObject(), anyObject())).thenReturn(Failure(new Exception(errorMessage)))

      val request = FakeRequest(
        GET,
        s"/${entityTypeName.toLowerCase}s?year=$year"
      )
      val result = controller.asInstanceOf[SemesterCRUDController].all()(request)

      status(result) shouldBe INTERNAL_SERVER_ERROR
      contentType(result) shouldBe Some("application/json")
      contentAsJson(result) shouldBe Json.obj(
        "status" -> "KO",
        "errors" -> s"$errorMessage"
      )
    }

    "not return semesters when there is an invalid query" in {
      val semesterWithDate = Semester("name to pass", DateTime.now.minusYears(2).toString, "endDate to pass", "examPeriod to pass", Semester.randomUUID)
      val anotherSemesterWithDate = Semester("name to pass", DateTime.now.minusYears(3).toString, "endDate to pass", "examPeriod to pass", Semester.randomUUID)
      val entitiesForYear = Set(semesterWithDate, anotherSemesterWithDate)
      val year = DateTime.now.getYear.toString

      when(repository.get[Semester](anyObject(), anyObject())).thenReturn(Success(entitiesForYear))

      val request = FakeRequest(
        GET,
        s"/${entityTypeName.toLowerCase}s?invalid=$year"
      )
      val result = controller.asInstanceOf[SemesterCRUDController].all()(request)

      status(result) shouldBe SERVICE_UNAVAILABLE
      contentType(result) shouldBe Some("application/json")
      contentAsJson(result) shouldBe Json.obj(
        "status" -> "KO",
        "message" -> "query attribute not found"
      )
    }

    "successfully return all semesters for a specific period" in {
      val semesterInWS = Semester("name to pass", DateTime.now.withMonthOfYear(10).toString, "endDate to pass", "examPeriod to pass", Semester.randomUUID)
      val semesterInSS = Semester("name to pass", DateTime.now.withMonthOfYear(3).toString, "endDate to pass", "examPeriod to pass", Semester.randomUUID)
      val semesters = Set(semesterInSS, semesterInWS)

      when(repository.get[Semester](anyObject(), anyObject())).thenReturn(Success(semesters))

      val year = DateTime.parse(semesterInSS.startDate).getYear.toString
      val period = "SS"

      val request = FakeRequest(
        GET,
        s"/${entityTypeName.toLowerCase}s?year=$year&period=$period"
      )
      val result = controller.asInstanceOf[SemesterCRUDController].all()(request)

      status(result) shouldBe OK
      contentType(result) shouldBe Some[String](mimeType)
      contentAsJson(result) shouldBe Json.toJson(Set(semesterInSS))
    }

    "successfully return all semesters in different years for a specific period" in {
      val firstWS = Semester("name to pass", DateTime.now.withMonthOfYear(10).toString, "endDate to pass", "examPeriod to pass", Semester.randomUUID)
      val secondWS = Semester("name to pass", DateTime.now.plusYears(2).withMonthOfYear(12).toString, "endDate to pass", "examPeriod to pass", Semester.randomUUID)
      val firstSS = Semester("name to pass", DateTime.now.withMonthOfYear(3).toString, "endDate to pass", "examPeriod to pass", Semester.randomUUID)
      val secondSS = Semester("name to pass", DateTime.now.plusYears(3).withMonthOfYear(5).toString, "endDate to pass", "examPeriod to pass", Semester.randomUUID)

      val semesters = Set(firstWS, secondWS, firstSS, secondSS)

      when(repository.get[Semester](anyObject(), anyObject())).thenReturn(Success(semesters))

      val first = DateTime.parse(firstSS.startDate).getYear.toString
      val second = DateTime.parse(secondSS.startDate).getYear.toString
      val period = "SS"

      val request = FakeRequest(
        GET,
        s"/${entityTypeName.toLowerCase}s?year=$first,$second&period=$period"
      )
      val result = controller.asInstanceOf[SemesterCRUDController].all()(request)

      status(result) shouldBe OK
      contentType(result) shouldBe Some[String](mimeType)
      contentAsJson(result) shouldBe Json.toJson(Set(firstSS, secondSS))
    }

    "not return semesters for a specific period when there is not match" in {
      val semesterInWS = Semester("name to pass", DateTime.now.withMonthOfYear(10).toString, "endDate to pass", "examPeriod to pass", Semester.randomUUID)
      val semesterInSS = Semester("name to pass", DateTime.now.withMonthOfYear(3).toString, "endDate to pass", "examPeriod to pass", Semester.randomUUID)
      val semesters = Set(semesterInSS, semesterInWS)

      when(repository.get[Semester](anyObject(), anyObject())).thenReturn(Success(semesters))

      val year = DateTime.parse(semesterInSS.startDate).getYear.toString
      val period = "invalid period"

      val request = FakeRequest(
        GET,
        s"/${entityTypeName.toLowerCase}s?year=$year&period=$period"
      )
      val result = controller.asInstanceOf[SemesterCRUDController].all()(request)

      status(result) shouldBe NOT_FOUND
      contentType(result) shouldBe Some("application/json")
      contentAsJson(result) shouldBe Json.obj(
        "status" -> "KO",
        "message" -> "No such element..."
      )
    }

    "not return semesters for a specific period when there is an exception" in {
      val errorMessage = s"Oops, cant get the desired semesters for a year for some reason"
      val semesterInSS = Semester("name to pass", DateTime.now.withMonthOfYear(3).toString, "endDate to pass", "examPeriod to pass", Semester.randomUUID)

      when(repository.get[Semester](anyObject(), anyObject())).thenReturn(Failure(new Exception(errorMessage)))

      val year = DateTime.parse(semesterInSS.startDate).getYear.toString
      val period = "SS"

      val request = FakeRequest(
        GET,
        s"/${entityTypeName.toLowerCase}s/$year/$period"
      )
      val result = controller.asInstanceOf[SemesterCRUDController].all()(request)

      status(result) shouldBe INTERNAL_SERVER_ERROR
      contentType(result) shouldBe Some("application/json")
      contentAsJson(result) shouldBe Json.obj(
        "status" -> "KO",
        "errors" -> errorMessage
      )
    }
  }
}
