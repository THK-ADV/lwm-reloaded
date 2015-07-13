package controllers.crud

import java.util.UUID

import models.{Semester, SemesterProtocol}
import org.joda.time.DateTime
import org.mockito.Matchers._
import org.mockito.Mockito._
import play.api.libs.json.{JsValue, Json, Writes}
import play.api.test.FakeRequest
import play.api.test.Helpers._

import scala.util.{Failure, Success}

class SemesterCRUDControllerSpec extends AbstractCRUDControllerSpec[SemesterProtocol, Semester] {
  override val entityToPass: Semester = Semester("name to pass", "startDate to pass", "endDate to pass", "examPeriod to pass", Semester.randomUUID)

  override val entityToFail: Semester = Semester("name to fail", "startDate to fail", "endDate to fail", "examPeriod to fail", Semester.randomUUID)

  override val inputJson: JsValue = Json.obj(
    "name" -> "name input",
    "startDate" -> "startDate input",
    "endDate" -> "endDate input",
    "examPeriod" -> "examPeriod input"
  )

  override val controller: AbstractCRUDController[SemesterProtocol, Semester] = new SemesterCRUDController(repository, namespace) {
    override protected def fromInput(input: SemesterProtocol, id: Option[UUID]) = entityToPass
  }

  override implicit val jsonWrites: Writes[Semester] = Semester.writes

  override val mimeType: String = "application/json" //TODO: this should be a proper content type

  override def entityTypeName: String = "Semester"

  "A SemesterCRUDController " should {
    "successfully return all semesters for a year" in {
      val semesterWithDate = Semester("name to pass", DateTime.now().toString, "endDate to pass", "examPeriod to pass", Semester.randomUUID)
      val entitiesForYear = Set(semesterWithDate)
      val year = new DateTime(2015, 1, 1, 1, 1).getYear.toString

      when(repository.get[Semester](anyObject(), anyObject())).thenReturn(Success(entitiesForYear))

      val request = FakeRequest(
        GET,
        s"/${entityTypeName.toLowerCase}s?year=$year"
      )
      val result = controller.asInstanceOf[SemesterCRUDController].all()(request)

      status(result) shouldBe OK
      contentType(result) shouldBe Some(mimeType)
      contentAsString(result) shouldBe Json.toJson(entitiesForYear).toString()
    }

    "successfully return all semesters for many years" in {
      val semesterIn2015 = Semester("name to pass", DateTime.now().toString, "endDate to pass", "examPeriod to pass", Semester.randomUUID)
      val semesterIn2016 = Semester("name to pass", DateTime.now().plusYears(1).toString, "endDate to pass", "examPeriod to pass", Semester.randomUUID)
      val semesterIn2017 = Semester("name to pass", DateTime.now().plusYears(2).toString, "endDate to pass", "examPeriod to pass", Semester.randomUUID)
      val semesterIn2018 = Semester("name to pass", DateTime.now().plusYears(3).toString, "endDate to pass", "examPeriod to pass", Semester.randomUUID)

      val entitiesForYear = Set(semesterIn2015, semesterIn2016, semesterIn2017, semesterIn2018)
      val year2015 = new DateTime(2015, 1, 1, 1, 1).getYear.toString
      val year2017 = new DateTime(2017, 1, 1, 1, 1).getYear.toString

      when(repository.get[Semester](anyObject(), anyObject())).thenReturn(Success(entitiesForYear))

      val request = FakeRequest(
        GET,
        s"/${entityTypeName.toLowerCase}s?year=$year2015,$year2017"
      )
      val result = controller.asInstanceOf[SemesterCRUDController].all()(request)

      status(result) shouldBe OK
      contentType(result) shouldBe Some(mimeType)
      contentAsString(result) shouldBe Json.toJson(Seq(semesterIn2015, semesterIn2017)).toString()
    }

    "not return semesters for a year when there is no match" in {
      val semesterWithDate = Semester("name to pass", new DateTime(2014, 1, 1, 1, 1).toString, "endDate to pass", "examPeriod to pass", Semester.randomUUID)
      val anotherSemesterWithDate = Semester("name to pass", new DateTime(2013, 1, 1, 1, 1).toString, "endDate to pass", "examPeriod to pass", Semester.randomUUID)
      val entitiesForYear = Set(semesterWithDate, anotherSemesterWithDate)
      val year = new DateTime(2015, 1, 1, 1, 1).getYear.toString

      when(repository.get[Semester](anyObject(), anyObject())).thenReturn(Success(entitiesForYear))

      val expectedErrorMessage = s"""{"status":"KO","message":"No such element..."}"""

      val request = FakeRequest(
        GET,
        s"/${entityTypeName.toLowerCase}s?year=$year"
      )
      val result = controller.asInstanceOf[SemesterCRUDController].all()(request)

      status(result) shouldBe NOT_FOUND
      contentType(result) shouldBe Some(mimeType)
      contentAsString(result) shouldBe expectedErrorMessage
    }

    "not return semesters for a year when there is an exception" in {
      val errorMessage = s"Oops, cant get the desired semesters for a year for some reason"
      val year = new DateTime(2015, 1, 1, 1, 1).getYear.toString

      when(repository.get[Semester](anyObject(), anyObject())).thenReturn(Failure(new Exception(errorMessage)))

      val expectedErrorMessage = s"""{"status":"KO","errors":"$errorMessage"}"""

      val request = FakeRequest(
        GET,
        s"/${entityTypeName.toLowerCase}s?year=$year"
      )
      val result = controller.asInstanceOf[SemesterCRUDController].all()(request)

      status(result) shouldBe INTERNAL_SERVER_ERROR
      contentType(result) shouldBe Some(mimeType)
      contentAsString(result) shouldBe expectedErrorMessage
    }

    "not return semesters when there is an invalid query" in {
      val semesterWithDate = Semester("name to pass", new DateTime(2014, 1, 1, 1, 1).toString, "endDate to pass", "examPeriod to pass", Semester.randomUUID)
      val anotherSemesterWithDate = Semester("name to pass", new DateTime(2013, 1, 1, 1, 1).toString, "endDate to pass", "examPeriod to pass", Semester.randomUUID)
      val entitiesForYear = Set(semesterWithDate, anotherSemesterWithDate)
      val year = new DateTime(2015, 1, 1, 1, 1).getYear.toString

      when(repository.get[Semester](anyObject(), anyObject())).thenReturn(Success(entitiesForYear))

      val expectedErrorMessage = s"""{"status":"KO","message":"query not found"}"""

      val request = FakeRequest(
        GET,
        s"/${entityTypeName.toLowerCase}s?invalid=$year"
      )
      val result = controller.asInstanceOf[SemesterCRUDController].all()(request)

      status(result) shouldBe SERVICE_UNAVAILABLE
      contentType(result) shouldBe Some(mimeType)
      contentAsString(result) shouldBe expectedErrorMessage
    }

    "successfully return all semesters for a specific period" in {
      val semesterInWS = Semester("name to pass", new DateTime(2015, 10, 1, 1, 1).toString, "endDate to pass", "examPeriod to pass", Semester.randomUUID)
      val semesterInSS = Semester("name to pass", new DateTime(2015, 3, 1, 1, 1).toString, "endDate to pass", "examPeriod to pass", Semester.randomUUID)
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
      contentType(result) shouldBe Some(mimeType)
      contentAsString(result) shouldBe Json.toJson(Set(semesterInSS)).toString()
    }

    "successfully return all semesters in different years for a specific period" in {
      val semesterInWS2015 = Semester("name to pass", new DateTime(2015, 10, 1, 1, 1).toString, "endDate to pass", "examPeriod to pass", Semester.randomUUID)
      val semesterInWS2017 = Semester("name to pass", new DateTime(2017, 12, 1, 1, 1).toString, "endDate to pass", "examPeriod to pass", Semester.randomUUID)
      val semesterInSS2015 = Semester("name to pass", new DateTime(2015, 3, 1, 1, 1).toString, "endDate to pass", "examPeriod to pass", Semester.randomUUID)
      val semesterInSS2018 = Semester("name to pass", new DateTime(2018, 5, 1, 1, 1).toString, "endDate to pass", "examPeriod to pass", Semester.randomUUID)

      val semesters = Set(semesterInWS2015, semesterInWS2017, semesterInSS2015, semesterInSS2018)

      when(repository.get[Semester](anyObject(), anyObject())).thenReturn(Success(semesters))

      val year2015 = DateTime.parse(semesterInSS2015.startDate).getYear.toString
      val year2018 = DateTime.parse(semesterInSS2018.startDate).getYear.toString
      val period = "SS"

      val request = FakeRequest(
        GET,
        s"/${entityTypeName.toLowerCase}s?year=$year2015,$year2018&period=$period"
      )
      val result = controller.asInstanceOf[SemesterCRUDController].all()(request)

      status(result) shouldBe OK
      contentType(result) shouldBe Some(mimeType)
      contentAsString(result) shouldBe Json.toJson(Set(semesterInSS2015, semesterInSS2018)).toString()
    }

    "not return semesters for a specific period when there is not match" in {
      val semesterInWS = Semester("name to pass", new DateTime(2015, 10, 1, 1, 1).toString, "endDate to pass", "examPeriod to pass", Semester.randomUUID)
      val semesterInSS = Semester("name to pass", new DateTime(2015, 3, 1, 1, 1).toString, "endDate to pass", "examPeriod to pass", Semester.randomUUID)
      val semesters = Set(semesterInSS, semesterInWS)

      when(repository.get[Semester](anyObject(), anyObject())).thenReturn(Success(semesters))

      val year = DateTime.parse(semesterInSS.startDate).getYear.toString
      val period = "invalid period"
      val expectedErrorMessage = s"""{"status":"KO","message":"No such element..."}"""

      val request = FakeRequest(
        GET,
        s"/${entityTypeName.toLowerCase}s?year=$year&period=$period"
      )
      val result = controller.asInstanceOf[SemesterCRUDController].all()(request)

      status(result) shouldBe NOT_FOUND
      contentType(result) shouldBe Some(mimeType)
      contentAsString(result) shouldBe expectedErrorMessage
    }

    "not return semesters for a specific period when there is an exception" in {
      val errorMessage = s"Oops, cant get the desired semesters for a year for some reason"
      val semesterInSS = Semester("name to pass", new DateTime(2015, 3, 1, 1, 1).toString, "endDate to pass", "examPeriod to pass", Semester.randomUUID)

      when(repository.get[Semester](anyObject(), anyObject())).thenReturn(Failure(new Exception(errorMessage)))

      val year = DateTime.parse(semesterInSS.startDate).getYear.toString
      val expectedErrorMessage = s"""{"status":"KO","errors":"$errorMessage"}"""
      val period = "SS"

      val request = FakeRequest(
        GET,
        s"/${entityTypeName.toLowerCase}s/$year/$period"
      )
      val result = controller.asInstanceOf[SemesterCRUDController].all()(request)

      status(result) shouldBe INTERNAL_SERVER_ERROR
      contentType(result) shouldBe Some(mimeType)
      contentAsString(result) shouldBe expectedErrorMessage
    }
  }
}
