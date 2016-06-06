package controllers.crud.semester

import java.util.UUID

import org.joda.time.LocalDate
import controllers.crud.{AbstractCRUDController, AbstractCRUDControllerSpec}
import models.semester.{SemesterProtocol, Semester}
import org.joda.time.DateTime
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

import scala.util.{Failure, Success}

class SemesterCRUDControllerSpec extends AbstractCRUDControllerSpec[SemesterProtocol, Semester] {

  override val entityToPass: Semester = Semester("label to pass", "abbreviation to pass", LocalDate.now, LocalDate.now, LocalDate.now, Semester.randomUUID)

  override val entityToFail: Semester = Semester("label to fail", "abbreviation to fail", LocalDate.now, LocalDate.now, LocalDate.now, Semester.randomUUID)

  override val mimeType: LwmMimeType = LwmMimeType.semesterV1Json

  override val controller: AbstractCRUDController[SemesterProtocol, Semester] = new SemesterCRUDController(repository, sessionService, namespace, roleService) {

    override protected def fromInput(input: SemesterProtocol, existing: Option[Semester]): Semester = entityToPass

    override protected def contextFrom: PartialFunction[Rule, SecureContext] = {
      case _ => NonSecureBlock
    }
  }

  override implicit val jsonWrites: Writes[Semester] = Semester.writes

  override def entityTypeName: String = "semester"

  override val inputJson: JsValue = Json.obj(
    "label" -> entityToPass.label,
    "abbreviation" -> entityToPass.abbreviation,
    "start" -> entityToPass.start,
    "end" -> entityToPass.end,
    "examStart" -> entityToPass.examStart
  )

  override val updateJson: JsValue = Json.obj(
    "label" -> s"${entityToPass.label} updated",
    "abbreviation" -> s"${entityToPass.abbreviation} updated",
    "start" -> entityToPass.start.plusWeeks(1),
    "end" -> entityToPass.end.plusWeeks(1),
    "examStart" -> entityToPass.examStart.plusWeeks(1)
  )

  import bindings.SemesterDescriptor
  import ops._

  implicit val semesterBinder = SemesterDescriptor.binder

  override def pointedGraph: PointedGraph[Sesame] = entityToPass.toPG

  "A SemesterCRUDController also " should {

    "successfully return all semesters for a year" in {
      val semesterWithDate = Semester("label", "abbrev", LocalDate.now, LocalDate.now, LocalDate.now, Semester.randomUUID)
      val entitiesForYear = Set(semesterWithDate)
      val year = LocalDate.now.getYear

      when(repository.getAll[Semester](anyObject())).thenReturn(Success(entitiesForYear))

      val request = FakeRequest(
        GET,
        s"/${entityTypeName.toLowerCase}s?year=${year.toString}"
      )

      val result = controller.asInstanceOf[SemesterCRUDController].all()(request)

      status(result) shouldBe OK
      contentType(result) shouldBe Some[String](mimeType)
      contentAsJson(result) shouldBe Json.toJson(entitiesForYear)
    }

    "successfully return all semesters for many years" in {
      val first = Semester("label", "abbrev", LocalDate.now, LocalDate.now, LocalDate.now, Semester.randomUUID)
      val second = Semester("label", "abbrev", LocalDate.now.plusYears(1), LocalDate.now, LocalDate.now, Semester.randomUUID)
      val third = Semester("label", "abbrev", LocalDate.now.plusYears(2), LocalDate.now, LocalDate.now, Semester.randomUUID)
      val fourth = Semester("label", "abbrev", LocalDate.now.plusYears(3), LocalDate.now, LocalDate.now, Semester.randomUUID)

      val entitiesForYear = Set(first, second, third, fourth)
      val some = first.start.getYear
      val other = third.start.getYear

      when(repository.getAll[Semester](anyObject())).thenReturn(Success(entitiesForYear))

      val request = FakeRequest(
        GET,
        s"/${entityTypeName.toLowerCase}s?year=${some.toString},${other.toString}"
      )
      val result = controller.asInstanceOf[SemesterCRUDController].all()(request)

      status(result) shouldBe OK
      contentType(result) shouldBe Some[String](mimeType)
      contentAsJson(result) shouldBe Json.toJson(Seq(first, third))
    }

    "not return semesters for a year when there is no match" in {
      val semesterWithDate = Semester("label", "abbrev", LocalDate.now.minusYears(1), LocalDate.now, LocalDate.now, Semester.randomUUID)
      val anotherSemesterWithDate = Semester("label", "abbrev", LocalDate.now.minusYears(2), LocalDate.now, LocalDate.now, Semester.randomUUID)
      val entitiesForYear = Set(semesterWithDate, anotherSemesterWithDate)
      val year = DateTime.now.getYear

      when(repository.getAll[Semester](anyObject())).thenReturn(Success(entitiesForYear))

      val request = FakeRequest(
        GET,
        s"/${entityTypeName.toLowerCase}s?year=${year.toString}"
      )
      val result = controller.asInstanceOf[SemesterCRUDController].all()(request)

      status(result) shouldBe OK
      contentType(result) shouldBe Some[String](mimeType)
      contentAsJson(result) shouldBe Json.toJson(Set.empty[Semester])
    }

    "not return semesters for a year when there is an exception" in {
      val errorMessage = s"Oops, cant get the desired semesters for a year for some reason"
      val year = DateTime.now.getYear

      when(repository.getAll[Semester](anyObject())).thenReturn(Failure(new Exception(errorMessage)))

      val request = FakeRequest(
        GET,
        s"/${entityTypeName.toLowerCase}s?year=${year.toString}"
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
      val semesterWithDate = Semester("label", "abbrev", LocalDate.now.minusYears(2), LocalDate.now, LocalDate.now, Semester.randomUUID)
      val anotherSemesterWithDate = Semester("label", "abbrev", LocalDate.now.minusYears(3), LocalDate.now, LocalDate.now, Semester.randomUUID)
      val entitiesForYear = Set(semesterWithDate, anotherSemesterWithDate)
      val year = DateTime.now.getYear

      when(repository.getAll[Semester](anyObject())).thenReturn(Success(entitiesForYear))

      val request = FakeRequest(
        GET,
        s"/${entityTypeName.toLowerCase}s?invalid=${year.toString}"
      )
      val result = controller.asInstanceOf[SemesterCRUDController].all()(request)

      status(result) shouldBe SERVICE_UNAVAILABLE
      contentType(result) shouldBe Some("application/json")
      contentAsJson(result) shouldBe Json.obj(
        "status" -> "KO",
        "message" -> "Unknown attribute"
      )
    }

    "successfully return all semesters for a specific period" in {
      val semesterInWS = Semester("label", "abbrev", LocalDate.now.withMonthOfYear(10), LocalDate.now, LocalDate.now, Semester.randomUUID)
      val semesterInSS = Semester("label", "abbrev", LocalDate.now.withMonthOfYear(3), LocalDate.now, LocalDate.now, Semester.randomUUID)
      val semesters = Set(semesterInSS, semesterInWS)

      when(repository.getAll[Semester](anyObject())).thenReturn(Success(semesters))

      val year = semesterInSS.start.getYear
      val period = "SS"

      val request = FakeRequest(
        GET,
        s"/${entityTypeName.toLowerCase}s?year=${year.toString}&period=$period"
      )
      val result = controller.asInstanceOf[SemesterCRUDController].all()(request)

      status(result) shouldBe OK
      contentType(result) shouldBe Some[String](mimeType)
      contentAsJson(result) shouldBe Json.toJson(Set(semesterInSS))
    }

    "successfully return all semesters in different years for a specific period" in {
      val firstWS = Semester("label", "abbrev", LocalDate.now.withMonthOfYear(10), LocalDate.now, LocalDate.now, Semester.randomUUID)
      val secondWS = Semester("label", "abbrev", LocalDate.now.plusYears(2).withMonthOfYear(12), LocalDate.now, LocalDate.now, Semester.randomUUID)
      val firstSS = Semester("label", "abbrev", LocalDate.now.withMonthOfYear(3), LocalDate.now, LocalDate.now, Semester.randomUUID)
      val secondSS = Semester("label", "abbrev", LocalDate.now.plusYears(3).withMonthOfYear(5), LocalDate.now, LocalDate.now, Semester.randomUUID)

      val semesters = Set(firstWS, secondWS, firstSS, secondSS)

      when(repository.getAll[Semester](anyObject())).thenReturn(Success(semesters))

      val first = firstSS.start.getYear
      val second = secondSS.start.getYear
      val period = "SS"

      val request = FakeRequest(
        GET,
        s"/${entityTypeName.toLowerCase}s?year=${first.toString},${second.toString}&period=$period"
      )
      val result = controller.asInstanceOf[SemesterCRUDController].all()(request)

      status(result) shouldBe OK
      contentType(result) shouldBe Some[String](mimeType)
      contentAsJson(result) shouldBe Json.toJson(Set(firstSS, secondSS))
    }

    "not return semesters for a specific period when there is not match" in {
      val semesterInWS = Semester("label", "abbrev", LocalDate.now.withMonthOfYear(10), LocalDate.now, LocalDate.now, Semester.randomUUID)
      val semesterInSS = Semester("label", "abbrev", LocalDate.now.withMonthOfYear(3), LocalDate.now, LocalDate.now, Semester.randomUUID)
      val semesters = Set(semesterInSS, semesterInWS)

      when(repository.getAll[Semester](anyObject())).thenReturn(Success(semesters))

      val year = semesterInSS.start.getYear
      val period = "invalid period"

      val request = FakeRequest(
        GET,
        s"/${entityTypeName.toLowerCase}s?year=${year.toString}&period=$period"
      )
      val result = controller.asInstanceOf[SemesterCRUDController].all()(request)

      status(result) shouldBe SERVICE_UNAVAILABLE
      contentType(result) shouldBe Some("application/json")
      contentAsJson(result) shouldBe Json.obj(
        "status" -> "KO",
        "message" -> "Unknown attribute"
      )
    }

    "not return semesters for a specific period when there is an exception" in {
      val errorMessage = s"Oops, cant get the desired semesters for a year for some reason"
      val semesterInSS = Semester("label", "abbrev", LocalDate.now.withMonthOfYear(3), LocalDate.now, LocalDate.now, Semester.randomUUID)

      when(repository.getAll[Semester](anyObject())).thenReturn(Failure(new Exception(errorMessage)))

      val year = semesterInSS.start.getYear
      val period = "SS"

      val request = FakeRequest(
        GET,
        s"/${entityTypeName.toLowerCase}s/${year.toString}/$period"
      )
      val result = controller.asInstanceOf[SemesterCRUDController].all()(request)

      status(result) shouldBe INTERNAL_SERVER_ERROR
      contentType(result) shouldBe Some("application/json")
      contentAsJson(result) shouldBe Json.obj(
        "status" -> "KO",
        "errors" -> errorMessage
      )
    }

    s"handle this model issue when creating a new $entityTypeName which already exists" in {
      when(repository.prepareQuery(anyObject())).thenReturn(query)
      when(qe.execute(anyObject())).thenReturn(Success(Map(
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
      when(repository.get[Semester](anyObject())(anyObject())).thenReturn(Success(None))
      when(repository.prepareQuery(Matchers.anyObject())).thenReturn(query)
      when(qe.execute(anyObject())).thenReturn(Success(Map(
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
