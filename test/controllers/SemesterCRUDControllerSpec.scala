package controllers

import base.StreamHandler._
import models._
import org.joda.time.LocalDate
import org.mockito.Matchers
import org.mockito.Matchers._
import org.mockito.Mockito._
import org.w3.banana.PointedGraph
import org.w3.banana.sesame.Sesame
import play.api.http.HeaderNames
import play.api.libs.json.{JsValue, Json, Writes}
import play.api.test.Helpers._
import play.api.test.{FakeHeaders, FakeRequest}
import utils.LwmMimeType

import scala.util.Success

object SemesterCRUDControllerSpec {

  def populate = {
    val template = {
      val start = LocalDate.now.withDayOfWeek(1).withMonthOfYear(9).minusYears(5)
      SesameSemester("template", "template", start, start.plusMonths(6), start.plusMonths(5))
    }

    (0 until 20).foldLeft((Set.empty[SesameSemester], template)) {
      case ((vector, prev), i) =>
        val current = SesameSemester(i.toString, i.toString, prev.end.plusDays(1), prev.end.plusDays(1).plusMonths(6), prev.end.plusDays(1).plusMonths(5))
        (vector + current, current)
    }._1
  }
}

class SemesterCRUDControllerSpec extends AbstractCRUDControllerSpec[SemesterProtocol, SesameSemester, SesameSemester] {

  override val entityToPass: SesameSemester = SesameSemester("label to pass", "abbreviation to pass", LocalDate.now, LocalDate.now, LocalDate.now)

  override val entityToFail: SesameSemester = SesameSemester("label to fail", "abbreviation to fail", LocalDate.now, LocalDate.now, LocalDate.now)

  override implicit val jsonWrites: Writes[SesameSemester] = SesameSemester.writes

  override val atomizedEntityToPass: SesameSemester = entityToPass

  override val atomizedEntityToFail: SesameSemester = entityToFail

  override val jsonWritesAtom: Writes[SesameSemester] = jsonWrites

  override val controller: SemesterCRUDController = new SemesterCRUDController(repository, sessionService, namespace, roleService) {

    override protected def fromInput(input: SemesterProtocol, existing: Option[SesameSemester]): SesameSemester = entityToPass

    override protected def contextFrom: PartialFunction[Rule, SecureContext] = {
      case _ => NonSecureBlock
    }
  }

  override val mimeType: LwmMimeType = LwmMimeType.semesterV1Json

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

    "successfully return current semester" in {
      import controllers.SemesterCRUDController._
      val semesters = SemesterCRUDControllerSpec.populate

      when(repository.getAll[SesameSemester](anyObject())).thenReturn(Success(semesters))

      val validRequest = FakeRequest(
        GET,
        s"/${entityTypeName.toLowerCase}?$selectAttribute=$currentValue"
      )

      val invalidRequest = FakeRequest(
        GET,
        s"/${entityTypeName.toLowerCase}?$selectAttribute=not_current"
      )

      val validResult = controller.all()(validRequest)
      val invalidResult = controller.all()(invalidRequest)
      val current = semesters.filter(semester => semester.start.isBefore(LocalDate.now) && semester.end.isAfter(LocalDate.now))

      status(validResult) shouldBe OK
      contentType(validResult) shouldBe Some(mimeType.value)
      contentFromStream(validResult) shouldBe current.map(s => Json.toJson(s))

      status(invalidResult) shouldBe SERVICE_UNAVAILABLE
      contentType(invalidResult) shouldBe Some("application/json")
      contentAsJson(invalidResult) shouldBe Json.obj(
        "status" -> "KO",
        "message" -> s"Value of $selectAttribute should be $currentValue, but was not_current"
      )
    }

    s"handle this model issue when creating a new $entityTypeName which already exists" in {
      when(repository.prepareQuery(anyObject())).thenReturn(query)
      when(qe.execute(anyObject())).thenReturn(Success(Map(
        "s" -> List(factory.createLiteral(SesameSemester.generateUri(entityToPass)))
      )))
      when(repository.get[SesameSemester](anyObject())(anyObject())).thenReturn(Success(Some(entityToPass)))

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
      doReturn(Success(None)).doReturn(Success(Some(entityToPass))).when(repository).get(anyObject())(anyObject())
      when(repository.prepareQuery(Matchers.anyObject())).thenReturn(query)
      when(qe.execute(anyObject())).thenReturn(Success(Map(
        "s" -> List(factory.createLiteral(SesameSemester.generateUri(entityToPass)))
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
