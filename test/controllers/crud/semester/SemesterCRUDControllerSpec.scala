package controllers.crud.semester

import org.joda.time.{DateTime, Interval, LocalDate}
import controllers.crud.AbstractCRUDControllerSpec
import models.semester.{Semester, SemesterProtocol}
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
import base.StreamHandler._
import org.joda.time.format.DateTimeFormat

import scala.util.{Failure, Success}

object SemesterCRUDControllerSpec {

  def populate = {
    val template = {
      val start = LocalDate.now.withDayOfWeek(1).withMonthOfYear(9).minusYears(5)
      Semester("template", "template", start, start.plusMonths(6), start.plusMonths(5))
    }

    (0 until 20).foldLeft((Set.empty[Semester], template)) {
      case ((vector, prev), i) =>
        val current = Semester(i.toString, i.toString, prev.end.plusDays(1), prev.end.plusDays(1).plusMonths(6), prev.end.plusDays(1).plusMonths(5))
        (vector + current, current)
    }._1
  }
}

class SemesterCRUDControllerSpec extends AbstractCRUDControllerSpec[SemesterProtocol, Semester, Semester] {

  override val entityToPass: Semester = Semester("label to pass", "abbreviation to pass", LocalDate.now, LocalDate.now, LocalDate.now)

  override val entityToFail: Semester = Semester("label to fail", "abbreviation to fail", LocalDate.now, LocalDate.now, LocalDate.now)

  override implicit val jsonWrites: Writes[Semester] = Semester.writes

  override val atomizedEntityToPass: Semester = entityToPass

  override val atomizedEntityToFail: Semester = entityToFail

  override val jsonWritesAtom: Writes[Semester] = jsonWrites

  override val controller: SemesterCRUDController = new SemesterCRUDController(repository, sessionService, namespace, roleService) {

    override protected def fromInput(input: SemesterProtocol, existing: Option[Semester]): Semester = entityToPass

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
      import models.semester.Semester.reads
      val semesters = SemesterCRUDControllerSpec.populate

      when(repository.getAll[Semester](anyObject())).thenReturn(Success(semesters))

      val request = FakeRequest(
        GET,
        s"/${entityTypeName.toLowerCase}/current"
      )

      val result = controller.asInstanceOf[SemesterCRUDController].current()(request)
      val current = semesters.filter(semester => semester.start.isBefore(LocalDate.now) && semester.end.isAfter(LocalDate.now))

      status(result) shouldBe OK
      contentType(result) shouldBe Some(mimeType.value)
      contentFromStream(result) shouldBe current.map(s => Json.toJson(s))
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
