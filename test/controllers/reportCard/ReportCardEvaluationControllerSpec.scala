package controllers.reportCard

import java.util.UUID

import base.StreamHandler._
import controllers.crud.AbstractCRUDControllerSpec
import models.labwork._
import models.users.Student
import org.joda.time.DateTime
import org.mockito.Matchers._
import org.mockito.Mockito.when
import org.openrdf.model.Value
import org.scalatest.mock.MockitoSugar._
import org.w3.banana.PointedGraph
import org.w3.banana.sesame.Sesame
import play.api.libs.json.{JsValue, Json, Writes}
import play.api.test.FakeRequest
import play.api.test.Helpers._
import services.ReportCardService
import utils.LwmMimeType

import scala.util.{Failure, Success}

class ReportCardEvaluationControllerSpec extends AbstractCRUDControllerSpec[ReportCardEvaluation, ReportCardEvaluation, ReportCardEvaluationAtom]{

  val reportCardService = mock[ReportCardService]

  val mimeType = LwmMimeType.reportCardEvaluationV1Json

  val controller: ReportCardEvaluationController = new ReportCardEvaluationController(repository, sessionService, namespace, roleService, reportCardService) {

    override protected def fromInput(input: ReportCardEvaluation, existing: Option[ReportCardEvaluation]): ReportCardEvaluation = entityToPass

    override protected def compareModel(input: ReportCardEvaluation, output: ReportCardEvaluation): Boolean = input == output

    override protected def contextFrom: PartialFunction[Rule, SecureContext] = {
      case _ => NonSecureBlock
    }

    override protected def restrictedContext(restrictionId: String): PartialFunction[Rule, SecureContext] = {
      case _ => NonSecureBlock
    }
  }

  import bindings.ReportCardEvaluationDescriptor
  import ops._

  implicit val binder = ReportCardEvaluationDescriptor.binder

  val labworkToPass = Labwork("label to pass", "desc to pass", UUID.randomUUID(), UUID.randomUUID(), UUID.randomUUID())
  val labworkToFail = Labwork("label to fail", "desc to fail", UUID.randomUUID(), UUID.randomUUID(), UUID.randomUUID())

  val studentToPass = Student("systemId to pass", "last name to pass", "first name to pass", "email to pass", "regId to pass", UUID.randomUUID())
  val studentToFail = Student("systemId to fail", "last name to fail", "first name to fail", "email to fail", "regId to fail", UUID.randomUUID())

  override val entityToPass: ReportCardEvaluation = ReportCardEvaluation(studentToPass.id, labworkToPass.id, "label to pass", bool = true, 2)
  override val entityToFail: ReportCardEvaluation = ReportCardEvaluation(studentToFail.id, labworkToFail.id, "label to fail", bool = false, 2)

  override val pointedGraph: PointedGraph[Sesame] = entityToPass.toPG

  override val atomizedEntityToPass: ReportCardEvaluationAtom = ReportCardEvaluationAtom(
    studentToPass,
    labworkToPass,
    entityToPass.label,
    entityToPass.bool,
    entityToPass.int,
    entityToPass.timestamp,
    entityToPass.invalidated,
    entityToPass.id
  )

  override val atomizedEntityToFail: ReportCardEvaluationAtom = ReportCardEvaluationAtom(
    studentToFail,
    labworkToFail,
    entityToFail.label,
    entityToFail.bool,
    entityToFail.int,
    entityToFail.timestamp,
    entityToFail.invalidated,
    entityToFail.id
  )

  override def entityTypeName: String = "reportCardEvaluation"

  override val inputJson: JsValue = Json.obj(
    "student" -> entityToPass.student,
    "labwork" -> entityToPass.labwork,
    "label" -> entityToPass.label,
    "bool" -> entityToPass.bool,
    "int" -> entityToPass.int,
    "timestamp" -> entityToPass.timestamp,
    "id" -> entityToPass.id
  )

  override val updateJson: JsValue = Json.obj(
    "student" -> entityToPass.student,
    "labwork" -> entityToPass.labwork,
    "label" -> entityToPass.label,
    "bool" -> !entityToPass.bool,
    "int" -> (entityToPass.int + 2),
    "timestamp" -> DateTime.now,
    "id" -> entityToPass.id
  )

  override implicit def jsonWrites: Writes[ReportCardEvaluation] = ReportCardEvaluation.writes

  override implicit def jsonWritesAtom: Writes[ReportCardEvaluationAtom] = ReportCardEvaluation.writesAtom

  def evaluations(student: UUID = UUID.randomUUID, labwork: UUID = UUID.randomUUID) = (0 until 4).map { i =>
    ReportCardEvaluation(student, labwork, i.toString, bool = true, i)
  }.toSet

  def toJson(entries: Set[ReportCardEvaluation]) = entries.map(e => Json.toJson(e))

  "A ReportCardEvaluationControllerSpec " should {

    "successfully return report card evaluations for a given student" in {
      val student = UUID.randomUUID
      val evals = evaluations(student)

      when(repository.prepareQuery(anyObject())).thenReturn(query)
      when(qe.parse(anyObject())).thenReturn(sparqlOps.parseSelect("SELECT * where {}"))
      when(qe.execute(anyObject())).thenReturn(Success(Map.empty[String, List[Value]]))
      when(repository.getMany[ReportCardEvaluation](anyObject())(anyObject())).thenReturn(Success(evals))

      val request = FakeRequest(
        GET,
        s"/reportCardEvaluations/student/$student"
      )

      val result = controller.get(student.toString)(request)

      status(result) shouldBe OK
      contentType(result) shouldBe Some(mimeType.value)
      contentFromStream(result) shouldBe evals.map(eval => Json.toJson(eval))
    }

    "not return report card evaluations when there is an exception" in {
      val student = UUID.randomUUID
      val errMsg = "Oops, something went wrong"

      when(repository.prepareQuery(anyObject())).thenReturn(query)
      when(qe.parse(anyObject())).thenReturn(sparqlOps.parseSelect("SELECT * where {}"))
      when(qe.execute(anyObject())).thenReturn(Success(Map.empty[String, List[Value]]))
      when(repository.getMany[ReportCardEvaluation](anyObject())(anyObject())).thenReturn(Failure(new Exception(errMsg)))

      val request = FakeRequest(
        GET,
        s"/reportCardEvaluations/student/$student"
      )

      val result = controller.get(student.toString)(request)

      status(result) shouldBe SERVICE_UNAVAILABLE
      contentAsJson(result) shouldBe Json.obj(
        "status" -> "KO",
        "message" -> errMsg
      )
    }

    "successfully return all report card evaluations for a given course and labwork" in {
      val course = UUID.randomUUID
      val labwork = UUID.randomUUID
      val evals = evaluations(UUID.randomUUID, labwork)

      when(repository.prepareQuery(anyObject())).thenReturn(query)
      when(qe.parse(anyObject())).thenReturn(sparqlOps.parseSelect("SELECT * where {}"))
      when(qe.execute(anyObject())).thenReturn(Success(Map.empty[String, List[Value]]))
      when(repository.getMany[ReportCardEvaluation](anyObject())(anyObject())).thenReturn(Success(evals))

      val request = FakeRequest(
        GET,
        s"/courses/$course/labworks/$labwork/reportCardEvaluations"
      )

      val result = controller.allFrom(course.toString, labwork.toString)(request)

      status(result) shouldBe OK
      contentType(result) shouldBe Some(mimeType.value)
      contentFromStream(result) shouldBe evals.map(eval => Json.toJson(eval))
    }

    // TODO test preview

    // TODO test create
  }
}
