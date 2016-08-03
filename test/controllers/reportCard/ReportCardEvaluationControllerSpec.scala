package controllers.reportCard

import java.util.UUID

import base.TestBaseDefinition
import models.labwork.{AssignmentPlan, ReportCardEntry, ReportCardEvaluation}
import org.joda.time.{LocalDate, LocalTime}
import org.openrdf.model.impl.ValueFactoryImpl
import org.scalatest.WordSpec
import org.mockito.Mockito.when
import org.scalatest.mock.MockitoSugar._
import org.w3.banana.sesame.SesameModule
import play.api.test.Helpers._
import play.api.test.{FakeHeaders, FakeRequest}
import services.{ReportCardService, RoleService, SessionHandlingService}
import store.sparql.{QueryEngine, QueryExecutor, SelectClause}
import store.{Namespace, SesameRepository}
import utils.LwmMimeType
import org.mockito.Matchers._
import org.openrdf.model.Value
import org.w3.banana.PointedGraph
import play.api.libs.json.Json

import scala.util.{Failure, Success}

class ReportCardEvaluationControllerSpec extends WordSpec with TestBaseDefinition with SesameModule {

  val repository = mock[SesameRepository]
  val roleService = mock[RoleService]
  val reportCardService = mock[ReportCardService]
  val namespace = Namespace("http://lwm.gm.th-koeln.de")
  val sessionService = mock[SessionHandlingService]
  val factory = ValueFactoryImpl.getInstance()
  val qe = mock[QueryExecutor[SelectClause]]
  val query = QueryEngine.empty(qe)

  val mimeType = LwmMimeType.reportCardEvaluationV1Json

  val controller: ReportCardEvaluationController = new ReportCardEvaluationController(repository, sessionService, namespace, roleService, reportCardService) {
    override protected def contextFrom: PartialFunction[Rule, SecureContext] = {
      case _ => NonSecureBlock
    }

    override protected def restrictedContext(restrictionId: String): PartialFunction[Rule, SecureContext] = {
      case _ => NonSecureBlock
    }
  }

  def evaluations(student: UUID = UUID.randomUUID, labwork: UUID = UUID.randomUUID) = (0 until 4).map { i =>
    ReportCardEvaluation(student, labwork, i.toString, bool = true, i)
  }.toSet

  def toJson(entries: Set[ReportCardEvaluation]) = entries.map(e => Json.toJson(e))

  "A ReportCardEvaluationControllerSpec " should {
    /* apply changes
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

      val jsValues = toJson(evals)
      val content = contentAsString(result)

      jsValues.forall { json =>
        content contains json.toString
      } shouldBe true
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

      val result = controller.all(course.toString, labwork.toString)(request)

      status(result) shouldBe OK
      contentType(result) shouldBe Some(mimeType.value)

      val jsValues = toJson(evals)
      val content = contentAsString(result)

      jsValues.forall { json =>
        content contains json.toString
      } shouldBe true
    }

    "successfully preview all report card evaluations for a given course and labwork" in {
      val course = UUID.randomUUID
      val labwork = UUID.randomUUID
      val evals = evaluations(UUID.randomUUID, labwork)
      val cardEntries = (0 until 4).map { i =>
        ReportCardEntry(UUID.randomUUID, labwork, i.toString, LocalDate.now, LocalTime.now, LocalTime.now, UUID.randomUUID, Set.empty)
      }.toSet
      val ap = AssignmentPlan(labwork, 0, 0, Set.empty)

      when(repository.prepareQuery(anyObject())).thenReturn(query)
      when(qe.parse(anyObject())).thenReturn(sparqlOps.parseSelect("SELECT * where {}"))
      when(qe.execute(anyObject())).thenReturn(Success(Map.empty[String, List[Value]]))
      when(repository.getMany[ReportCardEntry](anyObject())(anyObject())).thenReturn(Success(cardEntries))
      when(repository.getAll[AssignmentPlan](anyObject())).thenReturn(Success(Set(ap)))
      when(reportCardService.evaluate(anyObject(), anyObject())).thenReturn(evals)

      val request = FakeRequest(
        GET,
        s"/courses/$course/labworks/$labwork/reportCardEvaluations/preview"
      )

      val result = controller.preview(course.toString, labwork.toString)(request)

      status(result) shouldBe OK
      contentType(result) shouldBe Some(mimeType.value)

      val jsValues = toJson(evals)
      val content = contentAsString(result)

      jsValues.forall { json =>
        content contains json.toString
      } shouldBe true
    }

    "not preview report card evaluations when something is not found" in {
      val course = UUID.randomUUID
      val labwork = UUID.randomUUID
      val evals = evaluations(UUID.randomUUID, labwork)
      val cardEntries = (0 until 4).map { i =>
        ReportCardEntry(UUID.randomUUID, labwork, i.toString, LocalDate.now, LocalTime.now, LocalTime.now, UUID.randomUUID, Set.empty)
      }.toSet

      when(repository.prepareQuery(anyObject())).thenReturn(query)
      when(qe.parse(anyObject())).thenReturn(sparqlOps.parseSelect("SELECT * where {}"))
      when(qe.execute(anyObject())).thenReturn(Success(Map.empty[String, List[Value]]))
      when(repository.getMany[ReportCardEntry](anyObject())(anyObject())).thenReturn(Success(cardEntries))
      when(repository.getAll[AssignmentPlan](anyObject())).thenReturn(Success(Set(AssignmentPlan.empty)))
      when(reportCardService.evaluate(anyObject(), anyObject())).thenReturn(evals)

      val request = FakeRequest(
        GET,
        s"/courses/$course/labworks/$labwork/reportCardEvaluations/preview"
      )

      val result = controller.preview(course.toString, labwork.toString)(request)

      status(result) shouldBe NOT_FOUND
    }

    "successfully create report card evaluations for a given course and labwork" in {
      val course = UUID.randomUUID
      val labwork = UUID.randomUUID
      val evals = evaluations(UUID.randomUUID, labwork)
      val cardEntries = (0 until 4).map { i =>
        ReportCardEntry(UUID.randomUUID, labwork, i.toString, LocalDate.now, LocalTime.now, LocalTime.now, UUID.randomUUID, Set.empty)
      }.toSet
      val ap = AssignmentPlan(labwork, 0, 0, Set.empty)

      when(repository.prepareQuery(anyObject())).thenReturn(query)
      when(qe.parse(anyObject())).thenReturn(sparqlOps.parseSelect("SELECT * where {}"))
      when(qe.execute(anyObject())).thenReturn(Success(Map.empty[String, List[Value]]))
      when(repository.getMany[ReportCardEntry](anyObject())(anyObject())).thenReturn(Success(cardEntries))
      when(repository.getAll[AssignmentPlan](anyObject())).thenReturn(Success(Set(ap)))
      when(reportCardService.evaluate(anyObject(), anyObject())).thenReturn(evals)
      when(repository.addMany[ReportCardEvaluation](anyObject())(anyObject())).thenReturn(Success(Set(PointedGraph[Rdf](factory.createBNode()))))

      val request = FakeRequest(
        POST,
        s"/courses/$course/labworks/$labwork/reportCardEvaluations",
        FakeHeaders(Seq("Content-Type" -> mimeType)),
        Json.toJson("")
      )

      val result = controller.create(course.toString, labwork.toString)(request)

      status(result) shouldBe CREATED
      contentType(result) shouldBe Some(mimeType.value)

      val jsValues = toJson(evals)
      val content = contentAsString(result)

      jsValues.forall { json =>
        content contains json.toString
      } shouldBe true
    }*/
  }
}
