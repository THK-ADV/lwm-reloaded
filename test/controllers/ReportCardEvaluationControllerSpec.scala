package controllers

import java.util.UUID

import base.StreamHandler._
import models._
import org.joda.time.{DateTime, LocalDate}
import org.mockito.Matchers._
import org.mockito.Mockito.{doReturn, when}
import org.mockito.stubbing.OngoingStubbing
import org.openrdf.model.Value
import org.scalatest.mock.MockitoSugar._
import org.w3.banana.PointedGraph
import org.w3.banana.sesame.Sesame
import play.api.http.HeaderNames
import play.api.libs.json.{JsValue, Json, Writes}
import play.api.test.Helpers._
import play.api.test.{FakeHeaders, FakeRequest}
import services.ReportCardService
import utils.LwmMimeType

import scala.util.{Failure, Success, Try}

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

  val semesterToPass = Semester("label to pass", "abbrev to pass", LocalDate.now, LocalDate.now, LocalDate.now)
  val semesterToFail = Semester("label to pass", "abbrev to pass", LocalDate.now, LocalDate.now, LocalDate.now)

  val employeeToPass = Employee("systemId to pass", "last name to pass", "first name to pass", "email to pass", "employee")
  val employeeToFail = Employee("systemId to fail", "last name to fail", "first name to fail", "email to fail", "employee")

  val degreeToPass = Degree("label to pass", "abbrev to pass")
  val degreeToFail = Degree("label to fail", "abbrev to fail")

  val courseToPass = CourseAtom("label to pass", "desc to pass", "abbrev to pass", employeeToPass, 1, None, UUID.randomUUID)
  val courseToFail = CourseAtom("label to fail", "desc to fail", "abbrev to fail", employeeToFail, 1, None, UUID.randomUUID)

  val labworkToPass = LabworkAtom("label to pass", "desc to pass", semesterToPass, courseToPass, degreeToPass, subscribable = false, published = false, None, UUID.randomUUID)
  val labworkToFail = LabworkAtom("label to fail", "desc to fail", semesterToFail, courseToFail, degreeToFail, subscribable = false, published = false, None, UUID.randomUUID)

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

  def evaluations(student: UUID = UUID.randomUUID, labwork: UUID = UUID.randomUUID) = (0 until 20).map { i =>
    ReportCardEvaluation(student, labwork, i.toString, i % 2 == 0, i % 10)
  }.toSet

  def toJson(entries: Set[ReportCardEvaluation]) = entries.map(e => Json.toJson(e))

  "A ReportCardEvaluationControllerSpec " should {

    "successfully return report card evaluations for a given student" in {
      val student = UUID.randomUUID
      val evals = evaluations(student)

      whenFiltered(evals)


      val request = FakeRequest(
        GET,
        s"/reportCardEvaluations/student/$student"
      )

      val result = controller.get(student.toString)(request)

      status(result) shouldBe OK
      contentType(result) shouldBe Some[String](mimeType)
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

    def whenFiltered(evals: Set[ReportCardEvaluation]): OngoingStubbing[Try[Set[ReportCardEvaluation]]] = {
      when(repository.prepareQuery(anyObject())).thenReturn(query)
      when(qe.parse(anyObject())).thenReturn(sparqlOps.parseSelect("SELECT * where {}"))
      when(qe.execute(anyObject())).thenReturn(Success(Map.empty[String, List[Value]]))
      when(repository.getMany[ReportCardEvaluation](anyObject())(anyObject())).thenReturn(Success(evals))
    }

    "successfully return all report card evaluations for a given course and labwork" in {
      val course = UUID.randomUUID
      val labwork = UUID.randomUUID
      val evals = evaluations(UUID.randomUUID, labwork)

      whenFiltered(evals)

      val request = FakeRequest(
        GET,
        s"/courses/$course/labworks/$labwork/reportCardEvaluations"
      )

      val result = controller.allFrom(course.toString, labwork.toString)(request)

      status(result) shouldBe OK
      contentType(result) shouldBe Some[String](mimeType)
      contentFromStream(result) shouldBe evals.map(eval => Json.toJson(eval))
    }

    def whenPreview(evals: Set[ReportCardEvaluation]): OngoingStubbing[Set[ReportCardEvaluation]] = {
      doReturn(query).when(repository).prepareQuery(anyObject())
      doReturn(Success(
        Map("ap" -> List(factory.createURI(AssignmentPlan.generateUri(UUID.randomUUID())(namespace))))
      )).doReturn(Success(
        Map("cards" -> List(factory.createURI(ReportCardEntry.generateUri(UUID.randomUUID())(namespace))))
      )).when(qe).execute(anyObject())

      when(repository.get[AssignmentPlan](anyObject())(anyObject())).thenReturn(Success(Some(AssignmentPlan.empty)))
      when(repository.getMany[ReportCardEntry](anyObject())(anyObject())).thenReturn(Success(Set.empty[ReportCardEntry]))

      when(reportCardService.evaluate(anyObject(), anyObject())).thenReturn(evals)
    }

    "successfully preview report card evaluations for a given labwork" in {
      val course = UUID.randomUUID
      val labwork = UUID.randomUUID
      val evals = evaluations(UUID.randomUUID, labwork)

      whenPreview(evals)

      val request = FakeRequest(
        GET,
        s"/courses/$course/labworks/$labwork/reportCardEvaluations/preview"
      )

      val result = controller.preview(course.toString, labwork.toString)(request)

      status(result) shouldBe OK
      contentType(result) shouldBe Some[String](mimeType)
      contentFromStream(result) shouldBe evals.map(eval => Json.toJson(eval))
    }

    def whenDeleteAndCreate(deleted: Set[Unit], added: Set[ReportCardEvaluation]): OngoingStubbing[Try[Set[PointedGraph[Sesame]]]] = {
      when(repository.deleteMany[ReportCardEvaluation](anyObject())(anyObject())).thenReturn(Success(deleted))
      when(repository.addMany[ReportCardEvaluation](anyObject())(anyObject())).thenReturn(Success(added.map(_.toPG)))
    }

    "successfully create report card evaluations for a given labwork" in {
      val course = UUID.randomUUID
      val labwork = UUID.randomUUID
      val evals = evaluations(UUID.randomUUID, labwork)

      doReturn(query).when(repository).prepareQuery(anyObject())
      doReturn(Success(Map.empty[String, List[Value]])).doReturn(Success(
        Map("ap" -> List(factory.createURI(AssignmentPlan.generateUri(UUID.randomUUID())(namespace))))
      )).doReturn(Success(
        Map("cards" -> List(factory.createURI(ReportCardEntry.generateUri(UUID.randomUUID())(namespace))))
      )).when(qe).execute(anyObject())

      when(repository.get[AssignmentPlan](anyObject())(anyObject())).thenReturn(Success(Some(AssignmentPlan.empty)))
      doReturn(Success(Set.empty)).doReturn(Success(evals)).when(repository).getMany(anyObject())(anyObject())

      when(reportCardService.evaluate(anyObject(), anyObject())).thenReturn(evals)

      whenDeleteAndCreate(Set.empty[Unit], evals)

      val request = FakeRequest(
        POST,
        s"/courses/$course/labworks/$labwork/reportCardEvaluations",
        FakeHeaders(Seq(HeaderNames.CONTENT_TYPE -> mimeType)),
        Json.obj("" -> "")
      )

      val result = controller.createFrom(course.toString, labwork.toString)(request)

      status(result) shouldBe CREATED
      contentType(result) shouldBe Some[String](mimeType)
      contentFromStream(result) shouldBe evals.map(eval => Json.toJson(eval))
    }

    "successfully delete and create new report card evaluations for a given labwork" in {
      val course = UUID.randomUUID
      val labwork = UUID.randomUUID
      val evals = evaluations(UUID.randomUUID, labwork)

      doReturn(query).when(repository).prepareQuery(anyObject())
      doReturn(Success(Map.empty[String, List[Value]])).doReturn(Success(
        Map("ap" -> List(factory.createURI(AssignmentPlan.generateUri(UUID.randomUUID())(namespace))))
      )).doReturn(Success(
        Map("cards" -> List(factory.createURI(ReportCardEntry.generateUri(UUID.randomUUID())(namespace))))
      )).when(qe).execute(anyObject())

      when(repository.get[AssignmentPlan](anyObject())(anyObject())).thenReturn(Success(Some(AssignmentPlan.empty)))
      doReturn(Success(evals)).doReturn(Success(evals)).when(repository).getMany(anyObject())(anyObject())

      when(reportCardService.evaluate(anyObject(), anyObject())).thenReturn(evals)

      whenDeleteAndCreate(evals.map(_ => ()), evals)

      val request = FakeRequest(
        POST,
        s"/courses/$course/labworks/$labwork/reportCardEvaluations",
        FakeHeaders(Seq(HeaderNames.CONTENT_TYPE -> mimeType)),
        Json.obj("" -> "")
      )

      val result = controller.createFrom(course.toString, labwork.toString)(request)

      status(result) shouldBe CREATED
      contentType(result) shouldBe Some[String](mimeType)
      contentFromStream(result) shouldBe evals.map(eval => Json.toJson(eval))
    }

    "successfully create report card evaluations for a given student explicitly" in {
      val course = UUID.randomUUID
      val labwork = UUID.randomUUID
      val student = UUID.randomUUID
      val evals = evaluations(student, labwork).take(ReportCardEntryType.all.size)

      whenFiltered(Set.empty)
      whenDeleteAndCreate(Set.empty[Unit], evals)
      when(reportCardService.evaluateExplicit(anyObject(), anyObject())).thenReturn(evals)

      val request = FakeRequest(
        POST,
        s"/courses/$course/labworks/$labwork/students/$student/reportCardEvaluations",
        FakeHeaders(Seq(HeaderNames.CONTENT_TYPE -> mimeType)),
        Json.obj("" -> "")
      )

      val result = controller.createForStudent(course.toString, labwork.toString, student.toString)(request)

      status(result) shouldBe CREATED
      contentType(result) shouldBe Some[String](mimeType)
      contentFromStream(result) shouldBe evals.map(eval => Json.toJson(eval))
    }

    "successfully delete and create new report card evaluations for a given student explicitly" in {
      val course = UUID.randomUUID
      val labwork = UUID.randomUUID
      val student = UUID.randomUUID
      val evals = evaluations(student, labwork).take(ReportCardEntryType.all.size)

      whenFiltered(evals)
      whenDeleteAndCreate(evals.map(_ => ()), evals)
      when(reportCardService.evaluateExplicit(anyObject(), anyObject())).thenReturn(evals)

      val request = FakeRequest(
        POST,
        s"/courses/$course/labworks/$labwork/students/$student/reportCardEvaluations",
        FakeHeaders(Seq(HeaderNames.CONTENT_TYPE -> mimeType)),
        Json.obj("" -> "")
      )

      val result = controller.createForStudent(course.toString, labwork.toString, student.toString)(request)

      status(result) shouldBe CREATED
      contentType(result) shouldBe Some[String](mimeType)
      contentFromStream(result) shouldBe evals.map(eval => Json.toJson(eval))
    }

    "not create report card evaluations for a given student explicitly when preconditon fails for some reason" in {
      val course = UUID.randomUUID
      val labwork = UUID.randomUUID
      val student = UUID.randomUUID
      val evals = evaluations(student, labwork)

      whenFiltered(evals)

      val request = FakeRequest(
        POST,
        s"/courses/$course/labworks/$labwork/students/$student/reportCardEvaluations",
        FakeHeaders(Seq(HeaderNames.CONTENT_TYPE -> mimeType)),
        Json.obj("" -> "")
      )

      val result = controller.createForStudent(course.toString, labwork.toString, student.toString)(request)

      status(result) shouldBe PRECONDITION_FAILED
    }
  }
}
