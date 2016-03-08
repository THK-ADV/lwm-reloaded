package controllers.crud

import java.util.UUID

import models._
import models.semester.Semester
import models.users.Employee
import org.joda.time.LocalDate
import org.mockito.Matchers
import org.w3.banana.PointedGraph
import org.w3.banana.sesame.Sesame
import play.api.http.HeaderNames
import play.api.libs.json.{JsValue, Json, Writes}
import play.api.test.{FakeHeaders, FakeRequest}
import utils.LwmMimeType
import play.api.test.Helpers._
import org.mockito.Matchers._
import org.mockito.Mockito._
import store.sparql.{Initial, QueryExecutor, SelectClause}

import scala.util.{Failure, Success}

class LabworkCRUDControllerSpec extends AbstractCRUDControllerSpec[LabworkProtocol, Labwork] {

  val semesterToPass = Semester("label to pass", "abbrev to pass", LocalDate.now, LocalDate.now, LocalDate.now, Semester.randomUUID)
  val semesterToFail = Semester("label to pass", "abbrev to pass", LocalDate.now, LocalDate.now, LocalDate.now, Semester.randomUUID)

  val courseToPass = Course("label to pass", "desc to pass", "abbrev to pass", Employee.randomUUID, 1, Course.randomUUID)
  val courseToFail = Course("label to fail", "desc to fail", "abbrev to fail", Employee.randomUUID, 1, Course.randomUUID)

  val degreeToPass = Degree("label to pass", "abbrev to pass", Degree.randomUUID)
  val degreeToFail = Degree("label to fail", "abbrev to fail", Degree.randomUUID)

  override val entityToPass: Labwork = Labwork(
    "label to pass",
    "description to pass",
    semesterToPass.id,
    courseToPass.id,
    degreeToPass.id,
    AssignmentPlan(1, Set(AssignmentEntry(0, Set(EntryType("entry to pass"))))),
    Labwork.randomUUID
  )

  override def entityTypeName: String = "labwork"

  override val controller: AbstractCRUDController[LabworkProtocol, Labwork] = new LabworkCRUDController(repository, namespace, roleService) {

    override protected def fromInput(input: LabworkProtocol, id: Option[UUID]): Labwork = entityToPass

    override protected def contextFrom: PartialFunction[Rule, SecureContext] = {
      case _ => NonSecureBlock
    }
  }

  override val entityToFail: Labwork = Labwork(
    "label to fail",
    "description to fail",
    semesterToFail.id,
    courseToFail.id,
    degreeToFail.id,
    AssignmentPlan(1, Set(AssignmentEntry(0, Set(EntryType("entry to fail"))))),
    Labwork.randomUUID
  )

  override implicit val jsonWrites: Writes[Labwork] = Labwork.writes

  override val mimeType: LwmMimeType = LwmMimeType.labworkV1Json

  override val inputJson: JsValue = Json.obj(
    "label" -> entityToPass.label,
    "description" -> entityToPass.description,
    "semester" -> entityToPass.semester,
    "course" -> entityToPass.course,
    "degree" -> entityToPass.degree,
    "assignmentPlan" -> entityToPass.assignmentPlan
  )

  override val updateJson: JsValue = Json.obj(
    "label" -> entityToPass.label,
    "description" -> (entityToPass.description + "updated"),
    "semester" -> UUID.randomUUID(),
    "course" -> entityToPass.course,
    "degree" -> entityToPass.degree,
    "assignmentPlan" -> entityToPass.assignmentPlan
  )

  val atomizedEntityToPass = LabworkAtom(
    entityToPass.label,
    entityToPass.description,
    semesterToPass,
    courseToPass,
    degreeToPass,
    entityToPass.assignmentPlan,
    entityToPass.id
  )

  val atomizedEntityToFail = LabworkAtom(
    entityToFail.label,
    entityToFail.description,
    semesterToFail,
    courseToFail,
    degreeToFail,
    entityToFail.assignmentPlan,
    entityToFail.id
  )

  import bindings.LabworkBinding.labworkBinder
  import ops._

  override def pointedGraph: PointedGraph[Sesame] = entityToPass.toPG

  "A LabworkCRUDControllerSpec also " should {
    
    "return the corresponding labwork for a given course" in {
      val course = Course("label", "desc", "abbrev", Employee.randomUUID, 1, Course.randomUUID)
      val plan = AssignmentPlan(1, Set(AssignmentEntry(1, Set(EntryType("type")))))
      val first = Labwork("label 1", "description 1", Semester.randomUUID, Course.randomUUID, Degree.randomUUID, plan, Labwork.randomUUID)
      val second = Labwork("label 2", "description 2", Semester.randomUUID, course.id, Degree.randomUUID, plan, Labwork.randomUUID)
      val third = Labwork("label 3", "description 3", Semester.randomUUID, Course.randomUUID, Degree.randomUUID, plan, Labwork.randomUUID)
      val fourth = Labwork("label 4", "description 4", Semester.randomUUID, Course.randomUUID, Degree.randomUUID, plan, Labwork.randomUUID)

      val labworks = Set(first, second, third, fourth)

      when(repository.get[Labwork](anyObject(), anyObject())).thenReturn(Success(labworks))

      val request = FakeRequest(
        GET,
        s"/${entityTypeName.toLowerCase}s?${LabworkCRUDController.courseAttribute}=${course.id.toString}"
      )

      val result = controller.asInstanceOf[LabworkCRUDController].all()(request)

      status(result) shouldBe OK
      contentType(result) shouldBe Some[String](mimeType)
      contentAsJson(result) shouldBe Json.toJson(Set(second))
    }

    "return all corresponding labworks for a given course" in {
      val course = Course("label", "desc", "abbrev", Employee.randomUUID, 1, Course.randomUUID)
      val plan = AssignmentPlan(1, Set(AssignmentEntry(1, Set(EntryType("type")))))
      val first = Labwork("label 1", "description 1", Semester.randomUUID, Course.randomUUID, Degree.randomUUID, plan, Labwork.randomUUID)
      val second = Labwork("label 2", "description 2", Semester.randomUUID, course.id, Degree.randomUUID, plan, Labwork.randomUUID)
      val third = Labwork("label 3", "description 3", Semester.randomUUID, Course.randomUUID, Degree.randomUUID, plan, Labwork.randomUUID)
      val fourth = Labwork("label 4", "description 4", Semester.randomUUID, course.id, Degree.randomUUID, plan, Labwork.randomUUID)

      val labworks = Set(first, second, third, fourth)

      when(repository.get[Labwork](anyObject(), anyObject())).thenReturn(Success(labworks))

      val request = FakeRequest(
        GET,
        s"/${entityTypeName.toLowerCase}s?${LabworkCRUDController.courseAttribute}=${course.id.toString}"
      )

      val result = controller.asInstanceOf[LabworkCRUDController].all()(request)

      status(result) shouldBe OK
      contentType(result) shouldBe Some[String](mimeType)
      contentAsJson(result) shouldBe Json.toJson(Set(second, fourth))
    }

    "not return labworks for a course when there is no match" in {
      val course = Course("label", "desc", "abbrev", Employee.randomUUID, 1, Course.randomUUID)
      val plan = AssignmentPlan(1, Set(AssignmentEntry(1, Set(EntryType("type")))))
      val first = Labwork("label 1", "description 1", Semester.randomUUID, Course.randomUUID, Degree.randomUUID, plan, Labwork.randomUUID)
      val second = Labwork("label 2", "description 2", Semester.randomUUID, Course.randomUUID, Degree.randomUUID, plan, Labwork.randomUUID)
      val third = Labwork("label 3", "description 3", Semester.randomUUID, Course.randomUUID, Degree.randomUUID, plan, Labwork.randomUUID)
      val fourth = Labwork("label 4", "description 4", Semester.randomUUID, Course.randomUUID, Degree.randomUUID, plan, Labwork.randomUUID)

      val labworks = Set(first, second, third, fourth)

      when(repository.get[Labwork](anyObject(), anyObject())).thenReturn(Success(labworks))

      val request = FakeRequest(
        GET,
        s"/${entityTypeName.toLowerCase}s?${LabworkCRUDController.courseAttribute}=${course.id.toString}"
      )

      val result = controller.asInstanceOf[LabworkCRUDController].all()(request)

      status(result) shouldBe OK
      contentType(result) shouldBe Some[String](mimeType)
      contentAsJson(result) shouldBe Json.toJson(Set.empty[Labwork])
    }

    "not return labworks when there is an invalid query attribute" in {
      val course = Course("label", "desc", "abbrev", Employee.randomUUID, 1, Course.randomUUID)
      val plan = AssignmentPlan(1, Set(AssignmentEntry(1, Set(EntryType("type")))))
      val first = Labwork("label 1", "description 1", Semester.randomUUID, Course.randomUUID, Degree.randomUUID, plan, Labwork.randomUUID)
      val second = Labwork("label 2", "description 2", Semester.randomUUID, Course.randomUUID, Degree.randomUUID, plan, Labwork.randomUUID)
      val third = Labwork("label 3", "description 3", Semester.randomUUID, Course.randomUUID, Degree.randomUUID, plan, Labwork.randomUUID)
      val fourth = Labwork("label 4", "description 4", Semester.randomUUID, Course.randomUUID, Degree.randomUUID, plan, Labwork.randomUUID)

      val labworks = Set(first, second, third, fourth)

      when(repository.get[Labwork](anyObject(), anyObject())).thenReturn(Success(labworks))

      val request = FakeRequest(
        GET,
        s"/${entityTypeName.toLowerCase}s?unknownAttribute=${course.id.toString}"
      )

      val result = controller.asInstanceOf[LabworkCRUDController].all()(request)

      status(result) shouldBe SERVICE_UNAVAILABLE
      contentType(result) shouldBe Some("application/json")
      contentAsJson(result) shouldBe Json.obj(
        "status" -> "KO",
        "message" -> "Unknown attribute"
      )
    }

    "not return labworks when there is an invalid query parameter value" in {
      val invalidParameter = "invalidParameterValue"

      val plan = AssignmentPlan(1, Set(AssignmentEntry(1, Set(EntryType("type")))))
      val first = Labwork("label 1", "description 1", Semester.randomUUID, Course.randomUUID, Degree.randomUUID, plan, Labwork.randomUUID)
      val second = Labwork("label 2", "description 2", Semester.randomUUID, Course.randomUUID, Degree.randomUUID, plan, Labwork.randomUUID)
      val third = Labwork("label 3", "description 3", Semester.randomUUID, Course.randomUUID, Degree.randomUUID, plan, Labwork.randomUUID)
      val fourth = Labwork("label 4", "description 4", Semester.randomUUID, Course.randomUUID, Degree.randomUUID, plan, Labwork.randomUUID)

      val labworks = Set(first, second, third, fourth)

      when(repository.get[Labwork](anyObject(), anyObject())).thenReturn(Success(labworks))

      val request = FakeRequest(
        GET,
        s"/${entityTypeName.toLowerCase}s?${LabworkCRUDController.courseAttribute}=$invalidParameter"
      )

      val result = controller.asInstanceOf[LabworkCRUDController].all()(request)

      status(result) shouldBe SERVICE_UNAVAILABLE
      contentType(result) shouldBe Some("application/json")
      contentAsJson(result) shouldBe Json.obj(
        "status" -> "KO",
        "message" -> s"Invalid UUID string: $invalidParameter"
      )
    }

    "return the corresponding labwork for a given degree" in {
      val degree = Degree("label", "description", Degree.randomUUID)

      val plan = AssignmentPlan(1, Set(AssignmentEntry(1, Set(EntryType("type")))))
      val first = Labwork("label 1", "description 1", Semester.randomUUID, Course.randomUUID, degree.id, plan, Labwork.randomUUID)
      val second = Labwork("label 2", "description 2", Semester.randomUUID, Course.randomUUID, Degree.randomUUID, plan, Labwork.randomUUID)
      val third = Labwork("label 3", "description 3", Semester.randomUUID, Course.randomUUID, Degree.randomUUID, plan, Labwork.randomUUID)
      val fourth = Labwork("label 4", "description 4", Semester.randomUUID, Course.randomUUID, Degree.randomUUID, plan, Labwork.randomUUID)

      val labworks = Set(first, second, third, fourth)

      when(repository.get[Labwork](anyObject(), anyObject())).thenReturn(Success(labworks))

      val request = FakeRequest(
        GET,
        s"/${entityTypeName.toLowerCase}s?${LabworkCRUDController.degreeAttribute}=${degree.id.toString}"
      )

      val result = controller.asInstanceOf[LabworkCRUDController].all()(request)

      status(result) shouldBe OK
      contentType(result) shouldBe Some[String](mimeType)
      contentAsJson(result) shouldBe Json.toJson(Set(first))
    }

    "return all corresponding labworks for a given degree" in {
      val degree = Degree("label", "description", Degree.randomUUID)

      val plan = AssignmentPlan(1, Set(AssignmentEntry(1, Set(EntryType("type")))))
      val first = Labwork("label 1", "description 1", Semester.randomUUID, Course.randomUUID, degree.id, plan, Labwork.randomUUID)
      val second = Labwork("label 2", "description 2", Semester.randomUUID, Course.randomUUID, Degree.randomUUID, plan, Labwork.randomUUID)
      val third = Labwork("label 3", "description 3", Semester.randomUUID, Course.randomUUID, Degree.randomUUID, plan, Labwork.randomUUID)
      val fourth = Labwork("label 4", "description 4", Semester.randomUUID, Course.randomUUID, degree.id, plan, Labwork.randomUUID)

      val labworks = Set(first, second, third, fourth)

      when(repository.get[Labwork](anyObject(), anyObject())).thenReturn(Success(labworks))

      val request = FakeRequest(
        GET,
        s"/${entityTypeName.toLowerCase}s?${LabworkCRUDController.degreeAttribute}=${degree.id.toString}"
      )

      val result = controller.asInstanceOf[LabworkCRUDController].all()(request)

      status(result) shouldBe OK
      contentType(result) shouldBe Some[String](mimeType)
      contentAsJson(result) shouldBe Json.toJson(Set(first, fourth))
    }

    "not return labworks for a degree when there is no match" in {
      val degree = Degree("label", "description", Degree.randomUUID)

      val plan = AssignmentPlan(1, Set(AssignmentEntry(1, Set(EntryType("type")))))
      val first = Labwork("label 1", "description 1", Semester.randomUUID, Course.randomUUID, Degree.randomUUID, plan, Labwork.randomUUID)
      val second = Labwork("label 2", "description 2", Semester.randomUUID, Course.randomUUID, Degree.randomUUID, plan, Labwork.randomUUID)
      val third = Labwork("label 3", "description 3", Semester.randomUUID, Course.randomUUID, Degree.randomUUID, plan, Labwork.randomUUID)
      val fourth = Labwork("label 4", "description 4", Semester.randomUUID, Course.randomUUID, Degree.randomUUID, plan, Labwork.randomUUID)

      val labworks = Set(first, second, third, fourth)

      when(repository.get[Labwork](anyObject(), anyObject())).thenReturn(Success(labworks))

      val request = FakeRequest(
        GET,
        s"/${entityTypeName.toLowerCase}s?${LabworkCRUDController.degreeAttribute}=${degree.id.toString}"
      )

      val result = controller.asInstanceOf[LabworkCRUDController].all()(request)

      status(result) shouldBe OK
      contentType(result) shouldBe Some[String](mimeType)
      contentAsJson(result) shouldBe Json.toJson(Set.empty[Labwork])
    }

    "return the corresponding labwork for a given semester" in {
      val semester = Semester("label", "abbrev", LocalDate.now, LocalDate.now, LocalDate.now, Semester.randomUUID)
      val plan = AssignmentPlan(1, Set(AssignmentEntry(1, Set(EntryType("type")))))
      val first = Labwork("label 1", "description 1", Semester.randomUUID, Course.randomUUID, Degree.randomUUID, plan, Labwork.randomUUID)
      val second = Labwork("label 2", "description 2", Semester.randomUUID, Course.randomUUID, Degree.randomUUID, plan, Labwork.randomUUID)
      val third = Labwork("label 3", "description 3", Semester.randomUUID, Course.randomUUID, Degree.randomUUID, plan, Labwork.randomUUID)
      val fourth = Labwork("label 4", "description 4", semester.id, Course.randomUUID, Degree.randomUUID, plan, Labwork.randomUUID)

      val labworks = Set(first, second, third, fourth)

      when(repository.get[Labwork](anyObject(), anyObject())).thenReturn(Success(labworks))

      val request = FakeRequest(
        GET,
        s"/${entityTypeName.toLowerCase}s?${LabworkCRUDController.semesterAttribute}=${semester.id.toString}"
      )

      val result = controller.asInstanceOf[LabworkCRUDController].all()(request)

      status(result) shouldBe OK
      contentType(result) shouldBe Some[String](mimeType)
      contentAsJson(result) shouldBe Json.toJson(Set(fourth))
    }

    "return all corresponding labworks for a given semester" in {
      val semester = Semester("label", "abbrev", LocalDate.now, LocalDate.now, LocalDate.now, Semester.randomUUID)
      val plan = AssignmentPlan(1, Set(AssignmentEntry(1, Set(EntryType("type")))))
      val first = Labwork("label 1", "description 1", Semester.randomUUID, Course.randomUUID, Degree.randomUUID, plan, Labwork.randomUUID)
      val second = Labwork("label 2", "description 2", Semester.randomUUID, Course.randomUUID, Degree.randomUUID, plan, Labwork.randomUUID)
      val third = Labwork("label 3", "description 3", semester.id, Course.randomUUID, Degree.randomUUID, plan, Labwork.randomUUID)
      val fourth = Labwork("label 4", "description 4", semester.id, Course.randomUUID, Degree.randomUUID, plan, Labwork.randomUUID)

      val labworks = Set(first, second, third, fourth)

      when(repository.get[Labwork](anyObject(), anyObject())).thenReturn(Success(labworks))

      val request = FakeRequest(
        GET,
        s"/${entityTypeName.toLowerCase}s?${LabworkCRUDController.semesterAttribute}=${semester.id.toString}"
      )

      val result = controller.asInstanceOf[LabworkCRUDController].all()(request)

      status(result) shouldBe OK
      contentType(result) shouldBe Some[String](mimeType)
      contentAsJson(result) shouldBe Json.toJson(Set(third, fourth))
    }

    "not return labworks for a semester when there is no match" in {
      val semester = Semester("label", "abbrev", LocalDate.now, LocalDate.now, LocalDate.now, Semester.randomUUID)
      val plan = AssignmentPlan(1, Set(AssignmentEntry(1, Set(EntryType("type")))))
      val first = Labwork("label 1", "description 1", Semester.randomUUID, Course.randomUUID, Degree.randomUUID, plan, Labwork.randomUUID)
      val second = Labwork("label 2", "description 2", Semester.randomUUID, Course.randomUUID, Degree.randomUUID, plan, Labwork.randomUUID)
      val third = Labwork("label 3", "description 3", Semester.randomUUID, Course.randomUUID, Degree.randomUUID, plan, Labwork.randomUUID)
      val fourth = Labwork("label 4", "description 4", Semester.randomUUID, Course.randomUUID, Degree.randomUUID, plan, Labwork.randomUUID)

      val labworks = Set(first, second, third, fourth)

      when(repository.get[Labwork](anyObject(), anyObject())).thenReturn(Success(labworks))

      val request = FakeRequest(
        GET,
        s"/${entityTypeName.toLowerCase}s?${LabworkCRUDController.semesterAttribute}=${semester.id.toString}"
      )

      val result = controller.asInstanceOf[LabworkCRUDController].all()(request)

      status(result) shouldBe OK
      contentType(result) shouldBe Some[String](mimeType)
      contentAsJson(result) shouldBe Json.toJson(Set.empty[Labwork])
    }

    "return all corresponding labworks for a given course and degree" in {
      val course = Course("label", "desc", "abbrev", Employee.randomUUID, 1, Course.randomUUID)
      val degree = Degree("label", "abbrev", Degree.randomUUID)
      val plan = AssignmentPlan(1, Set(AssignmentEntry(1, Set(EntryType("type")))))
      val first = Labwork("label 1", "description 1", Semester.randomUUID, course.id, degree.id, plan, Labwork.randomUUID)
      val second = Labwork("label 2", "description 2", Semester.randomUUID, course.id, Degree.randomUUID, plan, Labwork.randomUUID)
      val third = Labwork("label 3", "description 3", Semester.randomUUID, course.id, degree.id, plan, Labwork.randomUUID)
      val fourth = Labwork("label 4", "description 4", Semester.randomUUID, Course.randomUUID, degree.id, plan, Labwork.randomUUID)

      val labworks = Set(first, second, third, fourth)

      when(repository.get[Labwork](anyObject(), anyObject())).thenReturn(Success(labworks))

      val request = FakeRequest(
        GET,
        s"/${entityTypeName.toLowerCase}s?${LabworkCRUDController.courseAttribute}=${course.id.toString}&${LabworkCRUDController.degreeAttribute}=${degree.id.toString}"
      )

      val result = controller.asInstanceOf[LabworkCRUDController].all()(request)

      status(result) shouldBe OK
      contentType(result) shouldBe Some[String](mimeType)
      contentAsJson(result) shouldBe Json.toJson(Set(first, third))
    }

    "return all corresponding labworks for a given course and semester" in {
      val course = Course("label", "desc", "abbrev", Employee.randomUUID, 1, Course.randomUUID)
      val semester = Semester("label", "abbrev", LocalDate.now, LocalDate.now, LocalDate.now, Semester.randomUUID)
      val plan = AssignmentPlan(1, Set(AssignmentEntry(1, Set(EntryType("type")))))
      val first = Labwork("label 1", "description 1", Semester.randomUUID, course.id, Degree.randomUUID, plan, Labwork.randomUUID)
      val second = Labwork("label 2", "description 2", semester.id, course.id, Degree.randomUUID, plan, Labwork.randomUUID)
      val third = Labwork("label 3", "description 3", semester.id, course.id, Degree.randomUUID, plan, Labwork.randomUUID)
      val fourth = Labwork("label 4", "description 4", semester.id, Course.randomUUID, Degree.randomUUID, plan, Labwork.randomUUID)

      val labworks = Set(first, second, third, fourth)

      when(repository.get[Labwork](anyObject(), anyObject())).thenReturn(Success(labworks))

      val request = FakeRequest(
        GET,
        s"/${entityTypeName.toLowerCase}s?${LabworkCRUDController.courseAttribute}=${course.id.toString}&${LabworkCRUDController.semesterAttribute}=${semester.id.toString}"
      )

      val result = controller.asInstanceOf[LabworkCRUDController].all()(request)

      status(result) shouldBe OK
      contentType(result) shouldBe Some[String](mimeType)
      contentAsJson(result) shouldBe Json.toJson(Set(second, third))
    }

    "return all corresponding labworks for a given degree and semester" in {
      val degree = Degree("label", "abbrev", Degree.randomUUID)
      val semester = Semester("label", "abbrev", LocalDate.now, LocalDate.now, LocalDate.now, Semester.randomUUID)
      val plan = AssignmentPlan(1, Set(AssignmentEntry(1, Set(EntryType("type")))))
      val first = Labwork("label 1", "description 1", semester.id, Course.randomUUID, degree.id, plan, Labwork.randomUUID)
      val second = Labwork("label 2", "description 2", semester.id, Course.randomUUID, degree.id, plan, Labwork.randomUUID)
      val third = Labwork("label 3", "description 3", semester.id, Course.randomUUID, Degree.randomUUID, plan, Labwork.randomUUID)
      val fourth = Labwork("label 4", "description 4", semester.id, Course.randomUUID, degree.id, plan, Labwork.randomUUID)

      val labworks = Set(first, second, third, fourth)

      when(repository.get[Labwork](anyObject(), anyObject())).thenReturn(Success(labworks))

      val request = FakeRequest(
        GET,
        s"/${entityTypeName.toLowerCase}s?${LabworkCRUDController.degreeAttribute}=${degree.id.toString}&${LabworkCRUDController.semesterAttribute}=${semester.id.toString}"
      )

      val result = controller.asInstanceOf[LabworkCRUDController].all()(request)

      status(result) shouldBe OK
      contentType(result) shouldBe Some[String](mimeType)
      contentAsJson(result) shouldBe Json.toJson(Set(first, second, fourth))
    }

    "return all corresponding labworks for a given degree, course and semester" in {
      val course = Course("label", "desc", "abbrev", Employee.randomUUID, 1, Course.randomUUID)
      val degree = Degree("label", "abbrev", Degree.randomUUID)
      val semester = Semester("label", "abbrev", LocalDate.now, LocalDate.now, LocalDate.now, Semester.randomUUID)
      val plan = AssignmentPlan(1, Set(AssignmentEntry(1, Set(EntryType("type")))))
      val first = Labwork("label 1", "description 1", semester.id, course.id, degree.id, plan, Labwork.randomUUID)
      val second = Labwork("label 2", "description 2", semester.id, Course.randomUUID, degree.id, plan, Labwork.randomUUID)
      val third = Labwork("label 3", "description 3", semester.id, course.id, Degree.randomUUID, plan, Labwork.randomUUID)
      val fourth = Labwork("label 4", "description 4", semester.id, course.id, degree.id, plan, Labwork.randomUUID)

      val labworks = Set(first, second, third, fourth)

      when(repository.get[Labwork](anyObject(), anyObject())).thenReturn(Success(labworks))

      val request = FakeRequest(
        GET,
        s"/${entityTypeName.toLowerCase}s?${LabworkCRUDController.degreeAttribute}=${degree.id.toString}&${LabworkCRUDController.semesterAttribute}=${semester.id.toString}&${LabworkCRUDController.courseAttribute}=${course.id.toString}"
      )

      val result = controller.asInstanceOf[LabworkCRUDController].all()(request)

      status(result) shouldBe OK
      contentType(result) shouldBe Some[String](mimeType)
      contentAsJson(result) shouldBe Json.toJson(Set(first, fourth))
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

    s"neither create n'or update an existing $entityTypeName when resource does not exists although body would lead to duplication" in {
      when(repository.get[Labwork](anyObject())(anyObject())).thenReturn(Success(None))
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

    s"successfully get a single $entityTypeName atomized" in {
      import Labwork.atomicWrites

      doReturn(Success(Some(entityToPass))).
      doReturn(Success(Some(semesterToPass))).
      doReturn(Success(Some(courseToPass))).
      doReturn(Success(Some(degreeToPass))).
      when(repository).get(anyObject())(anyObject())

      val request = FakeRequest(
        GET,
        s"/${entityTypeName}s/${entityToPass.id}"
      )
      val result = controller.getAtomic(entityToPass.id.toString)(request)

      status(result) shouldBe OK
      contentType(result) shouldBe Some[String](mimeType)
      contentAsJson(result) shouldBe Json.toJson(atomizedEntityToPass)
    }

    s"not get a single $entityTypeName atomized when one of the atomic models is not found" in {
      doReturn(Success(Some(entityToPass))).
      doReturn(Success(None)).
      doReturn(Success(Some(courseToPass))).
      doReturn(Success(Some(degreeToPass))).
      when(repository).get(anyObject())(anyObject())

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
      doReturn(Failure(new Exception(errorMessage))).
      when(repository).get(anyObject())(anyObject())

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
      import Labwork.atomicWrites

      val labworks = Set(entityToPass, entityToFail)
      val semesters = Set(semesterToPass, semesterToFail)
      val courses = Set(courseToPass, courseToFail)
      val degrees = Set(degreeToPass, degreeToFail)

      when(repository.get[Labwork](anyObject(), anyObject())).thenReturn(Success(labworks))
      doReturn(Success(semesters)).
      doReturn(Success(courses)).
      doReturn(Success(degrees)).
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
      val labworks = Set(entityToPass, entityToFail)
      val errorMessage = s"Oops, cant get the desired $entityTypeName for some reason"

      when(repository.get[Labwork](anyObject(), anyObject())).thenReturn(Success(labworks))
      doReturn(Failure(new Exception(errorMessage))).
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
}
