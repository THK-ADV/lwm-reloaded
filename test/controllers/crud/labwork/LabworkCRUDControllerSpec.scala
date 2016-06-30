package controllers.crud.labwork

import java.util.UUID

import controllers.crud.AbstractCRUDControllerSpec
import models._
import models.labwork.{Labwork, LabworkAtom, LabworkProtocol}
import models.semester.Semester
import models.users.{Employee, User}
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

class LabworkCRUDControllerSpec extends AbstractCRUDControllerSpec[LabworkProtocol, Labwork, LabworkAtom] {

  val semesterToPass = Semester("label to pass", "abbrev to pass", LocalDate.now, LocalDate.now, LocalDate.now, Semester.randomUUID)
  val semesterToFail = Semester("label to pass", "abbrev to pass", LocalDate.now, LocalDate.now, LocalDate.now, Semester.randomUUID)

  val employeeToPass = Employee("systemId to pass", "last name to pass", "first name to pass", "email to pass", "employee")
  val employeeToFail = Employee("systemId to fail", "last name to fail", "first name to fail", "email to fail", "employee")

  val courseToPass = Course("label to pass", "desc to pass", "abbrev to pass", employeeToPass.id, 1, Course.randomUUID)
  val courseToFail = Course("label to fail", "desc to fail", "abbrev to fail", employeeToFail.id, 1, Course.randomUUID)

  def toAtom(course: Course): CourseAtom = {
    def employee(id: UUID): Employee = if (id == employeeToPass.id) employeeToPass else employeeToFail
    CourseAtom(course.label, course.description, course.abbreviation, employee(course.lecturer), course.semesterIndex, course.id)
  }

  val degreeToPass = Degree("label to pass", "abbrev to pass", Degree.randomUUID)
  val degreeToFail = Degree("label to fail", "abbrev to fail", Degree.randomUUID)

  override val entityToPass: Labwork = Labwork(
    "label to pass",
    "description to pass",
    semesterToPass.id,
    courseToPass.id,
    degreeToPass.id
  )

  override def entityTypeName: String = "labwork"

  override val controller: LabworkCRUDController = new LabworkCRUDController(repository, sessionService, namespace, roleService) {

    override protected def fromInput(input: LabworkProtocol, existing: Option[Labwork]): Labwork = entityToPass

    override protected def contextFrom: PartialFunction[Rule, SecureContext] = {
      case _ => NonSecureBlock
    }
  }

  override val entityToFail: Labwork = Labwork(
    "label to fail",
    "description to fail",
    semesterToFail.id,
    courseToFail.id,
    degreeToFail.id
  )

  override implicit val jsonWrites: Writes[Labwork] = Labwork.writes

  override implicit def jsonWritesAtom: Writes[LabworkAtom] = Labwork.writesAtom

  override val mimeType: LwmMimeType = LwmMimeType.labworkV1Json

  override val inputJson: JsValue = Json.obj(
    "label" -> entityToPass.label,
    "description" -> entityToPass.description,
    "semester" -> entityToPass.semester,
    "course" -> entityToPass.course,
    "degree" -> entityToPass.degree,
    "subscribable" -> entityToPass.subscribable,
    "published" -> entityToPass.published
  )

  override val updateJson: JsValue = Json.obj(
    "label" -> entityToPass.label,
    "description" -> (entityToPass.description + "updated"),
    "semester" -> UUID.randomUUID(),
    "course" -> entityToPass.course,
    "degree" -> entityToPass.degree,
    "subscribable" -> !entityToPass.subscribable,
    "published" -> entityToPass.published

  )

  override val atomizedEntityToPass = LabworkAtom(
    entityToPass.label,
    entityToPass.description,
    semesterToPass,
    toAtom(courseToPass),
    degreeToPass,
    entityToPass.subscribable,
    entityToPass.published,
    entityToPass.id
  )

  override val atomizedEntityToFail = LabworkAtom(
    entityToFail.label,
    entityToFail.description,
    semesterToFail,
    toAtom(courseToFail),
    degreeToFail,
    entityToFail.subscribable,
    entityToFail.published,
    entityToFail.id
  )

  import bindings.LabworkDescriptor
  import ops._

  implicit val labworkBinder = LabworkDescriptor.binder

  override def pointedGraph: PointedGraph[Sesame] = entityToPass.toPG

  "A LabworkCRUDControllerSpec also " should {

    "return the corresponding labwork for a given course" in {
      val course = Course("label", "desc", "abbrev", User.randomUUID, 1, Course.randomUUID)
      val first = Labwork("label 1", "description 1", Semester.randomUUID, Course.randomUUID, Degree.randomUUID)
      val second = Labwork("label 2", "description 2", Semester.randomUUID, course.id, Degree.randomUUID)
      val third = Labwork("label 3", "description 3", Semester.randomUUID, Course.randomUUID, Degree.randomUUID)
      val fourth = Labwork("label 4", "description 4", Semester.randomUUID, Course.randomUUID, Degree.randomUUID)

      val labworks = Set(first, second, third, fourth)

      when(repository.getAll[Labwork](anyObject())).thenReturn(Success(labworks))

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
      val course = Course("label", "desc", "abbrev", User.randomUUID, 1, Course.randomUUID)
      val first = Labwork("label 1", "description 1", Semester.randomUUID, Course.randomUUID, Degree.randomUUID)
      val second = Labwork("label 2", "description 2", Semester.randomUUID, course.id, Degree.randomUUID)
      val third = Labwork("label 3", "description 3", Semester.randomUUID, Course.randomUUID, Degree.randomUUID)
      val fourth = Labwork("label 4", "description 4", Semester.randomUUID, course.id, Degree.randomUUID)

      val labworks = Set(first, second, third, fourth)

      when(repository.getAll[Labwork](anyObject())).thenReturn(Success(labworks))

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
      val course = Course("label", "desc", "abbrev", User.randomUUID, 1, Course.randomUUID)
      val first = Labwork("label 1", "description 1", Semester.randomUUID, Course.randomUUID, Degree.randomUUID)
      val second = Labwork("label 2", "description 2", Semester.randomUUID, Course.randomUUID, Degree.randomUUID)
      val third = Labwork("label 3", "description 3", Semester.randomUUID, Course.randomUUID, Degree.randomUUID)
      val fourth = Labwork("label 4", "description 4", Semester.randomUUID, Course.randomUUID, Degree.randomUUID)

      val labworks = Set(first, second, third, fourth)

      when(repository.getAll[Labwork](anyObject())).thenReturn(Success(labworks))

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
      val course = Course("label", "desc", "abbrev", User.randomUUID, 1, Course.randomUUID)
      val first = Labwork("label 1", "description 1", Semester.randomUUID, Course.randomUUID, Degree.randomUUID)
      val second = Labwork("label 2", "description 2", Semester.randomUUID, Course.randomUUID, Degree.randomUUID)
      val third = Labwork("label 3", "description 3", Semester.randomUUID, Course.randomUUID, Degree.randomUUID)
      val fourth = Labwork("label 4", "description 4", Semester.randomUUID, Course.randomUUID, Degree.randomUUID)

      val labworks = Set(first, second, third, fourth)

      when(repository.getAll[Labwork](anyObject())).thenReturn(Success(labworks))

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

      val first = Labwork("label 1", "description 1", Semester.randomUUID, Course.randomUUID, Degree.randomUUID)
      val second = Labwork("label 2", "description 2", Semester.randomUUID, Course.randomUUID, Degree.randomUUID)
      val third = Labwork("label 3", "description 3", Semester.randomUUID, Course.randomUUID, Degree.randomUUID)
      val fourth = Labwork("label 4", "description 4", Semester.randomUUID, Course.randomUUID, Degree.randomUUID)

      val labworks = Set(first, second, third, fourth)

      when(repository.getAll[Labwork](anyObject())).thenReturn(Success(labworks))

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
      val first = Labwork("label 1", "description 1", Semester.randomUUID, Course.randomUUID, degree.id)
      val second = Labwork("label 2", "description 2", Semester.randomUUID, Course.randomUUID, Degree.randomUUID)
      val third = Labwork("label 3", "description 3", Semester.randomUUID, Course.randomUUID, Degree.randomUUID)
      val fourth = Labwork("label 4", "description 4", Semester.randomUUID, Course.randomUUID, Degree.randomUUID)

      val labworks = Set(first, second, third, fourth)

      when(repository.getAll[Labwork](anyObject())).thenReturn(Success(labworks))

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
      val first = Labwork("label 1", "description 1", Semester.randomUUID, Course.randomUUID, degree.id)
      val second = Labwork("label 2", "description 2", Semester.randomUUID, Course.randomUUID, Degree.randomUUID)
      val third = Labwork("label 3", "description 3", Semester.randomUUID, Course.randomUUID, Degree.randomUUID)
      val fourth = Labwork("label 4", "description 4", Semester.randomUUID, Course.randomUUID, degree.id)

      val labworks = Set(first, second, third, fourth)

      when(repository.getAll[Labwork](anyObject())).thenReturn(Success(labworks))

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
      val first = Labwork("label 1", "description 1", Semester.randomUUID, Course.randomUUID, Degree.randomUUID)
      val second = Labwork("label 2", "description 2", Semester.randomUUID, Course.randomUUID, Degree.randomUUID)
      val third = Labwork("label 3", "description 3", Semester.randomUUID, Course.randomUUID, Degree.randomUUID)
      val fourth = Labwork("label 4", "description 4", Semester.randomUUID, Course.randomUUID, Degree.randomUUID)

      val labworks = Set(first, second, third, fourth)

      when(repository.getAll[Labwork](anyObject())).thenReturn(Success(labworks))

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
      val first = Labwork("label 1", "description 1", Semester.randomUUID, Course.randomUUID, Degree.randomUUID)
      val second = Labwork("label 2", "description 2", Semester.randomUUID, Course.randomUUID, Degree.randomUUID)
      val third = Labwork("label 3", "description 3", Semester.randomUUID, Course.randomUUID, Degree.randomUUID)
      val fourth = Labwork("label 4", "description 4", semester.id, Course.randomUUID, Degree.randomUUID)

      val labworks = Set(first, second, third, fourth)

      when(repository.getAll[Labwork](anyObject())).thenReturn(Success(labworks))

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
      val first = Labwork("label 1", "description 1", Semester.randomUUID, Course.randomUUID, Degree.randomUUID)
      val second = Labwork("label 2", "description 2", Semester.randomUUID, Course.randomUUID, Degree.randomUUID)
      val third = Labwork("label 3", "description 3", semester.id, Course.randomUUID, Degree.randomUUID)
      val fourth = Labwork("label 4", "description 4", semester.id, Course.randomUUID, Degree.randomUUID)

      val labworks = Set(first, second, third, fourth)

      when(repository.getAll[Labwork](anyObject())).thenReturn(Success(labworks))

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
      val first = Labwork("label 1", "description 1", Semester.randomUUID, Course.randomUUID, Degree.randomUUID)
      val second = Labwork("label 2", "description 2", Semester.randomUUID, Course.randomUUID, Degree.randomUUID)
      val third = Labwork("label 3", "description 3", Semester.randomUUID, Course.randomUUID, Degree.randomUUID)
      val fourth = Labwork("label 4", "description 4", Semester.randomUUID, Course.randomUUID, Degree.randomUUID)

      val labworks = Set(first, second, third, fourth)

      when(repository.getAll[Labwork](anyObject())).thenReturn(Success(labworks))

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
      val course = Course("label", "desc", "abbrev", User.randomUUID, 1, Course.randomUUID)
      val degree = Degree("label", "abbrev", Degree.randomUUID)
      val first = Labwork("label 1", "description 1", Semester.randomUUID, course.id, degree.id)
      val second = Labwork("label 2", "description 2", Semester.randomUUID, course.id, Degree.randomUUID)
      val third = Labwork("label 3", "description 3", Semester.randomUUID, course.id, degree.id)
      val fourth = Labwork("label 4", "description 4", Semester.randomUUID, Course.randomUUID, degree.id)

      val labworks = Set(first, second, third, fourth)

      when(repository.getAll[Labwork](anyObject())).thenReturn(Success(labworks))

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
      val course = Course("label", "desc", "abbrev", User.randomUUID, 1, Course.randomUUID)
      val semester = Semester("label", "abbrev", LocalDate.now, LocalDate.now, LocalDate.now, Semester.randomUUID)
      val first = Labwork("label 1", "description 1", Semester.randomUUID, course.id, Degree.randomUUID)
      val second = Labwork("label 2", "description 2", semester.id, course.id, Degree.randomUUID)
      val third = Labwork("label 3", "description 3", semester.id, course.id, Degree.randomUUID)
      val fourth = Labwork("label 4", "description 4", semester.id, Course.randomUUID, Degree.randomUUID)

      val labworks = Set(first, second, third, fourth)

      when(repository.getAll[Labwork](anyObject())).thenReturn(Success(labworks))

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
      val first = Labwork("label 1", "description 1", semester.id, Course.randomUUID, degree.id)
      val second = Labwork("label 2", "description 2", semester.id, Course.randomUUID, degree.id)
      val third = Labwork("label 3", "description 3", semester.id, Course.randomUUID, Degree.randomUUID)
      val fourth = Labwork("label 4", "description 4", semester.id, Course.randomUUID, degree.id)

      val labworks = Set(first, second, third, fourth)

      when(repository.getAll[Labwork](anyObject())).thenReturn(Success(labworks))

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
      val course = Course("label", "desc", "abbrev", User.randomUUID, 1, Course.randomUUID)
      val degree = Degree("label", "abbrev", Degree.randomUUID)
      val semester = Semester("label", "abbrev", LocalDate.now, LocalDate.now, LocalDate.now, Semester.randomUUID)
      val first = Labwork("label 1", "description 1", semester.id, course.id, degree.id)
      val second = Labwork("label 2", "description 2", semester.id, Course.randomUUID, degree.id)
      val third = Labwork("label 3", "description 3", semester.id, course.id, Degree.randomUUID)
      val fourth = Labwork("label 4", "description 4", semester.id, course.id, degree.id)

      val labworks = Set(first, second, third, fourth)

      when(repository.getAll[Labwork](anyObject())).thenReturn(Success(labworks))

      val request = FakeRequest(
        GET,
        s"/${entityTypeName.toLowerCase}s?${LabworkCRUDController.degreeAttribute}=${degree.id.toString}&${LabworkCRUDController.semesterAttribute}=${semester.id.toString}&${LabworkCRUDController.courseAttribute}=${course.id.toString}"
      )

      val result = controller.asInstanceOf[LabworkCRUDController].all()(request)

      status(result) shouldBe OK
      contentType(result) shouldBe Some[String](mimeType)
      contentAsJson(result) shouldBe Json.toJson(Set(first, fourth))
    }

    "return all corresponding labworks where a given student can apply for" in {
      val degree = Degree("label", "abbrev", Degree.randomUUID)
      val first = Labwork("label 1", "description 1", Semester.randomUUID, Course.randomUUID, Degree.randomUUID)
      val second = Labwork("label 2", "description 2", Semester.randomUUID, Course.randomUUID, Degree.randomUUID, subscribable = true)
      val third = Labwork("label 3", "description 3", Semester.randomUUID, Course.randomUUID, degree.id, subscribable = true)
      val fourth = Labwork("label 4", "description 4", Semester.randomUUID, Course.randomUUID, Degree.randomUUID)

      when(repository.getAll[Labwork](anyObject())).thenReturn(Success(Set(first, second, third, fourth)))

      val request = FakeRequest(
        GET,
        s"/${entityTypeName.toLowerCase}/degrees/${degree.id}"
      )

      val result = controller.asInstanceOf[LabworkCRUDController].allWithDegree(degree.id.toString)(request)

      status(result) shouldBe OK
      contentType(result) shouldBe Some[String](mimeType)
      contentAsJson(result) shouldBe Json.toJson(Set(third))
    }

    "return no labworks when there is nothing to apply for" in {
      val degree = Degree("label", "abbrev", Degree.randomUUID)
      val first = Labwork("label 1", "description 1", Semester.randomUUID, Course.randomUUID, Degree.randomUUID)
      val second = Labwork("label 2", "description 2", Semester.randomUUID, Course.randomUUID, Degree.randomUUID)
      val third = Labwork("label 3", "description 3", Semester.randomUUID, Course.randomUUID, degree.id)
      val fourth = Labwork("label 4", "description 4", Semester.randomUUID, Course.randomUUID, Degree.randomUUID)

      when(repository.getAll[Labwork](anyObject())).thenReturn(Success(Set(first, second, third, fourth)))

      val request = FakeRequest(
        GET,
        s"/${entityTypeName.toLowerCase}/degrees/${degree.id}"
      )

      val result = controller.asInstanceOf[LabworkCRUDController].allWithDegree(degree.id.toString)(request)

      status(result) shouldBe OK
      contentType(result) shouldBe Some[String](mimeType)
      contentAsJson(result) shouldBe Json.toJson(Set.empty[Labwork])
    }

    "return all corresponding labworks which schedules are published" in {
      val first = Labwork("label 1", "description 1", Semester.randomUUID, Course.randomUUID, Degree.randomUUID)
      val second = Labwork("label 2", "description 2", Semester.randomUUID, Course.randomUUID, Degree.randomUUID, subscribable = false, published = true)
      val third = Labwork("label 3", "description 3", Semester.randomUUID, Course.randomUUID, Degree.randomUUID, subscribable = false, published = true)
      val fourth = Labwork("label 4", "description 4", Semester.randomUUID, Course.randomUUID, Degree.randomUUID)
      val fifth = Labwork("label 5", "description 5", Semester.randomUUID, Course.randomUUID, Degree.randomUUID, subscribable = false, published = true)
      val sixth = Labwork("label 6", "description 6", Semester.randomUUID, Course.randomUUID, Degree.randomUUID)

      when(repository.getAll[Labwork](anyObject())).thenReturn(Success(Set(
        first, second, third, fourth, fifth, sixth
      )))

      val request = FakeRequest(
        GET,
        s"/${entityTypeName.toLowerCase}?${LabworkCRUDController.publishedAttribute}=true"
      )

      val result = controller.asInstanceOf[LabworkCRUDController].all()(request)

      status(result) shouldBe OK
      contentType(result) shouldBe Some[String](mimeType)
      Set(second, third, fifth).forall { l =>
        contentAsString(result) contains Json.toJson(l).toString
      } shouldBe true
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
  }
}
