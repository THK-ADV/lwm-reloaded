package controllers

import java.util.UUID

import base.StreamHandler._
import models._
import org.mockito.Matchers._
import org.mockito.Mockito._
import org.openrdf.model.Value
import org.w3.banana.PointedGraph
import org.w3.banana.sesame.Sesame
import play.api.http.HeaderNames
import play.api.libs.json.{JsValue, Json, Writes}
import play.api.test.Helpers._
import play.api.test.{FakeHeaders, FakeRequest}
import utils.LwmMimeType

import scala.util.Success

class CourseCRUDControllerSpec extends AbstractCRUDControllerSpec[CourseProtocol, Course, CourseAtom] {

  val lecturerToPass = SesameEmployee("systemId to pass", "last name to pass", "first name to pass", "email to pass", "status to pass")
  val lecturerToFail = SesameEmployee("systemId to fail", "last name to fail", "first name to fail", "email to fail", "status to fail")

  override val entityToPass: Course = Course("label to pass", "description to pass", "abbreviation to pass", lecturerToPass.id, 1)

  override val controller: CourseCRUDController = new CourseCRUDController(repository, sessionService, namespace, roleService) {

    override protected def fromInput(input: CourseProtocol, existing: Option[Course]): Course = entityToPass

    override protected def contextFrom: PartialFunction[Rule, SecureContext] = {
      case _ => NonSecureBlock
    }
  }

  override val entityToFail: Course = Course("label to fail", "description to fail", "abbreviation to fail", lecturerToFail.id, 1)

  override val atomizedEntityToPass = CourseAtom(
    entityToPass.label,
    entityToPass.description,
    entityToPass.abbreviation,
    lecturerToPass,
    entityToPass.semesterIndex,
    entityToPass.invalidated,
    entityToPass.id
  )

  override val atomizedEntityToFail = CourseAtom(
    entityToFail.label,
    entityToFail.description,
    entityToFail.abbreviation,
    lecturerToFail,
    entityToFail.semesterIndex,
    entityToPass.invalidated,
    entityToFail.id
  )

  override implicit val jsonWrites: Writes[Course] = Course.writes

  override implicit val jsonWritesAtom: Writes[CourseAtom] = Course.writesAtom

  override val mimeType: LwmMimeType = LwmMimeType.courseV1Json

  override val inputJson: JsValue = Json.obj(
    "label" -> entityToPass.label,
    "description" -> entityToPass.description,
    "abbreviation" -> entityToPass.abbreviation,
    "lecturer" -> entityToPass.lecturer,
    "semesterIndex" -> entityToPass.semesterIndex
  )

  override def entityTypeName: String = "course"

  import bindings.CourseDescriptor
  import ops._

  implicit val courseBinder = CourseDescriptor.binder

  override def pointedGraph: PointedGraph[Sesame] = entityToPass.toPG

  override val updateJson: JsValue = Json.obj(
    "label" -> entityToPass.label,
    "description" -> (entityToPass.description + " updated"),
    "abbreviation" -> (entityToPass.abbreviation + " updated"),
    "lecturer" -> entityToPass.lecturer,
    "semesterIndex" -> entityToPass.semesterIndex
  )


  "A CourseCRUDControllerSpec also " should {
    "return the corresponding course for a given lecturer" in {
      val lecturer = SesameEmployee("systemId", "last name", "first name", "email", "status")

      val first = Course("label1", "desc1", "abbreviation1", User.randomUUID, 1)
      val second = Course("label2", "desc2", "abbreviation2", lecturer.id, 1)
      val third = Course("label3", "desc3", "abbreviation3", User.randomUUID, 1)
      val fourth = Course("label4", "desc4", "abbreviation4", User.randomUUID, 1)

      val courses = Set(first, second, third, fourth)

      when(repository.getAll[Course](anyObject())).thenReturn(Success(courses))

      val request = FakeRequest(
        GET,
        s"/${entityTypeName.toLowerCase}s?${CourseCRUDController.lecturerAttribute}=${lecturer.id.toString}"
      )

      val result = controller.asInstanceOf[CourseCRUDController].all()(request)
      val expected = Set(Json.toJson(second))

      status(result) shouldBe OK
      contentType(result) shouldBe Some(mimeType.value)
      contentFromStream(result) shouldBe expected
    }

    "return all corresponding courses for a given lecturer" in {
      val lecturer = SesameEmployee("systemId", "last name", "first name", "email", "status")

      val first = Course("label1", "desc1", "abbreviation1", lecturer.id, 1)
      val second = Course("label2", "desc2", "abbreviation2", User.randomUUID, 1)
      val third = Course("label3", "desc3", "abbreviation3", lecturer.id, 1)
      val fourth = Course("label4", "desc4", "abbreviation4", User.randomUUID, 1)

      val courses = Set(first, second, third, fourth)

      when(repository.getAll[Course](anyObject())).thenReturn(Success(courses))

      val request = FakeRequest(
        GET,
        s"/${entityTypeName.toLowerCase}s?${CourseCRUDController.lecturerAttribute}=${lecturer.id.toString}"
      )

      val result = controller.all()(request)
      val expected = Set(Json.toJson(first), Json.toJson(third))

      status(result) shouldBe OK
      contentType(result) shouldBe Some(mimeType.value)
      contentFromStream(result) shouldBe expected
    }

    "not return courses for a lecturer when there is no match" in {
      val lecturer = SesameEmployee("systemId", "last name", "first name", "email", "status")

      val first = Course("label1", "desc1", "abbreviation1", User.randomUUID, 1)
      val second = Course("label2", "desc2", "abbreviation2", User.randomUUID, 1)
      val third = Course("label3", "desc3", "abbreviation3", User.randomUUID, 1)
      val fourth = Course("label4", "desc4", "abbreviation4", User.randomUUID, 1)

      val courses = Set(first, second, third, fourth)

      when(repository.getAll[Course](anyObject())).thenReturn(Success(courses))

      val request = FakeRequest(
        GET,
        s"/${entityTypeName.toLowerCase}s?${CourseCRUDController.lecturerAttribute}=${lecturer.id.toString}"
      )

      val result = controller.all()(request)

      status(result) shouldBe OK
      contentType(result) shouldBe Some(mimeType.value)
      contentFromStream(result) shouldBe emptyJson
    }

    "not return courses when there is an invalid query attribute" in {
      val first = Course("label1", "desc1", "abbreviation1", User.randomUUID, 1)
      val second = Course("label2", "desc2", "abbreviation2", User.randomUUID, 1)
      val third = Course("label3", "desc3", "abbreviation3", User.randomUUID, 1)
      val fourth = Course("label4", "desc4", "abbreviation4", User.randomUUID, 1)

      val courses = Set(first, second, third, fourth)

      when(repository.getAll[Course](anyObject())).thenReturn(Success(courses))

      val request = FakeRequest(
        GET,
        s"/${entityTypeName.toLowerCase}s?invalidAttribute=${first.lecturer.toString}"
      )

      val result = controller.all()(request)

      status(result) shouldBe SERVICE_UNAVAILABLE
      contentType(result) shouldBe Some("application/json")
      contentAsJson(result) shouldBe Json.obj(
        "status" -> "KO",
        "message" -> "Unknown attribute"
      )
    }

    "not return courses when there is an invalid query parameter value" in {
      val invalidParameter = "invalidParameterValue"

      val first = Course("label1", "desc1", "abbreviation1", User.randomUUID, 1)
      val second = Course("label2", "desc2", "abbreviation2", User.randomUUID, 1)
      val third = Course("label3", "desc3", "abbreviation3", User.randomUUID, 1)
      val fourth = Course("label4", "desc4", "abbreviation4", User.randomUUID, 1)

      val courses = Set(first, second, third, fourth)

      when(repository.getAll[Course](anyObject())).thenReturn(Success(courses))

      val request = FakeRequest(
        GET,
        s"/${entityTypeName.toLowerCase}invalidParameter?${CourseCRUDController.lecturerAttribute}=$invalidParameter"
      )

      val result = controller.all()(request)

      status(result) shouldBe SERVICE_UNAVAILABLE
      contentType(result) shouldBe Some("application/json")
      contentAsJson(result) shouldBe Json.obj(
        "status" -> "KO",
        "message" -> s"Invalid UUID string: $invalidParameter"
      )
    }

    s"handle this model issue when creating a new $entityTypeName which already exists" in {
      when(repository.prepareQuery(anyObject())).thenReturn(query)
      when(qe.execute(anyObject())).thenReturn(Success(Map(
        "s" -> List(factory.createLiteral(Course.generateUri(entityToPass)))
      )))
      when(repository.get[Course](anyObject())(anyObject())).thenReturn(Success(Some(entityToPass)))

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
      when(repository.prepareQuery(anyObject())).thenReturn(query)
      when(qe.execute(anyObject())).thenReturn(Success(Map(
        "s" -> List(factory.createLiteral(Course.generateUri(entityToPass)))
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

    implicit val writes: Writes[CourseProtocol] = Json.writes[CourseProtocol]

    "create a course whilst also creating its respective security models" in {
      val rm = Role(Roles.RightsManager, Set.empty)
      val cm = Role(Roles.CourseManager, Set.empty)

      val course = CourseProtocol("Course", "Desc", "C", UUID.randomUUID(), 0)
      val dummyGraph = PointedGraph[repository.Rdf](makeBNodeLabel("empty"))

      when(repository.prepareQuery(anyObject())).thenReturn(query)
      when(qe.execute(anyObject())).thenReturn(Success(Map.empty[String, List[Value]]))

      when(roleService.rolesForCourse(anyObject())).thenReturn(Success(Set(rm, cm)))
      when(repository.addMany[Authority](anyObject())(anyObject())).thenReturn(Success(Set(dummyGraph)))
      when(repository.add[Course](anyObject())(anyObject())).thenReturn(Success(dummyGraph))

      val request = FakeRequest(
        POST,
        s"/${entityTypeName}sWithRights",
        FakeHeaders(Seq(HeaderNames.CONTENT_TYPE -> mimeType)),
        Json.toJson(course)
      )

      val result = controller.createWithRoles()(request)

      status(result) shouldBe CREATED
      contentType(result) shouldBe Some[String](mimeType)
    }

    "skip adding rights-manager when already exists" in {
      val cm = Role(Roles.CourseManager, Set.empty)

      val course = CourseProtocol("Course", "Desc", "C", UUID.randomUUID(), 0)
      val dummyGraph = PointedGraph[repository.Rdf](makeBNodeLabel("empty"))

      when(repository.prepareQuery(anyObject())).thenReturn(query)
      when(qe.execute(anyObject())).thenReturn(Success(Map.empty[String, List[Value]]))

      when(roleService.rolesForCourse(anyObject())).thenReturn(Success(Set(cm)))
      when(repository.addMany[Authority](anyObject())(anyObject())).thenReturn(Success(Set(dummyGraph)))
      when(repository.add[Course](anyObject())(anyObject())).thenReturn(Success(dummyGraph))

      val request = FakeRequest(
        POST,
        s"/${entityTypeName}sWithRights",
        FakeHeaders(Seq(HeaderNames.CONTENT_TYPE -> mimeType)),
        Json.toJson(course)
      )

      val result = controller.createWithRoles()(request)

      status(result) shouldBe CREATED
      contentType(result) shouldBe Some[String](mimeType)
    }

    "stop the creation when the appropriate roles haven't been found" in {
      val course = CourseProtocol("Course", "Desc", "C", UUID.randomUUID(), 0)

      when(repository.prepareQuery(anyObject())).thenReturn(query)
      when(qe.execute(anyObject())).thenReturn(Success(Map.empty[String, List[Value]]))

      when(roleService.rolesForCourse(anyObject())).thenReturn(Success(Set.empty[Role]))

      val request = FakeRequest(
        POST,
        s"/${entityTypeName}sWithRights",
        FakeHeaders(Seq(HeaderNames.CONTENT_TYPE -> mimeType)),
        Json.toJson(course)
      )

      val result = controller.createWithRoles()(request)

      status(result) shouldBe INTERNAL_SERVER_ERROR
      contentType(result) shouldBe Some("application/json")
    }
  }

}
