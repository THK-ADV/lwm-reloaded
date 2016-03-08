package controllers.crud

import java.util.UUID

import models.security.{Authority, RefRole, Role, Roles}
import models.users.{Employee, User}
import models.{Course, CourseAtom, CourseProtocol}
import org.mockito.Matchers
import org.mockito.Matchers._
import org.mockito.Mockito._
import org.openrdf.model.Value
import org.w3.banana.PointedGraph
import org.w3.banana.sesame.Sesame
import play.api.http.HeaderNames
import play.api.libs.json.{JsValue, Json, Writes}
import play.api.test.{FakeHeaders, FakeRequest}
import play.api.test.Helpers._
import utils.LwmMimeType

import scala.util.{Failure, Success, Try}

class CourseCRUDControllerSpec extends AbstractCRUDControllerSpec[CourseProtocol, Course] {

  val lecturerToPass = Employee("systemId to pass", "last name to pass", "first name to pass", "email to pass", Employee.randomUUID)
  val lecturerToFail = Employee("systemId to fail", "last name to fail", "first name to fail", "email to fail", Employee.randomUUID)

  override val entityToPass: Course = Course("label to pass", "description to pass", "abbreviation to pass", lecturerToPass.id, 1, Course.randomUUID)

  override val controller: CourseCRUDController = new CourseCRUDController(repository, namespace, roleService) {

    override protected def fromInput(input: CourseProtocol, id: Option[UUID]) = entityToPass

    override protected def contextFrom: PartialFunction[Rule, SecureContext] = {
      case _ => NonSecureBlock
    }
  }

  override val entityToFail: Course = Course("label to fail", "description to fail", "abbreviation to fail", lecturerToFail.id, 1, Course.randomUUID)

  val atomizedEntityToPass = CourseAtom(
    entityToPass.label,
    entityToPass.description,
    entityToPass.abbreviation,
    lecturerToPass,
    entityToPass.semesterIndex,
    entityToPass.id
  )

  val atomizedEntityToFail = CourseAtom(
    entityToFail.label,
    entityToFail.description,
    entityToFail.abbreviation,
    lecturerToFail,
    entityToFail.semesterIndex,
    entityToFail.id
  )

  override implicit val jsonWrites: Writes[Course] = Course.writes

  override val mimeType: LwmMimeType = LwmMimeType.courseV1Json

  override val inputJson: JsValue = Json.obj(
    "label" -> entityToPass.label,
    "description" -> entityToPass.description,
    "abbreviation" -> entityToPass.abbreviation,
    "lecturer" -> entityToPass.lecturer,
    "semesterIndex" -> entityToPass.semesterIndex
  )

  override def entityTypeName: String = "course"

  import bindings.CourseBinding.courseBinder
  import ops._

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
      val lecturer = Employee("systemId", "last name", "first name", "email", Employee.randomUUID)

      val first = Course("label1", "desc1", "abbreviation1", Employee.randomUUID, 1, Course.randomUUID)
      val second = Course("label2", "desc2", "abbreviation2", lecturer.id, 1, Course.randomUUID)
      val third = Course("label3", "desc3", "abbreviation3", Employee.randomUUID, 1, Course.randomUUID)
      val fourth = Course("label4", "desc4", "abbreviation4", Employee.randomUUID, 1, Course.randomUUID)

      val courses = Set(first, second, third, fourth)

      when(repository.get[Course](anyObject(), anyObject())).thenReturn(Success(courses))

      val request = FakeRequest(
        GET,
        s"/${entityTypeName.toLowerCase}s?${CourseCRUDController.lecturerAttribute}=${lecturer.id.toString}"
      )

      val result = controller.asInstanceOf[CourseCRUDController].all()(request)

      status(result) shouldBe OK
      contentType(result) shouldBe Some[String](mimeType)
      contentAsJson(result) shouldBe Json.toJson(Set(second))
    }

    "return all corresponding courses for a given lecturer" in {
      val lecturer = Employee("systemId", "last name", "first name", "email", Employee.randomUUID)

      val first = Course("label1", "desc1", "abbreviation1", lecturer.id, 1, Course.randomUUID)
      val second = Course("label2", "desc2", "abbreviation2", Employee.randomUUID, 1, Course.randomUUID)
      val third = Course("label3", "desc3", "abbreviation3", lecturer.id, 1, Course.randomUUID)
      val fourth = Course("label4", "desc4", "abbreviation4", Employee.randomUUID, 1, Course.randomUUID)

      val courses = Set(first, second, third, fourth)

      when(repository.get[Course](anyObject(), anyObject())).thenReturn(Success(courses))

      val request = FakeRequest(
        GET,
        s"/${entityTypeName.toLowerCase}s?${CourseCRUDController.lecturerAttribute}=${lecturer.id.toString}"
      )

      val result = controller.asInstanceOf[CourseCRUDController].all()(request)

      status(result) shouldBe OK
      contentType(result) shouldBe Some[String](mimeType)
      contentAsJson(result) shouldBe Json.toJson(Set(first, third))
    }

    "not return courses for a lecturer when there is no match" in {
      val lecturer = Employee("systemId", "last name", "first name", "email", Employee.randomUUID)

      val first = Course("label1", "desc1", "abbreviation1", Employee.randomUUID, 1, Course.randomUUID)
      val second = Course("label2", "desc2", "abbreviation2", Employee.randomUUID, 1, Course.randomUUID)
      val third = Course("label3", "desc3", "abbreviation3", Employee.randomUUID, 1, Course.randomUUID)
      val fourth = Course("label4", "desc4", "abbreviation4", Employee.randomUUID, 1, Course.randomUUID)

      val courses = Set(first, second, third, fourth)

      when(repository.get[Course](anyObject(), anyObject())).thenReturn(Success(courses))

      val request = FakeRequest(
        GET,
        s"/${entityTypeName.toLowerCase}s?${CourseCRUDController.lecturerAttribute}=${lecturer.id.toString}"
      )

      val result = controller.asInstanceOf[CourseCRUDController].all()(request)

      status(result) shouldBe OK
      contentType(result) shouldBe Some[String](mimeType)
      contentAsJson(result) shouldBe Json.toJson(Set.empty[Course])
    }

    "not return courses when there is an invalid query attribute" in {
      val first = Course("label1", "desc1", "abbreviation1", Employee.randomUUID, 1, Course.randomUUID)
      val second = Course("label2", "desc2", "abbreviation2", Employee.randomUUID, 1, Course.randomUUID)
      val third = Course("label3", "desc3", "abbreviation3", Employee.randomUUID, 1, Course.randomUUID)
      val fourth = Course("label4", "desc4", "abbreviation4", Employee.randomUUID, 1, Course.randomUUID)

      val courses = Set(first, second, third, fourth)

      when(repository.get[Course](anyObject(), anyObject())).thenReturn(Success(courses))

      val request = FakeRequest(
        GET,
        s"/${entityTypeName.toLowerCase}s?invalidAttribute=${first.lecturer.toString}"
      )

      val result = controller.asInstanceOf[CourseCRUDController].all()(request)

      status(result) shouldBe SERVICE_UNAVAILABLE
      contentType(result) shouldBe Some("application/json")
      contentAsJson(result) shouldBe Json.obj(
        "status" -> "KO",
        "message" -> "Unknown attribute"
      )
    }

    "not return courses when there is an invalid query parameter value" in {
      val invalidParameter = "invalidParameterValue"

      val first = Course("label1", "desc1", "abbreviation1", Employee.randomUUID, 1, Course.randomUUID)
      val second = Course("label2", "desc2", "abbreviation2", Employee.randomUUID, 1, Course.randomUUID)
      val third = Course("label3", "desc3", "abbreviation3", Employee.randomUUID, 1, Course.randomUUID)
      val fourth = Course("label4", "desc4", "abbreviation4", Employee.randomUUID, 1, Course.randomUUID)

      val courses = Set(first, second, third, fourth)

      when(repository.get[Course](anyObject(), anyObject())).thenReturn(Success(courses))

      val request = FakeRequest(
        GET,
        s"/${entityTypeName.toLowerCase}invalidParameter?${CourseCRUDController.lecturerAttribute}=$invalidParameter"
      )

      val result = controller.asInstanceOf[CourseCRUDController].all()(request)

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
      when(repository.get[Course](anyObject())(anyObject())).thenReturn(Success(None))
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
      import Course.atomicWrites

      doReturn(Success(Some(entityToPass))).
      doReturn(Success(Some(lecturerToPass))).
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

    s"not get a single $entityTypeName atomized when lecturer is not found" in {
      doReturn(Success(Some(entityToPass))).
      doReturn(Success(None)).
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
      import Course.atomicWrites

      val courses = Set(entityToPass, entityToFail)
      val lecturers = Set(lecturerToPass, lecturerToFail)

      when(repository.get[Course](anyObject(), anyObject())).thenReturn(Success(courses))
      when(repository.getMany[Employee](anyObject())(anyObject())).thenReturn(Success(lecturers))

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
      val courses = Set(entityToPass, entityToFail)
      val errorMessage = s"Oops, cant get the desired $entityTypeName for some reason"

      when(repository.get[Course](anyObject(), anyObject())).thenReturn(Success(courses))
      when(repository.getMany[Employee](anyObject())(anyObject())).thenReturn(Failure(new Exception(errorMessage)))

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

    "create a course whilst also creating its respective security models" in {
      def role(r: String) = Role(r, Set.empty)

      implicit val writes: Writes[CourseProtocol] = Json.writes[CourseProtocol]
      val course = CourseProtocol("Course", "Desc", "C", UUID.randomUUID(), 0)
      val roles = Set(role(Roles.RightsManager), role(Roles.CourseEmployee), role(Roles.CourseManager), role(Roles.CourseAssistant))
      val refrole = RefRole(None, roles.head.id)
      val dummyGraph = PointedGraph[repository.Rdf](makeBNodeLabel("empty"))
      val auth = Authority.empty

      when(repository.prepareQuery(anyObject())).thenReturn(query)
      when(qe.execute(anyObject())).thenReturn(Success(Map.empty[String, List[Value]]))

      when(repository.get[Role](anyObject(), anyObject())).thenReturn(Success(roles))
      when(roleService.authorityFor(anyObject())).thenReturn(Success(Some(auth)))
      when(repository.get[RefRole](anyObject())(anyObject())).thenReturn(Success(Some(refrole)))
      when(repository.add[Course](anyObject())(anyObject())).thenReturn(Success(dummyGraph))
      when(repository.addMany[RefRole](anyObject())(anyObject())).thenReturn(Success(Set(dummyGraph)))
      when(repository.update(anyObject())(anyObject(), anyObject())).thenReturn(Success(dummyGraph))

      val request = FakeRequest(
        POST,
        s"/${entityTypeName}sWithRights",
        FakeHeaders(Seq(HeaderNames.CONTENT_TYPE -> mimeType)),
        Json.toJson(course)
      )

      val result = controller.createWithRoles()(request)

      status(result) shouldBe CREATED
      contentType(result) shouldBe Some(mimeType.value)
    }


    "stop the creation when the appropriate roles haven't been found" in {
      def role(r: String) = Role(r, Set.empty)

      implicit val writes: Writes[CourseProtocol] = Json.writes[CourseProtocol]
      val course = CourseProtocol("Course", "Desc", "C", UUID.randomUUID(), 0)
      val roles = Set.empty[Role]
      val dummyGraph = PointedGraph[repository.Rdf](makeBNodeLabel("empty"))

      when(repository.prepareQuery(anyObject())).thenReturn(query)
      when(qe.execute(anyObject())).thenReturn(Success(Map.empty[String, List[Value]]))
      when(repository.get[Role](anyObject(), anyObject())).thenReturn(Success(roles))

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
