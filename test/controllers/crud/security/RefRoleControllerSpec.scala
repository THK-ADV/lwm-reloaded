package controllers.crud.security

import java.util.UUID

import controllers.crud.{AbstractCRUDController, AbstractCRUDControllerSpec}
import controllers.security.RefRoleController
import models.{CourseAtom, Course}
import models.security._
import models.users.{Employee, User}
import org.mockito.Matchers._
import org.mockito.Mockito._
import org.w3.banana.PointedGraph
import org.w3.banana.sesame.Sesame
import play.api.libs.json.{JsValue, Json, Writes}
import play.api.test.FakeRequest
import play.api.test.Helpers._
import utils.LwmMimeType
import models.security.Permissions._

import scala.util.{Failure, Success}

class RefRoleControllerSpec extends AbstractCRUDControllerSpec[RefRoleProtocol, RefRole] {

  import ops._
  import bindings.RefRoleDescriptor

  override def entityTypeName: String = "refRole"

  override val controller: AbstractCRUDController[RefRoleProtocol, RefRole] = new RefRoleController(repository, sessionService, namespace, roleService) {

    override protected def fromInput(input: RefRoleProtocol, existing: Option[RefRole]): RefRole = entityToPass

    override protected def contextFrom: PartialFunction[Rule, SecureContext] = {
      case _ => NonSecureBlock
    }
  }

  val employeeToPass = Employee("systemId to pass", "last name to pass", "first name to pass", "email to pass", "status to pass")
  val courseToPass = Course("label to pass", "desc to pass", "abbrev to pass", employeeToPass.id, 1, Course.randomUUID)
  val courseAtomToPass = CourseAtom(courseToPass.label, courseToPass.description, courseToPass.abbreviation, employeeToPass, courseToPass.semesterIndex, courseToPass.id)
  val roleToPass = Role("role to pass", labwork.all)
  val roleToFail = Role("role to fail", course.all)

  override val entityToFail: RefRole = RefRole(None, roleToFail.id, RefRole.randomUUID)

  override val entityToPass: RefRole = RefRole(Some(courseAtomToPass.id), roleToPass.id, RefRole.randomUUID)

  override implicit val jsonWrites: Writes[RefRole] = RefRole.writes

  override val mimeType: LwmMimeType = LwmMimeType.refRoleV1Json

  implicit val refroleBinder = RefRoleDescriptor.binder

  override val pointedGraph: PointedGraph[Sesame] = entityToPass.toPG

  override val inputJson: JsValue = Json.obj(
    "module" -> entityToPass.course,
    "role" -> entityToPass.role
  )

  override val updateJson: JsValue = Json.obj(
    "module" -> entityToPass.course,
    "role" -> UUID.randomUUID()
  )

  val atomizedEntityToPass = RefRoleAtom(Some(courseAtomToPass), roleToPass, entityToPass.id)
  val atomizedEntityToFail = RefRoleAtom(None, roleToFail, entityToFail.id)

  "A RefRoleControllerSpec " should {

    "return refRoles for a given course" in {
      val course = UUID.randomUUID
      val rr1 = RefRole(Some(course), UUID.randomUUID)
      val rr2 = RefRole(Some(UUID.randomUUID), UUID.randomUUID)
      val rr3 = RefRole(Some(course), UUID.randomUUID)
      val rr4 = RefRole(None, UUID.randomUUID)

      when(repository.getAll[RefRole](anyObject())).thenReturn(Success(Set(rr1, rr2, rr3, rr4)))

      val request = FakeRequest(
        GET,
        s"/refRoles?${RefRoleController.courseAttribute}=$course"
      )
      val result = controller.all()(request)

      status(result) shouldBe OK
      contentType(result) shouldBe Some[String](mimeType)
      contentAsJson(result) shouldBe Json.toJson(Set(rr1, rr3))
    }

    "successfully get a single refrole with course restriction atomized" in {
      import RefRole.atomicWrites

      doReturn(Success(Some(entityToPass))).
        doReturn(Success(Some(atomizedEntityToPass))).
        when(repository).get(anyObject())(anyObject())

      val request = FakeRequest(
        GET,
        s"/refRoles/${entityToPass.id}"
      )
      val result = controller.getAtomic(entityToPass.id.toString)(request)

      status(result) shouldBe OK
      contentType(result) shouldBe Some[String](mimeType)
      contentAsJson(result) shouldBe Json.toJson(atomizedEntityToPass)
    }

    "successfully get a single refrole without course restriction atomized" in {
      import RefRole.atomicWrites

      doReturn(Success(Some(entityToFail))).
        doReturn(Success(Some(atomizedEntityToFail))).
        when(repository).get(anyObject())(anyObject())

      val request = FakeRequest(
        GET,
        s"/refRoles/${entityToFail.id}"
      )
      val result = controller.getAtomic(entityToFail.id.toString)(request)

      status(result) shouldBe OK
      contentType(result) shouldBe Some[String](mimeType)
      contentAsJson(result) shouldBe Json.toJson(atomizedEntityToFail)
    }

    "not get a single refrole atomized when role is not found" in {
      doReturn(Success(Some(entityToPass))).
        doReturn(Success(None)).
        doReturn(Success(None)).
        when(repository).get(anyObject())(anyObject())

      val request = FakeRequest(
        GET,
        s"/refRoles/${entityToPass.id}"
      )
      val result = controller.getAtomic(entityToPass.id.toString)(request)

      status(result) shouldBe NOT_FOUND
      contentType(result) shouldBe Some("application/json")
      contentAsJson(result) shouldBe Json.obj(
        "status" -> "KO",
        "message" -> "No such element..."
      )
    }

    "not get a single refrole atomized when there is an exception" in {
      val errorMessage = s"Oops, cant get the desired authority for some reason"

      doReturn(Success(Some(entityToPass))).
        doReturn(Failure(new Exception(errorMessage))).
        when(repository).get(anyObject())(anyObject())

      val request = FakeRequest(
        GET,
        s"/authorities/${entityToPass.id}"
      )
      val result = controller.getAtomic(entityToPass.id.toString)(request)

      status(result) shouldBe INTERNAL_SERVER_ERROR
      contentType(result) shouldBe Some("application/json")
      contentAsJson(result) shouldBe Json.obj(
        "status" -> "KO",
        "errors" -> errorMessage
      )
    }

    "successfully get all refroles atomized" in {
      import RefRole.atomicWrites

      val refRoles = Set(entityToPass, entityToFail)

      when(repository.getAll[RefRole](anyObject())).thenReturn(Success(refRoles))

      doReturn(Success(Some(atomizedEntityToPass))).
        doReturn(Success(Some(atomizedEntityToFail))).
        when(repository).get(anyObject())(anyObject())

      val request = FakeRequest(
        GET,
        s"/${entityTypeName}s"
      )
      val result = controller.allAtomic()(request)

      status(result) shouldBe OK
      contentType(result) shouldBe Some[String](mimeType)
      contentAsJson(result) shouldBe Json.toJson(Set(atomizedEntityToPass, atomizedEntityToFail))
    }

    "not get all refroles atomized when there is an exception" in {
      val refRoles = Set(entityToPass, entityToFail)
      val errorMessage = s"Oops, cant get the desired $entityTypeName for some reason"

      when(repository.getAll[RefRole](anyObject())).thenReturn(Success(refRoles))

      doReturn(Failure(new Exception(errorMessage))).
        when(repository).get(anyObject())(anyObject())

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
