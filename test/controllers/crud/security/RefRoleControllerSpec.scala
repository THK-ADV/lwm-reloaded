package controllers.crud.security

import java.util.UUID

import controllers.crud.{AbstractCRUDController, AbstractCRUDControllerSpec}
import controllers.security.RefRoleController
import models.Course
import models.security._
import models.users.User
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
  import bindings.RefRoleBinding._
  override def entityTypeName: String = "refRole"

  override val controller: AbstractCRUDController[RefRoleProtocol, RefRole] = new RefRoleController(repository, sessionService, namespace, roleService) {

    override protected def fromInput(input: RefRoleProtocol, existing: Option[RefRole]): RefRole = entityToPass

    override protected def contextFrom: PartialFunction[Rule, SecureContext] = {
      case _ => NonSecureBlock
    }
  }

  val courseToPass = Course("label to pass", "desc to pass", "abbrev to pass", User.randomUUID, 1, Course.randomUUID)
  val roleToPass = Role("role to pass", labwork.all)
  val roleToFail = Role("role to fail", course.all)

  override val entityToFail: RefRole = RefRole(None, roleToFail.id, RefRole.randomUUID)

  override val entityToPass: RefRole = RefRole(Some(courseToPass.id), roleToPass.id, RefRole.randomUUID)

  override implicit val jsonWrites: Writes[RefRole] = RefRole.writes

  override val mimeType: LwmMimeType = LwmMimeType.refRoleV1Json

  override val pointedGraph: PointedGraph[Sesame] = entityToPass.toPG

  override val inputJson: JsValue = Json.obj(
    "module" -> entityToPass.module,
    "role" -> entityToPass.role
  )

  override val updateJson: JsValue = Json.obj(
    "module" -> entityToPass.module,
    "role" -> UUID.randomUUID()
  )

  val atomizedEntityToPass = RefRoleAtom(
    Some(courseToPass),
    roleToPass,
    entityToPass.id
  )
  val atomizedEntityToFail = RefRoleAtom(
    None,
    roleToFail,
    entityToFail.id
  )

  "A RefRoleControllerSpec " should {

    s"successfully get a single refrole with course restriction atomized" in {
      import RefRole.atomicWrites

      doReturn(Success(Some(entityToPass))).
      doReturn(Success(Some(roleToPass))).
      doReturn(Success(Some(courseToPass))).
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

    s"successfully get a single refrole without course restriction atomized" in {
      import RefRole.atomicWrites

      doReturn(Success(Some(entityToFail))).
        doReturn(Success(Some(roleToFail))).
        doReturn(Success(None)).
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

    s"not get a single refrole atomized when role is not found" in {
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

    s"not get a single refrole atomized when there is an exception" in {
      val errorMessage = s"Oops, cant get the desired authority for some reason"

      doReturn(Success(Some(entityToPass))).
        doReturn(Failure(new Exception(errorMessage))).
        doReturn(Success(Some(courseToPass))).
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

    s"successfully get all refroles atomized" in {
      import RefRole.atomicWrites

      val refRoles = Set(entityToPass, entityToFail)

      when(repository.get[RefRole](anyObject(), anyObject())).thenReturn(Success(refRoles))

      doReturn(Success(Some(roleToPass))).
        doReturn(Success(Some(courseToPass))).
        doReturn(Success(Some(roleToFail))).
        doReturn(Success(None)).
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

    s"not get all refroles atomized when there is an exception" in {
      val refRoles = Set(entityToPass, entityToFail)
      val errorMessage = s"Oops, cant get the desired $entityTypeName for some reason"

      when(repository.get[RefRole](anyObject(), anyObject())).thenReturn(Success(refRoles))

      doReturn(Success(Some(roleToPass))).
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
