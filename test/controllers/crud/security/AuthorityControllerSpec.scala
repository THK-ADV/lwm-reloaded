package controllers.crud.security

import java.util.UUID

import controllers.crud.{AbstractCRUDController, AbstractCRUDControllerSpec}
import controllers.security.AuthorityController
import models.security._
import models.users.{Employee, Student}
import org.mockito.Matchers._
import org.mockito.Mockito._
import org.w3.banana.PointedGraph
import org.w3.banana.sesame.Sesame
import play.api.libs.json.{JsValue, Json, Writes}
import play.api.test.FakeRequest
import play.api.test.Helpers._
import utils.LwmMimeType

import scala.util.{Failure, Success}

class AuthorityControllerSpec extends AbstractCRUDControllerSpec[AuthorityProtocol, Authority] {

  override def entityTypeName: String = "authority"

  override val controller: AbstractCRUDController[AuthorityProtocol, Authority] = new AuthorityController(repository, namespace, roleService) {

    override protected def fromInput(input: AuthorityProtocol, existing: Option[Authority]): Authority = entityToPass

    override protected def contextFrom: PartialFunction[Rule, SecureContext] = {
      case _ => NonSecureBlock
    }
  }

  val studentToPass = Student("systemId to pass", "last name to pass", "first name to pass", "email to pass", "regId to pass", UUID.randomUUID(), Student.randomUUID)
  val employeeToFail =  Employee("systemId to fail", "last name to fail", "first name to fail", "email to fail", Employee.randomUUID)

  val refRolesToPass = Set(
    RefRole(None, UUID.randomUUID()),
    RefRole(Some(UUID.randomUUID()), UUID.randomUUID())
  )
  val refRolesToFail = Set(
    RefRole(None, UUID.randomUUID()),
    RefRole(Some(UUID.randomUUID()), UUID.randomUUID())
  )

  override val entityToFail: Authority = Authority(
    employeeToFail.id,
    refRolesToFail.map(_.id),
    Authority.randomUUID
  )

  override val entityToPass: Authority = Authority(
    studentToPass.id,
    refRolesToPass.map(_.id),
    Authority.randomUUID
  )

  val atomizedEntityToPass = AuthorityStudentAtom(
    studentToPass,
    refRolesToPass,
    entityToPass.id
  )

  val atomizedEntityToFail = AuthorityEmployeeAtom(
    employeeToFail,
    refRolesToFail,
    entityToFail.id
  )

  import ops._
  import bindings.AuthorityBinding.authorityBinder
  override val pointedGraph: PointedGraph[Sesame] = entityToPass.toPG

  override implicit val jsonWrites: Writes[Authority] = Authority.writes

  override val mimeType: LwmMimeType = LwmMimeType.authorityV1Json

  override val inputJson: JsValue = Json.obj(
    "user" -> entityToPass.user,
    "refRoles" -> entityToPass.refRoles
  )

  override val updateJson: JsValue = Json.obj(
    "user" -> entityToPass.user,
    "refRoles" -> (entityToPass.refRoles + RefRole.randomUUID)
  )

  "A AuthorityControllerSpec " should {
    s"successfully get a single student authority atomized" in {
      import Authority.atomicStudentWrites

      doReturn(Success(Some(entityToPass))).
        doReturn(Success(None)).
        doReturn(Success(Some(studentToPass))).
        when(repository).get(anyObject())(anyObject())

      when(repository.getMany[RefRole](anyObject())(anyObject())).thenReturn(Success(refRolesToPass))

      val request = FakeRequest(
        GET,
        s"/authorities/${entityToPass.id}"
      )
      val result = controller.getAtomic(entityToPass.id.toString)(request)

      status(result) shouldBe OK
      contentType(result) shouldBe Some[String](mimeType)
      contentAsJson(result) shouldBe Json.toJson(atomizedEntityToPass)
    }

    s"successfully get a single employee authority atomized" in {
      import Authority.atomicEmployeeWrites

      doReturn(Success(Some(entityToFail))).
        doReturn(Success(Some(employeeToFail))).
        doReturn(Success(None)).
        when(repository).get(anyObject())(anyObject())

      when(repository.getMany[RefRole](anyObject())(anyObject())).thenReturn(Success(refRolesToFail))

      val request = FakeRequest(
        GET,
        s"/authorities/${entityToFail.id}"
      )
      val result = controller.getAtomic(entityToFail.id.toString)(request)

      status(result) shouldBe OK
      contentType(result) shouldBe Some[String](mimeType)
      contentAsJson(result) shouldBe Json.toJson(atomizedEntityToFail)
    }

    s"not get a single authority atomized when user is not found" in {
      doReturn(Success(Some(entityToPass))).
        doReturn(Success(None)).
        doReturn(Success(None)).
        when(repository).get(anyObject())(anyObject())

      when(repository.getMany[RefRole](anyObject())(anyObject())).thenReturn(Success(refRolesToPass))

      val request = FakeRequest(
        GET,
        s"/authorities/${entityToPass.id}"
      )
      val result = controller.getAtomic(entityToPass.id.toString)(request)

      status(result) shouldBe NOT_FOUND
      contentType(result) shouldBe Some("application/json")
      contentAsJson(result) shouldBe Json.obj(
        "status" -> "KO",
        "message" -> "No such element..."
      )
    }

    s"not get a single authority atomized when there is an exception" in {
      val errorMessage = s"Oops, cant get the desired authority for some reason"

      doReturn(Success(Some(entityToPass))).
        doReturn(Failure(new Exception(errorMessage))).
        doReturn(Success(None)).
        when(repository).get(anyObject())(anyObject())

      when(repository.getMany[RefRole](anyObject())(anyObject())).thenReturn(Success(refRolesToPass))

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
  }

  /*s"successfully get all ${fgrammar(entityTypeName)} atomized" in {
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
  }*/
}