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
import play.api.libs.json.{JsArray, JsValue, Json, Writes}
import play.api.test.FakeRequest
import play.api.test.Helpers._
import utils.LwmMimeType
import Permissions._
import models.Course

import scala.util.{Failure, Success}

class AuthorityControllerSpec extends AbstractCRUDControllerSpec[AuthorityProtocol, Authority] {

  override def entityTypeName: String = "authority"

  override val controller: AbstractCRUDController[AuthorityProtocol, Authority] = new AuthorityController(repository, namespace, roleService) {
    override protected def fromInput(input: AuthorityProtocol, id: Option[UUID]): Authority = entityToPass

    override protected def contextFrom: PartialFunction[Rule, SecureContext] = {
      case _ => NonSecureBlock
    }
  }

  val studentToPass = Student("systemId to pass", "last name to pass", "first name to pass", "email to pass", "regId to pass", UUID.randomUUID(), Student.randomUUID)
  val employeeToFail = Employee("systemId to fail", "last name to fail", "first name to fail", "email to fail", "status to fail", Employee.randomUUID)

  val refRoleIdToPass1 = RefRole.randomUUID
  val refRoleIdToPass2 = RefRole.randomUUID
  val refRoleIdToFail1 = RefRole.randomUUID
  val refRoleIdToFail2 = RefRole.randomUUID

  val courseToPass = Course("Course2", "Description", "Abbrev", UUID.randomUUID(), 0, Course.randomUUID)
  val courseToFail = Course("Course1", "Description", "Abbrev", UUID.randomUUID(), 0, Course.randomUUID)

  val role1 = Role("role1", Set(user.get, user.getAll))
  val role2 = Role("role2", Set(course.get, course.create, course.getAll))
  val role3 = Role("role3", Set(degree.get, degree.getAll))
  val role4 = Role("role4", Set(authority.get, authority.getAll))

  val refRolesAtomicToPass = Set(
    RefRoleAtom(None, role1, refRoleIdToPass1),
    RefRoleAtom(Some(courseToPass), role2, refRoleIdToPass2)
  )

  val refRolesAtomicToFail = Set(
    RefRoleAtom(None, role3, refRoleIdToFail1),
    RefRoleAtom(Some(courseToFail), role4, refRoleIdToFail2)
  )

  val refRolesToPass = Set(
    RefRole(None, role1.id, refRoleIdToPass1),
    RefRole(Some(courseToPass.id), role2.id, refRoleIdToPass2)
  )
  val refRolesToFail = Set(
    RefRole(None, role3.id, refRoleIdToFail1),
    RefRole(Some(courseToFail.id), role4.id, refRoleIdToFail2)
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

  val atomizedEntityToPass = AuthorityAtom(
    studentToPass,
    refRolesAtomicToPass,
    entityToPass.id
  )

  val atomizedEntityToFail = AuthorityAtom(
    employeeToFail,
    refRolesAtomicToFail,
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
      import Authority.writesAtomic

      doReturn(Success(Some(entityToPass))).
        doReturn(Success(Some(studentToPass))).
        doReturn(Success(Some(role1))).
        doReturn(Success(Some(courseToPass))).
        doReturn(Success(Some(role2))).
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
      import Authority.writesAtomic

      doReturn(Success(Some(entityToFail))).
        doReturn(Success(Some(employeeToFail))).
        doReturn(Success(Some(role3))).
        doReturn(Success(Some(courseToFail))).
        doReturn(Success(Some(role4))).
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

  s"successfully get all ${fgrammar(entityTypeName)} atomized" in {
    import Authority._

    val authorities = Set(entityToPass, entityToFail)

    when(repository.get[Authority](anyObject(), anyObject())).thenReturn(Success(authorities))

    doReturn(Success(Some(studentToPass))).
      doReturn(Success(Some(role1))).
      doReturn(Success(Some(courseToPass))).
      doReturn(Success(Some(role2))).
      doReturn(Success(Some(employeeToFail))).
      doReturn(Success(Some(role3))).
      doReturn(Success(Some(courseToFail))).
      doReturn(Success(Some(role4))).
      when(repository).get(anyObject())(anyObject())

    doReturn(Success(refRolesToPass)).
      doReturn(Success(refRolesToFail)).
      when(repository).getMany(anyObject())(anyObject())

    val request = FakeRequest(
      GET,
      s"/${entityTypeName}s"
    )
    val result = controller.allAtomic()(request)
    val jsVals = Set(Json.toJson(atomizedEntityToPass), Json.toJson(atomizedEntityToFail))

    status(result) shouldBe OK
    contentType(result) shouldBe Some[String](mimeType)
    contentAsJson(result).asInstanceOf[JsArray].value foreach { entity =>
      jsVals contains entity shouldBe true
    }
  }

  s"not get all ${fgrammar(entityTypeName)} atomized when there is an exception" in {
    val authorities = Set(entityToPass, entityToFail)
    val errorMessage = s"Oops, cant get the desired $entityTypeName for some reason"

    doReturn(Failure(new Exception(errorMessage))).
      when(repository).get(anyObject())(anyObject())

    when(repository.getMany[Authority](anyObject())(anyObject())).thenReturn(Success(authorities))

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