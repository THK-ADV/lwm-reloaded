package controllers.crud.security

import java.util.UUID

import controllers.crud.{AbstractCRUDController, AbstractCRUDControllerSpec}
import controllers.security.AuthorityController
import models.security._
import models.users.{Employee, Student}
import org.mockito.Matchers._
import org.mockito.Mockito._
import org.w3.banana.{PointedGraph, RDFPrefix}
import org.w3.banana.sesame.Sesame
import play.api.libs.json.{JsArray, JsValue, Json, Writes}
import play.api.test.FakeRequest
import play.api.test.Helpers._
import utils.LwmMimeType
import Permissions._
import models.Course
import org.openrdf.model.Value
import store.Prefixes.LWMPrefix
import store.{Namespace, SesameRepository}
import utils.Ops.MonadInstances._

import scala.util.{Failure, Success}

class AuthorityControllerSpec extends AbstractCRUDControllerSpec[AuthorityProtocol, Authority] {

  override def entityTypeName: String = "authority"

  override val controller: AbstractCRUDController[AuthorityProtocol, Authority] = new AuthorityController(repository, sessionService, namespace, roleService) {

    override protected def fromInput(input: AuthorityProtocol, existing: Option[Authority]): Authority = entityToPass

    override protected def contextFrom: PartialFunction[Rule, SecureContext] = {
      case _ => NonSecureBlock
    }
  }

  val studentToPass = Student("systemId to pass", "last name to pass", "first name to pass", "email to pass", "regId to pass", UUID.randomUUID())
  val employeeToFail = Employee("systemId to fail", "last name to fail", "first name to fail", "email to fail", "status to fail")

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

  "filter through nested levels of graphs" in {
    import bindings.AuthorityBinding._
    import bindings.RefRoleBinding._

    val realRepo = SesameRepository(namespace)
    val lwm = LWMPrefix[realRepo.Rdf](realRepo.rdfOps, realRepo.rdfOps)

    val localController = new AuthorityController(realRepo, sessionService, namespace, roleService) {
      override protected def contextFrom: PartialFunction[Rule, SecureContext] = {
        case _ => NonSecureBlock
      }
      override protected def restrictedContext(restrictionId: String): PartialFunction[Rule, SecureContext] = {
        case _ => NonSecureBlock
      }
    }

    val role1 = UUID.randomUUID()
    val role2 = UUID.randomUUID()

    val user1 = UUID.randomUUID()
    val user2 = UUID.randomUUID()
    val user3 = UUID.randomUUID()

    val course1 = UUID.randomUUID()
    val course2 = UUID.randomUUID()

    val refrole1 = RefRole(Some(course1), role1)
    val refrole2 = RefRole(None, role2)
    val refrole3 = RefRole(Some(course2), role1)

    val auth1 = Authority(user1, Set(refrole1.id, refrole2.id))
    val auth2 = Authority(user2, Set(refrole2.id))
    val auth3 = Authority(user3, Set(refrole1.id, refrole3.id))

    realRepo.addMany[RefRole](List(refrole1, refrole2))
    realRepo.addMany[Authority](List(auth1, auth2, auth3))

    val requestWithCourse = FakeRequest(
      GET,
      s"/${entityTypeName}s?course=$course2"
    )

    val requestWithCourseAndRole = FakeRequest(
      GET,
      s"/${entityTypeName}s?course=$course1&role=$role1"
    )

    val requestWithUser = FakeRequest(
      GET,
      s"/${entityTypeName}s?user=$user3"
    )

    val requestManyCourses = FakeRequest(
      GET,
      s"/${entityTypeName}s?course=$course2&course=$course1"
    )

    val requestManyCoursesAndRole = FakeRequest(
      GET,
      s"/${entityTypeName}s?course=$course2&course=$course1&role=$role1"
    )

    val result1 = localController.all()(requestWithCourse)
    val result2 = localController.all()(requestWithCourseAndRole)
    val result3 = localController.all()(requestWithUser)
    val result4 = localController.all()(requestManyCourses)
    val result5 = localController.all()(requestManyCoursesAndRole)

    val expected1 = List(Json.toJson(auth2))
    val expected2 = List(Json.toJson(auth1), Json.toJson(auth3))
    val expected3 = List(Json.toJson(auth3))
    val expected4 = List(Json.toJson(auth1), Json.toJson(auth3))
    val expected5 = List(Json.toJson(auth3))

    contentAsJson(result1).asInstanceOf[JsArray].value foreach { auth =>
      expected1 contains auth shouldBe true
    }

    contentAsJson(result2).asInstanceOf[JsArray].value foreach { auth =>
      expected2 contains auth shouldBe true
    }

    contentAsJson(result3).asInstanceOf[JsArray].value foreach { auth =>
     expected3 contains auth shouldBe true
    }

    contentAsJson(result4).asInstanceOf[JsArray].value foreach { auth =>
      expected4 contains auth shouldBe true
    }

    contentAsJson(result5).asInstanceOf[JsArray].value foreach { auth =>
      expected5 contains auth shouldBe true
    }
  }
}