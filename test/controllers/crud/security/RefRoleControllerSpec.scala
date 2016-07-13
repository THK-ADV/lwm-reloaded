package controllers.crud.security

import java.util.UUID

import controllers.crud.AbstractCRUDControllerSpec
import controllers.security.RefRoleController
import models.{CourseAtom, Course}
import models.security._
import models.users.Employee
import org.mockito.Matchers._
import org.mockito.Mockito._
import org.w3.banana.PointedGraph
import org.w3.banana.sesame.Sesame
import play.api.libs.json.{JsValue, Json, Writes}
import play.api.test.FakeRequest
import play.api.test.Helpers._
import utils.LwmMimeType
import models.security.Permissions._
import scala.util.Success

class RefRoleControllerSpec extends AbstractCRUDControllerSpec[RefRoleProtocol, RefRole, RefRoleAtom] {

  import ops._
  import bindings.RefRoleDescriptor

  override def entityTypeName: String = "refRole"

  override val controller: RefRoleController = new RefRoleController(repository, sessionService, namespace, roleService) {

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

  override val atomizedEntityToPass = RefRoleAtom(Some(courseAtomToPass), roleToPass, entityToPass.id)

  override val atomizedEntityToFail = RefRoleAtom(None, roleToFail, entityToFail.id)

  override implicit val jsonWrites: Writes[RefRole] = RefRole.writes

  override implicit def jsonWritesAtom: Writes[RefRoleAtom] = RefRole.writesAtom

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
  }
}
