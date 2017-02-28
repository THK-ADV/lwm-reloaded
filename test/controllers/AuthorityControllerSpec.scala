package controllers

import java.util.UUID

import base.StreamHandler._
import models.Permissions._
import models._
import org.mockito.Matchers._
import org.mockito.Mockito._
import org.w3.banana.PointedGraph
import org.w3.banana.sesame.Sesame
import play.api.libs.json.{JsValue, Json, Writes}
import play.api.test.FakeRequest
import play.api.test.Helpers._
import store.SesameRepository
import utils.LwmMimeType

import scala.util.Success

class AuthorityControllerSpec extends AbstractCRUDControllerSpec[SesameAuthorityProtocol, SesameAuthority, SesameAuthorityAtom] {

  override def entityTypeName: String = "authority"

  override val controller: AuthorityController = new AuthorityController(repository, sessionService, namespace, roleService) {

    override protected def fromInput(input: SesameAuthorityProtocol, existing: Option[SesameAuthority]): SesameAuthority = entityToPass

    override protected def contextFrom: PartialFunction[Rule, SecureContext] = {
      case _ => NonSecureBlock
    }
  }

  val studentToPass = SesameStudent("systemId to pass", "last name to pass", "first name to pass", "email to pass", "regId to pass", UUID.randomUUID())
  val employeeToPass = SesameEmployee("systemId to pass", "last name to pass", "first name to pass", "email to pass", "status to pass")
  val employeeToFail = SesameEmployee("systemId to fail", "last name to fail", "first name to fail", "email to fail", "status to fail")

  val courseToPass = Course("Course2", "Description", "Abbrev", employeeToPass.id, 0)
  val courseToFail = Course("Course1", "Description", "Abbrev", employeeToFail.id, 0)

  val courseAtomToPass = CourseAtom(courseToPass.label, courseToPass.description, courseToPass.abbreviation, employeeToPass, courseToPass.semesterIndex, courseToPass.invalidated, courseToPass.id)
  val courseAtomToFail = CourseAtom(courseToFail.label, courseToFail.description, courseToFail.abbreviation, employeeToFail, courseToFail.semesterIndex, courseToPass.invalidated, courseToFail.id)

  val role1 = SesameRole(Roles.AdminLabel, Set(user.get, user.getAll))
  val role2 = SesameRole("role2", Set(course.get, course.create, course.getAll))
  val role3 = SesameRole("role3", Set(degree.get, degree.getAll))
  val role4 = SesameRole("role4", Set(authority.get, authority.getAll))

  override val entityToPass: SesameAuthority = SesameAuthority(
    studentToPass.id,
    role1.id
  )

  override val entityToFail: SesameAuthority = SesameAuthority(
    employeeToFail.id,
    role2.id
  )

  override val atomizedEntityToPass = SesameAuthorityAtom(
    studentToPass,
    role1,
    Some(courseAtomToPass),
    entityToPass.invalidated,
    entityToPass.id
  )

  override val atomizedEntityToFail = SesameAuthorityAtom(
    employeeToFail,
    role2,
    Some(courseAtomToFail),
    entityToFail.invalidated,
    entityToFail.id
  )

  import bindings.AuthorityDescriptor
  import ops._

  implicit val authorityBinder = AuthorityDescriptor.binder
  override val pointedGraph: PointedGraph[Sesame] = entityToPass.toPG

  override implicit val jsonWrites: Writes[SesameAuthority] = SesameAuthority.writes

  override implicit def jsonWritesAtom: Writes[SesameAuthorityAtom] = SesameAuthority.writesAtom

  override val mimeType: LwmMimeType = LwmMimeType.authorityV1Json

  override val inputJson: JsValue = Json.obj(
    "user" -> entityToPass.user,
    "role" -> entityToPass.role,
    "course" -> entityToPass.course
  )

  override val updateJson: JsValue = Json.obj(
    "user" -> entityToPass.user,
    "role" -> role3.id
  )

  def role(label: String) = SesameRole(label, Set.empty)

  "A AuthorityControllerSpec " should {

    "filter through nested levels of graphs" in {
      import bindings.{AuthorityDescriptor, CourseDescriptor, EmployeeDescriptor, RoleDescriptor}

      val realRepo = SesameRepository(namespace)

      val localController: AuthorityController = new AuthorityController(realRepo, sessionService, namespace, roleService) {
        override protected def contextFrom: PartialFunction[Rule, SecureContext] = {
          case _ => NonSecureBlock
        }

        override protected def restrictedContext(restrictionId: String): PartialFunction[Rule, SecureContext] = {
          case _ => NonSecureBlock
        }
      }

      val role1 = SesameRole("", Set.empty)
      val role2 = SesameRole("", Set.empty)

      val user1 = SesameEmployee("", "", "", "", "")
      val user2 = SesameEmployee("", "", "", "", "")
      val user3 = SesameEmployee("", "", "", "", "")

      val course1 = Course("", "", "", UUID.randomUUID, 1)
      val course2 = Course("", "", "", UUID.randomUUID, 2)

      val auth1 = SesameAuthority(user1.id, role1.id, Some(course1.id))
      val auth2 = SesameAuthority(user1.id, role2.id)
      val auth3 = SesameAuthority(user2.id, role2.id)
      val auth4 = SesameAuthority(user3.id, role1.id, Some(course1.id))
      val auth5 = SesameAuthority(user3.id, role1.id, Some(course2.id))

      realRepo.addMany[SesameAuthority](List(auth1, auth2, auth3, auth4, auth5))
      realRepo.addMany[SesameRole](List(role1, role2))
      realRepo.addMany[SesameEmployee](List(user1, user2, user3))
      realRepo.addMany[Course](List(course1, course2))

      val requestWithCourse = FakeRequest(
        GET,
        s"/${
          entityTypeName
        }s?course=${course2.id}"
      )

      val requestWithCourseAndRole = FakeRequest(
        GET,
        s"/${
          entityTypeName
        }s?course=${course1.id}&role=${role1.id}"
      )

      val requestWithUser = FakeRequest(
        GET,
        s"/${
          entityTypeName
        }s?user=${user3.id}"
      )

      val result1 = localController.all()(requestWithCourse)
      val result2 = localController.all()(requestWithCourseAndRole)
      val result3 = localController.all()(requestWithUser)

      val expected1 = Set(Json.toJson(auth5))
      val expected2 = Set(Json.toJson(auth1), Json.toJson(auth4))
      val expected3 = Set(Json.toJson(auth4), Json.toJson(auth5))

      contentFromStream(result1) shouldBe expected1
      contentFromStream(result2) shouldBe expected2
      contentFromStream(result3) shouldBe expected3
    }

    "successfully delete an authority when there is at least one basic role left" in {
      val auth = SesameAuthorityAtom(atomizedEntityToPass.user, role(Roles.CourseAssistantLabel), Some(courseAtomToPass), None, UUID.randomUUID)

      when(repository.get[SesameAuthorityAtom](anyObject())(anyObject())).thenReturn(Success(Some(auth)))
      when(repository.invalidate[SesameAuthority](anyObject())(anyObject())).thenReturn(Success(()))

      val request = FakeRequest(
        DELETE,
        s"/${entityTypeName}s/${auth.id}"
      )

      val result = controller.delete(auth.id.toString)(request)

      status(result) shouldBe OK
      contentAsJson(result) shouldBe Json.obj("status" -> "OK")
    }

    "not delete an authority when he has only one basic role left" in {
      val auth = SesameAuthorityAtom(atomizedEntityToPass.user, role(Roles.StudentLabel), None, None, atomizedEntityToPass.id)

      when(repository.get[SesameAuthorityAtom](anyObject())(anyObject())).thenReturn(Success(Some(auth)))

      val request = FakeRequest(
        DELETE,
        s"/${entityTypeName}s/${auth.id}"
      )

      val result = controller.delete(auth.id.toString)(request)

      status(result) shouldBe PRECONDITION_FAILED
      contentAsJson(result) shouldBe Json.obj(
        "status" -> "KO",
        "message" -> s"The user associated with ${auth.id.toString} have to remain with at least one basic role, namely ${Roles.StudentLabel} or ${Roles.EmployeeLabel}"
      )
    }
  }
}