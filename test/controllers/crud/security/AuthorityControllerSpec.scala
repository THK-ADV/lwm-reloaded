package controllers.crud.security

import java.util.UUID

import controllers.crud.AbstractCRUDControllerSpec
import controllers.security.AuthorityController
import models.security.Permissions._
import models.security._
import models.users.{Employee, Student}
import models.{Course, CourseAtom}
import org.w3.banana.PointedGraph
import org.w3.banana.sesame.Sesame
import play.api.libs.json.{JsArray, JsValue, Json, Writes}
import play.api.test.FakeRequest
import play.api.test.Helpers._
import store.SesameRepository
import utils.LwmMimeType

class AuthorityControllerSpec extends AbstractCRUDControllerSpec[AuthorityProtocol, Authority, AuthorityAtom] {

  override def entityTypeName: String = "authority"

  override val controller: AuthorityController = new AuthorityController(repository, sessionService, namespace, roleService) {

    override protected def fromInput(input: AuthorityProtocol, existing: Option[Authority]): Authority = entityToPass

    override protected def contextFrom: PartialFunction[Rule, SecureContext] = {
      case _ => NonSecureBlock
    }
  }

  val studentToPass = Student("systemId to pass", "last name to pass", "first name to pass", "email to pass", "regId to pass", UUID.randomUUID())
  val employeeToPass = Employee("systemId to pass", "last name to pass", "first name to pass", "email to pass", "status to pass")
  val employeeToFail = Employee("systemId to fail", "last name to fail", "first name to fail", "email to fail", "status to fail")

  val refRoleIdToPass1 = RefRole.randomUUID
  val refRoleIdToPass2 = RefRole.randomUUID
  val refRoleIdToFail1 = RefRole.randomUUID
  val refRoleIdToFail2 = RefRole.randomUUID

  val courseToPass = Course("Course2", "Description", "Abbrev", employeeToPass.id, 0, Course.randomUUID)
  val courseToFail = Course("Course1", "Description", "Abbrev", employeeToFail.id, 0, Course.randomUUID)

  val courseAtomToPass = CourseAtom(courseToPass.label, courseToPass.description, courseToPass.abbreviation, employeeToPass, courseToPass.semesterIndex, courseToPass.id)
  val courseAtomToFail = CourseAtom(courseToFail.label, courseToFail.description, courseToFail.abbreviation, employeeToFail, courseToFail.semesterIndex, courseToFail.id)

  val role1 = Role("role1", Set(user.get, user.getAll))
  val role2 = Role("role2", Set(course.get, course.create, course.getAll))
  val role3 = Role("role3", Set(degree.get, degree.getAll))
  val role4 = Role("role4", Set(authority.get, authority.getAll))

  val refRolesAtomicToPass = Set(
    RefRoleAtom(None, role1, refRoleIdToPass1),
    RefRoleAtom(Some(courseAtomToPass), role2, refRoleIdToPass2)
  )

  val refRolesAtomicToFail = Set(
    RefRoleAtom(None, role3, refRoleIdToFail1),
    RefRoleAtom(Some(courseAtomToFail), role4, refRoleIdToFail2)
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

  override val atomizedEntityToPass = AuthorityAtom(
    studentToPass,
    refRolesAtomicToPass,
    entityToPass.id
  )

  override val atomizedEntityToFail = AuthorityAtom(
    employeeToFail,
    refRolesAtomicToFail,
    entityToFail.id
  )


  import bindings.AuthorityDescriptor
  import ops._

  implicit val authorityBinder = AuthorityDescriptor.binder
  override val pointedGraph: PointedGraph[Sesame] = entityToPass.toPG

  override implicit val jsonWrites: Writes[Authority] = Authority.writes

  override implicit def jsonWritesAtom: Writes[AuthorityAtom] = Authority.writesAtom

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

    "filter through nested levels of graphs" in {
      import bindings.{
      AuthorityDescriptor,
      RefRoleDescriptor}

      val realRepo = SesameRepository(namespace)

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
}