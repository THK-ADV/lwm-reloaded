package controllers.security

import java.util.UUID

import base.TestBaseDefinition
import controllers.SessionController
import models.security.{Authority, Permissions, RefRole, Roles}
import models.semester.Semester
import org.joda.time.LocalDate
import org.scalatest.WordSpecLike
import play.api.http.HeaderNames
import play.api.libs.json.Json
import play.api.{Application, ApplicationLoader}
import play.api.ApplicationLoader.Context
import play.api.test.{FakeHeaders, FakeRequest, WithApplicationLoader}
import services.RoleService
import utils.{DefaultLwmApplication, LwmMimeType}
import org.mockito.Mockito._
import org.scalatest.mock.MockitoSugar.mock
import play.api.test.Helpers._

import scala.util.Success


class SemesterSecuritySpec extends WordSpecLike with TestBaseDefinition {
  self =>

  val mimeType = LwmMimeType.semesterV1Json
  val roleService = mock[RoleService]

  class FakeApplication extends WithApplicationLoader(new ApplicationLoader {
    override def load(context: Context): Application = new DefaultLwmApplication(context) {
      override lazy val roleService: RoleService = self.roleService
    }.application
  })

  "Semester security " should {

    "Allow invocations when user is admin" in new FakeApplication() {
      val user = UUID.randomUUID()
      val userRefRole = RefRole(None, Roles.admin.id)
      val userAuth = Authority(user, Set(userRefRole.id))

      when(roleService.authorityFor(user.toString)).thenReturn(Success(Some(userAuth)))
      when(roleService.checkWith((None, Set(Permissions.prime)))(userAuth)).thenReturn(Success(true))

      val semester = Semester("label", "abbrev", LocalDate.now, LocalDate.now, LocalDate.now, Semester.randomUUID)
      val json = Json.toJson(semester)

      val request = FakeRequest(
        "POST",
        "/semesters",
        FakeHeaders(Seq(HeaderNames.CONTENT_TYPE -> mimeType)),
        json
      ).withSession(SessionController.userId -> user.toString)

      val result = route(request).get

      status(result) shouldBe CREATED
    }

    "Block invocations when user not an admin" in new FakeApplication() {
      val user = UUID.randomUUID()
      val userRefRole = RefRole(None, Roles.employee.id)
      val userAuth = Authority(user, Set(userRefRole.id))

      when(roleService.authorityFor(user.toString)).thenReturn(Success(Some(userAuth)))
      when(roleService.checkWith((None, Set(Permissions.prime)))(userAuth)).thenReturn(Success(false))

      val semester = Semester("label", "abbrev", LocalDate.now, LocalDate.now, LocalDate.now, Semester.randomUUID)
      val json = Json.toJson(semester)

      val request = FakeRequest(
        "POST",
        "/semesters",
        FakeHeaders(Seq(HeaderNames.CONTENT_TYPE -> mimeType)),
        json
      ).withSession(SessionController.userId -> user.toString)

      val result = route(request).get

      status(result) shouldBe UNAUTHORIZED
    }

    "Allow invocations when employee wants to get all semesters" in new FakeApplication() {
      val user = UUID.randomUUID()
      val userRefRole = RefRole(None, Roles.employee.id)
      val userAuth = Authority(user, Set(userRefRole.id))

      when(roleService.authorityFor(user.toString)).thenReturn(Success(Some(userAuth)))
      when(roleService.checkWith((None, Set(Permissions.allSemesters)))(userAuth)).thenReturn(Success(true))

      val request = FakeRequest(
        "GET",
        "/semesters"
      ).withSession(SessionController.userId -> user.toString)

      val result = route(request).get

      status(result) shouldBe OK
    }

    "Allow invocations when student wants to get all semesters" in new FakeApplication() {
      val user = UUID.randomUUID()
      val userRefRole = RefRole(None, Roles.student.id)
      val userAuth = Authority(user, Set(userRefRole.id))

      when(roleService.authorityFor(user.toString)).thenReturn(Success(Some(userAuth)))
      when(roleService.checkWith((None, Set(Permissions.allSemesters)))(userAuth)).thenReturn(Success(false))

      val request = FakeRequest(
        "GET",
        "/semesters"
      ).withSession(SessionController.userId -> user.toString)

      val result = route(request).get

      status(result) shouldBe UNAUTHORIZED
    }
  }
}
