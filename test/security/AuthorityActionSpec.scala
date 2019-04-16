package security

import java.util.UUID

import auth.UserToken
import base.LwmFakeApplication
import controllers.helper.RequestOps
import dao.helper.DBResult.Created
import dao.{AuthorityDao, UserDao}
import database.UserDb
import models.Authority
import org.mockito.ArgumentMatchers._
import org.mockito.Mockito._
import org.scalatest.mockito.MockitoSugar
import org.scalatestplus.play.PlaySpec
import org.scalatestplus.play.guice.GuiceOneAppPerSuite
import play.api.inject.bind
import play.api.inject.guice.GuiceableModule
import play.api.libs.json.Json
import play.api.mvc._
import play.api.test.FakeRequest
import play.api.test.Helpers._
import database.helper.LdapUserStatus._

import scala.concurrent.Future

class AuthorityActionSpec extends PlaySpec with GuiceOneAppPerSuite with LwmFakeApplication with MockitoSugar with RequestOps {

  import Authority.writes

  val authDaoMock = mock[AuthorityDao]
  val userDaoMock = mock[UserDao]

  val authAction = app.injector.instanceOf(classOf[AuthorityAction])

  "A AuthorityActionSpec" should {
    "continue when authorities are found" in {
      val auth = Authority(UUID.randomUUID, UUID.randomUUID, Some(UUID.randomUUID), UUID.randomUUID)
      when(authDaoMock.authoritiesFor(anyString)).thenReturn(Future.successful(Seq(auth)))

      val result = authAction.invokeBlock(IdRequest(FakeRequest(), "systemId value"), (r: AuthRequest[_]) => Future.successful(Results.Ok(Json.toJson(r.authorities))))

      status(result) mustBe OK
      contentAsJson(result) mustBe Json.toJson(Seq(auth))
    }

    "create dedicated authorities for a student if there are none" in {
      val student = UserDb("systemId value", "last", "first", "mail", StudentStatus, Some("reg"), Some(UUID.randomUUID))
      val studentAuth = Authority(UUID.randomUUID, UUID.randomUUID, Some(UUID.randomUUID), UUID.randomUUID)
      val token = UserToken(UUID.randomUUID.toString, Set.empty, student.firstname, student.lastname, student.systemId, student.email, student.status.label, Some("abbrev"), student.registrationId)

      when(authDaoMock.authoritiesFor(student.systemId)).thenReturn(Future.successful(Seq.empty))
      when(userDaoMock.createOrUpdateWithBasicAuthority(anyString, anyString, anyString, anyString, anyString, Some(anyString), Some(anyString))).thenReturn(Future.successful(Created(student)))
      when(authDaoMock.authoritiesFor(student.systemId)).thenReturn(Future.successful(Seq(studentAuth)))

      val result = authAction.invokeBlock(IdRequest(FakeRequest().withUserToken(token), "systemId value"), (r: AuthRequest[_]) => Future.successful(Results.Ok(Json.toJson(r.authorities))))

      status(result) mustBe OK
      contentAsJson(result) mustBe Json.toJson(Seq(studentAuth))
    }

    "create dedicated authorities for a employee if there are none" in {
      val employee = UserDb("systemId value", "last", "first", "mail", EmployeeStatus, None, None)
      val employeeAuth = Authority(UUID.randomUUID, UUID.randomUUID, Some(UUID.randomUUID), UUID.randomUUID)
      val token = UserToken(UUID.randomUUID.toString, Set.empty, employee.firstname, employee.lastname, employee.systemId, employee.email, employee.status.label, None, None)

      when(authDaoMock.authoritiesFor(employee.systemId)).thenReturn(Future.successful(Seq.empty))
      when(userDaoMock.createOrUpdateWithBasicAuthority(anyString, anyString, anyString, anyString, anyString, Some(anyString), Some(anyString))).thenReturn(Future.successful(Created(employee)))
      when(authDaoMock.authoritiesFor(employee.systemId)).thenReturn(Future.successful(Seq(employeeAuth)))

      val result = authAction.invokeBlock(IdRequest(FakeRequest().withUserToken(token), "systemId value"), (r: AuthRequest[_]) => Future.successful(Results.Ok(Json.toJson(r.authorities))))

      status(result) mustBe OK
      contentAsJson(result) mustBe Json.toJson(Seq(employeeAuth))
    }

    "create dedicated authorities for a lecturer if there are none" in {
      val lecturer = UserDb("systemId value", "last", "first", "mail", LecturerStatus, None, None)
      val lecturerAuth = Authority(UUID.randomUUID, UUID.randomUUID, Some(UUID.randomUUID), UUID.randomUUID)
      val token = UserToken(UUID.randomUUID.toString, Set.empty, lecturer.firstname, lecturer.lastname, lecturer.systemId, lecturer.email, lecturer.status.label, None, None)

      when(authDaoMock.authoritiesFor(lecturer.systemId)).thenReturn(Future.successful(Seq.empty))
      when(userDaoMock.createOrUpdateWithBasicAuthority(anyString, anyString, anyString, anyString, anyString, Some(anyString), Some(anyString))).thenReturn(Future.successful(Created(lecturer)))
      when(authDaoMock.authoritiesFor(lecturer.systemId)).thenReturn(Future.successful(Seq(lecturerAuth)))

      val result = authAction.invokeBlock(IdRequest(FakeRequest().withUserToken(token), "systemId value"), (r: AuthRequest[_]) => Future.successful(Results.Ok(Json.toJson(r.authorities))))

      status(result) mustBe OK
      contentAsJson(result) mustBe Json.toJson(Seq(lecturerAuth))
    }

    "fail when an exception is thrown" in {
      when(authDaoMock.authoritiesFor(anyString)).thenReturn(Future.failed(new Throwable("some message")))

      val result = authAction.invokeBlock(IdRequest(FakeRequest(), "systemId value"), (r: AuthRequest[_]) => Future.successful(Results.Ok(Json.toJson(r.authorities))))

      status(result) mustBe INTERNAL_SERVER_ERROR
      contentAsString(result).contains("some message") mustBe true
    }

    "fail when an user-token is missing" in {
      when(authDaoMock.authoritiesFor(anyString)).thenReturn(Future.successful(Seq.empty))

      val result = authAction.invokeBlock(IdRequest(FakeRequest(), "systemId value"), (r: AuthRequest[_]) => Future.successful(Results.Ok(Json.toJson(r.authorities))))

      status(result) mustBe INTERNAL_SERVER_ERROR
      contentAsString(result).contains("no userToken found") mustBe true
    }
  }

  override protected def bindings: Seq[GuiceableModule] = Seq(
    bind(classOf[AuthorityDao]).toInstance(authDaoMock),
    bind(classOf[UserDao]).toInstance(userDaoMock)
  )
}
