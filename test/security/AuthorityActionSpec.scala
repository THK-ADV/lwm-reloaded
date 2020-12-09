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

    "fail with status code 'Conflict' if no authorities are found" in {
      val systemId = "inf123"
      when(authDaoMock.authoritiesFor(systemId)).thenReturn(Future.successful(Seq.empty))

      val result = authAction.invokeBlock(IdRequest(FakeRequest(), systemId), (r: AuthRequest[_]) => Future.successful(Results.Conflict))

      status(result) mustBe CONFLICT
      contentAsJson(result) mustBe Json.obj("status" -> "KO", "message" -> s"no user found with systemId $systemId")
    }

    "fail when an exception is thrown" in {
      when(authDaoMock.authoritiesFor(anyString)).thenReturn(Future.failed(new Throwable("some message")))

      val result = authAction.invokeBlock(IdRequest(FakeRequest(), "systemId value"), (r: AuthRequest[_]) => Future.successful(Results.Ok(Json.toJson(r.authorities))))

      status(result) mustBe INTERNAL_SERVER_ERROR
      contentAsString(result).contains("some message") mustBe true
    }
  }

  override protected def bindings: Seq[GuiceableModule] = Seq(
    bind(classOf[AuthorityDao]).toInstance(authDaoMock),
    bind(classOf[UserDao]).toInstance(userDaoMock)
  )
}
