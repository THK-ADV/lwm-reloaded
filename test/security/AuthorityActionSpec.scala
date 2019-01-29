package security

import java.util.UUID

import base.LwmFakeApplication
import dao.AuthorityDao
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

import scala.concurrent.Future

class AuthorityActionSpec extends PlaySpec with GuiceOneAppPerSuite with LwmFakeApplication with MockitoSugar {
  import Authority.writes

  val authDaoMock = mock[AuthorityDao]

  val authAction = app.injector.instanceOf(classOf[AuthorityAction])

  "A AuthorityActionSpec" should {
    "continue when authorities are found" in {
      val auth = Authority(UUID.randomUUID, UUID.randomUUID, Some(UUID.randomUUID), UUID.randomUUID)
      when(authDaoMock.authoritiesFor(anyString)).thenReturn(Future.successful(Seq(auth)))

      val result = authAction.invokeBlock(IdRequest(FakeRequest(), "systemId value"), (r: AuthRequest[_]) => Future.successful(Results.Ok(Json.toJson(r.authorities))))

      status(result) mustBe OK
      contentAsJson(result) mustBe Json.toJson(Seq(auth))
    }

    "fail when there are no authorities for the user" in {
      when(authDaoMock.authoritiesFor(anyString)).thenReturn(Future.successful(Seq.empty))

      val result = authAction.invokeBlock(IdRequest(FakeRequest(), "systemId value"), (r: AuthRequest[_]) => Future.successful(Results.Ok(Json.toJson(r.authorities))))

      status(result) mustBe UNAUTHORIZED
      contentAsString(result).contains("No authority found for systemId value") mustBe true
    }

    "fail when an exception is thrown" in {
      when(authDaoMock.authoritiesFor(anyString)).thenReturn(Future.failed(new Throwable("some message")))

      val result = authAction.invokeBlock(IdRequest(FakeRequest(), "systemId value"), (r: AuthRequest[_]) => Future.successful(Results.Ok(Json.toJson(r.authorities))))

      status(result) mustBe INTERNAL_SERVER_ERROR
      contentAsString(result).contains("some message") mustBe true
    }
  }

  override protected def bindings: Seq[GuiceableModule] = Seq(
    bind(classOf[AuthorityDao]).toInstance(authDaoMock)
  )
}
