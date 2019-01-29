package security

import auth.{OAuthAuthorization, UserToken}
import base.LwmFakeApplication
import org.mockito.ArgumentMatchers._
import org.mockito.Mockito._
import org.scalatest.mockito.MockitoSugar
import org.scalatestplus.play.PlaySpec
import org.scalatestplus.play.guice.GuiceOneAppPerSuite
import play.api.inject.bind
import play.api.mvc._
import play.api.test.Helpers._
import play.api.test._

import scala.concurrent.Future

class AuthorizationActionSpec extends PlaySpec with GuiceOneAppPerSuite with LwmFakeApplication with MockitoSugar {

  val authMock = mock[OAuthAuthorization]

  val authAction = app.injector.instanceOf(classOf[AuthorizationAction])

  "A AuthorizationActionSpec" should {

    "pass user authorization" in {
      when(authMock.authorized(any)).thenReturn(Future.successful(UserToken("id", Set.empty, "first", "last", "systemId value", "email", "status", Some("abbreviation"), Some("regId"))))

      val action = authAction(r => Results.Ok(r.systemId))
      val result = call(action, FakeRequest())

      status(result) mustEqual OK
      contentAsString(result) mustEqual "systemId value"
    }

    "return unauthorized when authorization fails" in {
      when(authMock.authorized(any)).thenReturn(Future.failed(new Throwable("some message")))

      val action = authAction(r => Results.Ok(r.systemId))
      val result = call(action, FakeRequest())

      status(result) mustEqual UNAUTHORIZED
      contentAsString(result).contains("some message") mustBe true
    }
  }

  override def bindings = Seq(
    bind(classOf[OAuthAuthorization]).toInstance(authMock)
  )
}
