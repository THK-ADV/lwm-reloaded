package controllers

import akka.util.Timeout
import org.scalatestplus.play._
import play.api.{Play, Application}
import play.api.http.HeaderNames
import play.api.libs.concurrent.Akka
import play.api.libs.json.{JsString, Json}
import play.api.test.Helpers._
import play.api.test._
import org.mockito.Matchers._
import org.mockito.Mockito
import org.mockito.Mockito._
import org.scalatest.mock.MockitoSugar.mock
import utils.{SessionHandler, GlobalDef, Authenticator}

import scala.concurrent.Future
import scala.concurrent.duration._

class SessionManagementSpec extends PlaySpec with OneAppPerSuite {
  import Play.current

  val authenticator = mock[Authenticator]
  when(authenticator.authenticate("student1", "abcde123")).thenReturn(Future.successful(true))
  when(authenticator.authenticate("student1", "blabla")).thenReturn(Future.successful(false))

  implicit override lazy val app: FakeApplication = FakeApplication(withGlobal = Some(new GlobalDef {
    override implicit def timeout: Timeout = 1.minute

    override def onStart(app: Application): Unit = {
      Akka.system.actorOf(SessionHandler.props(authenticator, timeout.duration), "sessions")
    }
  }))

  "Login action" must {

    "allow students to log in and log out" in {

      val json = Json.obj(
        "username" -> JsString("student1"),
        "password" -> JsString("abcde123")
      )

      val loginRequest = FakeRequest(
        Helpers.POST,
        routes.SessionManagementController.login().url,
        FakeHeaders(Seq(HeaderNames.CONTENT_TYPE -> Seq("application/json"))),
        json
      )

      val loginResult = controllers.SessionManagementController.login()(loginRequest)

      status(loginResult) mustBe OK
      contentType(loginResult) mustBe Some("text/plain")
      contentAsString(loginResult) must include("Session created")

      val logoutRequest = FakeRequest(
        Helpers.DELETE,
        routes.SessionManagementController.logout().url
      )

      val logoutResult = controllers.SessionManagementController.logout()(logoutRequest)

      status(logoutResult) mustBe OK
      contentType(logoutResult) mustBe Some("text/plain")
      contentAsString(logoutResult) must include("Session removed")
    }
    /*
    "should time-out when IDM not available" in {
      val json = Json.obj(
        "username" -> JsString("student1"),
        "password" -> JsString("abcde123")
      )

      val loginRequest = FakeRequest(
        Helpers.POST,
        routes.SessionManagementController.login().url,
        FakeHeaders(Seq(HeaderNames.CONTENT_TYPE -> Seq("application/json"))),
        json
      )

      val loginResult = controllers.SessionManagementController.login()(loginRequest)

      status(loginResult) mustBe INTERNAL_SERVER_ERROR
      contentType(loginResult) mustBe Some("text/plain")
      contentAsString(loginResult) must include("No response from IDM")
    }
    */
    "deny non-members log in" in {
      val json = Json.obj(
        "username" -> JsString("student1"),
        "password" -> JsString("blabla")
      )

      val request = FakeRequest(
        Helpers.POST,
        routes.SessionManagementController.login().url,
        FakeHeaders(Seq(HeaderNames.CONTENT_TYPE -> Seq("application/json"))),
        json
      )

      val result = controllers.SessionManagementController.login()(request)

      status(result) mustBe UNAUTHORIZED
      contentType(result) mustBe Some("text/plain")
      contentAsString(result) must include("invalid credentials")
    }


  }

}