package controllers

import play.api.Play
import play.api.http.HeaderNames
import play.api.libs.json.{JsString, Json}
import play.api.test._
import play.api.test.Helpers._
import org.scalatestplus.play._

class SessionManagementSpec extends PlaySpec with OneAppPerSuite {

  import Play.current

  implicit override lazy val app: FakeApplication = FakeApplication(withGlobal = Some(utils.Global))

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