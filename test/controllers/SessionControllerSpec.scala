package controllers

import base.TestBaseDefinition
import modules.{AkkaActorSystemModule, SessionRepositoryModule}
import org.mockito.Mockito._
import org.scalatest.WordSpec
import org.scalatest.mock.MockitoSugar.mock
import play.api.http.HeaderNames
import play.api.libs.json.Json
import play.api.mvc.Security
import play.api.test.Helpers._
import play.api.test.{FakeApplication, FakeHeaders, FakeRequest}
import services.{ActorBasedSessionService, SessionHandlingService}
import utils.Authenticator

import scala.concurrent.Future

class SessionControllerSpec extends WordSpec with TestBaseDefinition {

  val sessionRepository: SessionRepositoryModule = new SessionRepositoryModule with AkkaActorSystemModule {

    val authenticator: Authenticator = mock[Authenticator]

    when(authenticator.authenticate("student1", "abcde123")).thenReturn(Future.successful(true))
    when(authenticator.authenticate("student1", "blabla")).thenReturn(Future.successful(false))
    when(authenticator.authenticate("foo", "bar")).thenReturn(Future.successful(false))

    override def sessionService: SessionHandlingService = new ActorBasedSessionService(system, authenticator)
  }

  val controller = new SessionController(sessionRepository.sessionService)

  "A SessionControllerSpec " should {
    "allow students to log in and log out" in {
      val json = Json.obj(
        "username" -> "student1",
        "password" -> "abcde123"
      )

      val loginRequest = FakeRequest(
        POST,
        "/sessions",
        FakeHeaders(Seq(HeaderNames.CONTENT_TYPE -> "application/json")),
        json
      )

      val result = controller.login()(loginRequest)

      status(result) shouldBe OK
      contentType(result) shouldBe Some("application/json")
      //contentAsString(result) shouldBe errorMessage
    }

    "not allow students to log in when there is a validation error" in {
      val invalidJson = Json.obj(
        "name" -> "foo",
        "secret" -> "bar"
      )

      val loginRequest = FakeRequest(
        POST,
        "/sessions",
        FakeHeaders(Seq(HeaderNames.CONTENT_TYPE -> "application/json")),
        invalidJson
      )

      val result = controller.login()(loginRequest)

      status(result) shouldBe BAD_REQUEST
      contentType(result) shouldBe Some("application/json")
      contentAsString(result) should include("KO")
      contentAsString(result) should include("errors")
    }

    "not allow students to log in when they have invalid credentials" in {
      val errorMessage = s"""{"status":"KO","errors":"Invalid Credentials"}"""

      val json = Json.obj(
        "username" -> "student1",
        "password" -> "blabla"
      )

      val loginRequest = FakeRequest(
        POST,
        "/sessions",
        FakeHeaders(Seq(HeaderNames.CONTENT_TYPE -> "application/json")),
        json
      )

      val result = controller.login()(loginRequest)

      status(result) shouldBe UNAUTHORIZED
      contentType(result) shouldBe Some("application/json")
      contentAsString(result) shouldBe errorMessage
    }

    "allow students to successfully log out" in {
      val json = Json.obj(
        "username" -> "student1",
        "password" -> "abcde123"
      )

      val request = FakeRequest(
        DELETE,
        "/sessions",
        FakeHeaders(Seq(HeaderNames.CONTENT_TYPE -> "application/json")),
        json
      ).withSession(SessionController.sessionId -> "1", Security.username -> "student1")

      val result = controller.logout()(request).run

      status(result) shouldBe OK
    }

    "not allow students to successfully log out when there is no session match" in {
      val json = Json.obj(
        "username" -> "student1",
        "password" -> "abcde123"
      )

      val request = FakeRequest(
        DELETE,
        "/sessions",
        FakeHeaders(Seq(HeaderNames.CONTENT_TYPE -> "application/json")),
        json
      ).withSession(SessionController.sessionId -> "fd500d9e-04a8-453b-aad6-e70395432494", Security.username -> "student1")

      val result = controller.logout()(request).run

      status(result) shouldBe BAD_REQUEST
    }
  }
}