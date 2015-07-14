package controllers

import java.util.UUID

import base.TestBaseDefinition
import models.Session
import modules._
import org.mockito.Mockito._
import org.scalatest.WordSpec
import org.scalatest.mock.MockitoSugar.mock
import org.scalatestplus.play.OneAppPerSuite
import play.api.ApplicationLoader.Context
import play.api.http.HeaderNames
import play.api.libs.json.Json
import play.api.mvc.Security
import play.api.test.Helpers._
import play.api.test._
import play.api.{http, Application, ApplicationLoader}
import services.SessionHandlingService
import utils._

import scala.concurrent.Future


class SessionControllerSpec extends WordSpec with TestBaseDefinition  {
  val uuid = UUID.randomUUID()
  val uuid2 = UUID.randomUUID()

  val sessionRepository: SessionRepositoryModule = new SessionRepositoryModule {

    val service = mock[SessionHandlingService]

    when(service.newSession("student1", "abcde123")).thenReturn(Future.successful(Session("student1")))
    when(service.newSession("student1", "blabla")).thenReturn(Future.failed(new RuntimeException("Invalid Credentials")))
    when(service.isValid(uuid)).thenReturn(Future.successful(true))
    when(service.deleteSession(uuid)).thenReturn(Future.successful(true))
    when(service.deleteSession(uuid2)).thenReturn(Future.successful(false))


    override def sessionService: SessionHandlingService = service
  }

  val controller = new SessionController(sessionRepository.sessionService)

  class WithDepsApplication extends WithApplicationLoader(new ApplicationLoader {
    override def load(context: Context): Application = new DefaultLwmApplication(context) {
      override def sessionController: SessionController = controller
    }.application
  })


  "A SessionControllerSpec " should {
    "allow students to log in and log out" in new WithDepsApplication {
      val json = Json.obj(
        "username" -> "student1",
        "password" -> "abcde123"
      )

      val result = route(FakeRequest(
        POST,
        "/sessions",
        FakeHeaders(Seq(HeaderNames.CONTENT_TYPE -> http.MimeTypes.JSON)),
        json
      )).get

      status(result) shouldBe OK
      session(result).data.keys should contain ("session-id")
    }

    "not allow students to log in with invalid credentials" in new WithDepsApplication {
      val invalidJson = Json.obj(
        "name" -> "student1",
        "secret" -> "blabla"
      )

      val result =  route(FakeRequest(
        POST,
        "/sessions",
        FakeHeaders(Seq(HeaderNames.CONTENT_TYPE -> http.MimeTypes.JSON)),
        invalidJson
      )).get

      status(result) shouldBe BAD_REQUEST
      contentType(result) shouldBe Some("application/json")
      session(result).data.keys should not contain "session-id"
      contentAsString(result) should include("KO")
      contentAsString(result) should include("errors")
    }

    "allow students to successfully log out" in new WithDepsApplication {
      val request = FakeRequest(
        DELETE,
        "/sessions"
      ).withSession(SessionController.sessionId -> uuid.toString, Security.username -> "student1")

      val result =  route(request).get

      status(result) shouldBe OK
      session(result).data.keys should not contain "session-id"
    }

    "not allow students to successfully log out when there is no session match" in new WithDepsApplication {


      val request = FakeRequest(
        DELETE,
        "/sessions"
      ).withSession(SessionController.sessionId -> uuid2.toString, Security.username -> "student1")

      val result =  route(request).get

      status(result) shouldBe BAD_REQUEST
    }

  }
}
