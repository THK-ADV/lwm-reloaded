package controllers

import java.util.UUID

import base.TestBaseDefinition
import models.{Login, Session}
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

  val loginToPass = Login("student1", "abcde123")
  val loginToFail = Login("student1", "blabla")
  val mimeType = LwmMimeType.loginV1Json

  val validUuid = UUID.randomUUID()
  val invalidUuid = UUID.randomUUID()

  val invalidCredentialException = new RuntimeException("Invalid Credentials")

  val sessionRepository: SessionRepositoryModule = new SessionRepositoryModule {

    val service = mock[SessionHandlingService]

    when(service.newSession(loginToPass.username, loginToPass.password)).thenReturn(Future.successful(Session(loginToPass.username, validUuid)))
    when(service.newSession(loginToFail.username, loginToFail.password)).thenReturn(Future.failed(invalidCredentialException))
    when(service.isValid(validUuid)).thenReturn(Future.successful(true))
    when(service.deleteSession(validUuid)).thenReturn(Future.successful(true))
    when(service.deleteSession(invalidUuid)).thenReturn(Future.successful(false))


    override def sessionService: SessionHandlingService = service
  }

  val controller = new SessionController(sessionRepository.sessionService)

  class WithDepsApplication extends WithApplicationLoader(new ApplicationLoader {
    override def load(context: Context): Application = new DefaultLwmApplication(context) {
      override def sessionController: SessionController = controller
    }.application
  })


  "A SessionControllerSpec " should {
    "allow students to log in" in new WithDepsApplication {
      val json = Json.obj(
        "username" -> loginToPass.username,
        "password" -> loginToPass.password
      )

      val result = route(FakeRequest(
        POST,
        "/sessions",
        FakeHeaders(Seq(HeaderNames.CONTENT_TYPE -> mimeType)),
        json
      )).get

      status(result) shouldBe OK
      contentType(result) shouldBe Some[String](mimeType)
      session(result).data.keys should contain (SessionController.sessionId)
      session(result).data.keys should contain (SessionController.userId)
      session(result).data.keys should contain (Security.username)
    }

    "allow students to log in with proper username resolving" in new WithDepsApplication {
      val json = Json.obj(
        "username" -> loginToPass.username,
        "password" -> loginToPass.password
      )

      val result = route(FakeRequest(
        POST,
        "/sessions",
        FakeHeaders(Seq(HeaderNames.CONTENT_TYPE -> mimeType)),
        json
      )).get

      status(result) shouldBe OK
      contentType(result) shouldBe Some[String](mimeType)

      session(result).data.keys should contain (SessionController.userId)
      session(result).data.get(SessionController.userId) shouldBe Some(validUuid.toString)

      session(result).data.keys should contain (Security.username)
      session(result).data.get(Security.username) shouldBe Some(loginToPass.username)
    }

    "not allow students to log in with invalid mimeType" in new WithDepsApplication {
      val json = Json.obj(
        "username" -> loginToPass.username,
        "password" -> loginToPass.password
      )

      val result = route(FakeRequest(
        POST,
        "/sessions",
        FakeHeaders(Seq(HeaderNames.CONTENT_TYPE -> "application/json")),
        json
      )).get

      status(result) shouldBe UNSUPPORTED_MEDIA_TYPE
      contentType(result) shouldBe Some("text/html")
      contentAsString(result) should include (s"Expecting ${LwmMimeType.loginV1Json.value} body")
    }

    "not allow students to log in with invalid credentials" in new WithDepsApplication {
      import scala.concurrent.ExecutionContext.Implicits.global

      val json = Json.obj(
        "username" -> loginToFail.username,
        "password" -> loginToFail.password
      )

      val result = route(FakeRequest(
        POST,
        "/sessions",
        FakeHeaders(Seq(HeaderNames.CONTENT_TYPE -> mimeType)),
        json
      )).get

      result onFailure {
        case e => e shouldEqual invalidCredentialException
      }
    }

    "not allow students to log in with invalid json" in new WithDepsApplication {
      val invalidJson = Json.obj(
        "name" -> loginToFail.username,
        "secret" -> loginToFail.password
      )

      val result = route(FakeRequest(
        POST,
        "/sessions",
        FakeHeaders(Seq(HeaderNames.CONTENT_TYPE -> mimeType)),
        invalidJson
      )).get

      status(result) shouldBe BAD_REQUEST
      contentType(result) shouldBe Some("application/json")
      session(result).data.keys should not contain SessionController.sessionId
      contentAsString(result) should include("KO")
      contentAsString(result) should include("errors")
    }

    "allow students to successfully log out" in new WithDepsApplication {
      val request = FakeRequest(
        DELETE,
        "/sessions"
      ).withSession(SessionController.sessionId -> validUuid.toString, Security.username -> loginToPass.username)

      val result = route(request).get

      status(result) shouldBe OK
      session(result).data.keys should not contain SessionController.sessionId
    }

    "not allow students to successfully log out when there is no session match" in new WithDepsApplication {
      val request = FakeRequest(
        DELETE,
        "/sessions"
      ).withSession(SessionController.sessionId -> invalidUuid.toString, Security.username -> loginToFail.username)

      val result = route(request).get

      status(result) shouldBe BAD_REQUEST
    }

    "should return the expected content type" in new WithDepsApplication {
      val result = route(FakeRequest(
        HEAD,
        "/sessions"
      )).get

      status(result) shouldBe NO_CONTENT
      contentType(result) shouldBe Some[String](LwmMimeType.loginV1Json)
      contentAsString(result) shouldBe empty
    }
  }
}