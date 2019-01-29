package auth

import base.{LwmFakeApplication, TestBaseDefinition}
import org.scalatest.WordSpec
import org.scalatest.mockito.MockitoSugar
import org.scalatest.time.{Seconds, Span}
import org.scalatestplus.play.guice.GuiceOneAppPerSuite
import play.api.inject.guice.GuiceableModule
import play.api.test.FakeRequest

class KeycloakAuthorizationSpec extends WordSpec with TestBaseDefinition with GuiceOneAppPerSuite with LwmFakeApplication with MockitoSugar {

  val auth = app.injector.instanceOf(classOf[OAuthAuthorization])

  "A KeycloakAuthorizationSpec" should {
    "extract bearer token from request when present" in {
      val request = FakeRequest().withHeaders(("Authorization", "Bearer RANDOM_HASH"))
      val maybeToken = auth.bearerToken(request)

      maybeToken.value shouldBe "RANDOM_HASH"
    }

    "not extract bearer token from request with wrong header configurations" in {
      auth.bearerToken(FakeRequest().withHeaders()) shouldBe None
      auth.bearerToken(FakeRequest().withHeaders(("fake", "fake"))) shouldBe None
      auth.bearerToken(FakeRequest().withHeaders(("Authorization", "fake"))) shouldBe None
    }

    "not verify token when public keys are not matching" in {
      val result = auth.verifyToken("eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9.eyJzdWIiOiIxMjM0NTY3ODkwIiwibmFtZSI6IkpvaG4gRG9lIiwiaWF0IjoxNTE2MjM5MDIyfQ.SflKxwRJSMeKKF2QT4fwpMeJf36POk6yJV_adQssw5c")
      whenReady(result.failed, timeout(Span(5, Seconds)))(_.getLocalizedMessage shouldBe "No matching public key found")
    }
  }

  override protected def bindings: Seq[GuiceableModule] = Seq.empty
}
