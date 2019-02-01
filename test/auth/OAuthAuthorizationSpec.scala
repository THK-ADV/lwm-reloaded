package auth

import base.{LwmFakeApplication, TestBaseDefinition}
import org.scalatest.WordSpec
import org.scalatest.mockito.MockitoSugar
import org.scalatestplus.play.guice.GuiceOneAppPerSuite
import play.api.inject.guice.GuiceableModule
import play.api.test.FakeRequest

class OAuthAuthorizationSpec extends WordSpec with TestBaseDefinition with GuiceOneAppPerSuite with LwmFakeApplication with MockitoSugar {

  val auth = app.injector.instanceOf(classOf[OAuthAuthorization])

  "A OAuthAuthorization" should {

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
  }

  override protected def bindings: Seq[GuiceableModule] = Seq.empty
}
