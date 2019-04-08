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

    "extract bearer token from request when present, regardless of capitalization" in {
      val request1 = FakeRequest().withHeaders(("Authorization", "Bearer RANDOM_HASH"))
      val maybeToken1 = auth.bearerToken(request1)
      val request2 = FakeRequest().withHeaders(("Authorization", "bearer random_hash"))
      val maybeToken2 = auth.bearerToken(request2)

      maybeToken1.value shouldBe "RANDOM_HASH"
      maybeToken2.value shouldBe "random_hash"
    }

    "not extract bearer token from request with wrong header configurations" in {
      auth.bearerToken(FakeRequest().withHeaders()) shouldBe None
      auth.bearerToken(FakeRequest().withHeaders(("fake", "fake"))) shouldBe None
      auth.bearerToken(FakeRequest().withHeaders(("Authorization", "fake"))) shouldBe None
      auth.bearerToken(FakeRequest().withHeaders(("Authorization", "bearer"))) shouldBe None
      auth.bearerToken(FakeRequest().withHeaders(("Authorization", "bearer "))) shouldBe None
    }
  }

  override protected def bindings: Seq[GuiceableModule] = Seq.empty
}
