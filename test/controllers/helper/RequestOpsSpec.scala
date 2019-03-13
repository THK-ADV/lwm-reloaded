package controllers.helper

import auth.UserToken
import base.TestBaseDefinition
import org.scalatest.WordSpec
import play.api.test.FakeRequest

class RequestOpsSpec extends WordSpec with TestBaseDefinition with RequestOps {

  "A RequestOpsSpec" should {
    "build query strings by using paths" in {
      val fake = FakeRequest("GET", "/fake?existing=yeah&more=yes")

      fake.queryString shouldBe Map(
        "existing" -> Seq("yeah"),
        "more" -> Seq("yes")
      )
    }

    "append params to empty query string" in {
      val fake = FakeRequest("GET", "/fake")
        .appending("key" -> Seq("value"), "second" -> Seq("second_value"))
        .appending("yet_another" -> Seq("value2"))

      fake.queryString shouldBe Map(
        "key" -> Seq("value"),
        "second" -> Seq("second_value"),
        "yet_another" -> Seq("value2")
      )
    }

    "append params to existing query string" in {
      val fake = FakeRequest("GET", "/fake?existing=yeah")
        .appending("key" -> Seq("value"), "second" -> Seq("second_value"))
        .appending("yet_another" -> Seq("value2"))

      fake.queryString shouldBe Map(
        "existing" -> Seq("yeah"),
        "key" -> Seq("value"),
        "second" -> Seq("second_value"),
        "yet_another" -> Seq("value2")
      )
    }

    "override query strings" in {
      val fake = FakeRequest("GET", "/fake?existing=yeah&more=yes")
      val overridden = fake.overrideQueryString(Map("override" -> Seq("value")))

      overridden.queryString shouldBe Map("override" -> Seq("value"))
    }

    "extract user token if present" in {
      val fake = FakeRequest("GET", "/")
      val token = UserToken("id", Set.empty, "first", "last", "systemId value", "email", "status", Some("abbreviation"), Some("regId"))
      val updated = fake.addAttr(RequestOps.UserToken, token)

      updated.userToken.value shouldBe token
      updated.systemId.value shouldBe "systemId value"
    }

    "not extract user token if absent" in {
      val fake = FakeRequest("GET", "/")

      fake.userToken shouldBe None
      fake.systemId shouldBe None
    }
  }
}
