package utils

import akka.stream.Materializer
import org.scalatestplus.play.PlaySpec
import org.scalatestplus.play.guice.GuiceOneAppPerSuite
import play.api.inject.guice.GuiceApplicationBuilder
import play.api.libs.json.Json
import play.api.mvc._
import play.api.test._
import play.api.test.Helpers._
import play.test.WithApplication

class SecuredActionSpec extends PlaySpec with GuiceOneAppPerSuite {


  "An essential action" should {
    "can parse a JSON body" in {

      val action: EssentialAction = Action { request =>
        val value = (request.body.asJson.get \ "field").as[String]
        Results.Ok(value)
      }

      implicit val x = app.materializer

      val request = FakeRequest(POST, "/").withJsonBody(Json.parse("""{ "field": "value" }"""))

      val result = call(action, request)

      status(result) mustEqual OK
      contentAsString(result) mustEqual "value"
    }
  }
}
