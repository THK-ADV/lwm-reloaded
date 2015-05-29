package controllers

import akka.util.Timeout
import models.Degree
import org.mockito.Mockito._
import org.openrdf.model.impl.LinkedHashModel
import org.scalatest.mock.MockitoSugar.mock
import org.mockito.Matchers._
import org.scalatestplus.play.{OneAppPerSuite, PlaySpec}
import org.w3.banana.sesame.SesameModule
import play.api.http.HeaderNames
import play.api.libs.json.Json
import play.api.test.Helpers._
import play.api.test.{FakeApplication, FakeHeaders, FakeRequest, Helpers}
import store.{Namespace, SesameRepository}
import utils.GlobalDef

import scala.concurrent.duration._
import scala.util.{Failure, Success}

class AbstractCRUDControllerSpec extends PlaySpec with OneAppPerSuite {


  val degreeToFail = Degree("labelToFail")
  val degreeToPass = Degree("labelToPass")
  val repository = mock[SesameRepository]

  "A DegreeCRUDController " must {
    "create a new degree" in {
      when(repository.add(anyObject[Degree]())(anyObject())).thenReturn(Success(new LinkedHashModel))

      val json = Json.toJson(degreeToPass)

      val request = FakeRequest(
        Helpers.POST,
        routes.DegreeCRUDController.create().url,
        FakeHeaders(Seq(HeaderNames.CONTENT_TYPE -> Seq("application/json"))),
        json
      )
      val result = route(request).get

      status(result) mustBe CREATED
    }

    "fail while creating a degree" in {
      when(repository.add(anyObject[Degree]())(anyObject())).thenReturn(Failure(new Exception()))

      val json = Json.toJson(degreeToFail)

      val request = FakeRequest(
        Helpers.POST,
        routes.DegreeCRUDController.create().url,
        FakeHeaders(Seq(HeaderNames.CONTENT_TYPE -> Seq("application/json"))),
        json
      )


      val result = route(request).get

      status(result) mustBe INTERNAL_SERVER_ERROR
    }
  }
}
