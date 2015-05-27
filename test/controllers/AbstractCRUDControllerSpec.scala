package controllers

import akka.util.Timeout
import models.Degree
import org.mockito.Mockito._
import org.scalatest.mock.MockitoSugar.mock
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

class AbstractCRUDControllerSpec extends PlaySpec with OneAppPerSuite with SesameModule {

  import DegreeCRUDController._

  val degreeToFail = Degree("labelToFail")
  val degreeToPass = Degree("labelToPass")

  implicit override lazy val app: FakeApplication = FakeApplication(withGlobal = Some(new GlobalDef {
    override implicit def timeout: Timeout = 1.minute

    override lazy val namespace = Namespace("http://fuckyou/")
    override lazy val repo = mock[SesameRepository]

    when(repo.add(degreeToFail)).thenReturn(Failure(new Exception()))
    when(repo.add(degreeToPass)).thenReturn(Success(ops.makeEmptyMGraph()))
  }))

  "A DegreeCRUDController " must {
    "create a new degree" in {
      val json = Json.toJson(degreeToPass)

      val request = FakeRequest(
        Helpers.POST,
        routes.DegreeCRUDController.create().url,
        FakeHeaders(Seq(HeaderNames.CONTENT_TYPE -> Seq("application/json"))),
        json
      )
      val result = controllers.DegreeCRUDController.create()(request)

      status(result) mustBe CREATED
    }

    "fail while creating a degree" in {
      val json = Json.toJson(degreeToFail)

      val request = FakeRequest(
        Helpers.POST,
        routes.DegreeCRUDController.create().url,
        FakeHeaders(Seq(HeaderNames.CONTENT_TYPE -> Seq("application/json"))),
        json
      )
      val result = controllers.DegreeCRUDController.create()(request)

      status(result) mustBe INTERNAL_SERVER_ERROR
    }
  }
}
