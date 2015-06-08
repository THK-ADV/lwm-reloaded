package controllers

import base.TestBaseDefinition
import models.Degree
import org.mockito.Matchers._
import org.mockito.Mockito._
import org.openrdf.model.impl.LinkedHashModel
import org.scalatest.WordSpec
import org.scalatest.mock.MockitoSugar.mock
import org.scalatestplus.play.PlaySpec
import play.api.http.HeaderNames
import play.api.libs.json.Json
import play.api.test.Helpers._
import play.api.test.{FakeHeaders, FakeRequest, Helpers}
import store.{Namespace, SesameRepository}

import scala.util.{Failure, Success}



class DegreeCRUDControllerSpec extends WordSpec with TestBaseDefinition {
  import Degree._

  val degreeToFail = Degree("labelToFail")
  val degreeToPass = Degree("labelToPass")
  val repository = mock[SesameRepository]
  

  val controller = new DegreeCRUDController(repository, Namespace("http://testNamespace/"))
  
  "A DegreeCRUDController " should {
    "create a new degree" in {
      when(repository.add(anyObject[Degree]())(anyObject())).thenReturn(Success(new LinkedHashModel))

      val json = Json.toJson(degreeToPass)

      val request = FakeRequest(
        Helpers.POST,
        "/degrees",
        FakeHeaders(Seq(HeaderNames.CONTENT_TYPE -> "application/json")),
        json
      )
      val result = controller.create()(request)

      status(result) shouldBe CREATED
    }

    "fail while creating a degree" in {
      when(repository.add(anyObject[Degree]())(anyObject())).thenReturn(Failure(new Exception()))

      val json = Json.toJson(degreeToFail)

      val request = FakeRequest(
        Helpers.POST,
        "/degrees",
        FakeHeaders(Seq(HeaderNames.CONTENT_TYPE -> "application/json")),
        json
      )


      val result = controller.create()(request)

      status(result) shouldBe INTERNAL_SERVER_ERROR
    }
  }
}
