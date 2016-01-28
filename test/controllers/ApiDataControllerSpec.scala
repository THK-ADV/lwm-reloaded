package controllers

import base.TestBaseDefinition
import models.security.Role
import org.scalatest.WordSpec
import play.api.libs.json.JsValue
import play.api.mvc.{Action, Result, AnyContent, Request}
import play.api.test.FakeRequest
import store.bind.Bindings
import store.{Namespace, SesameRepository}
import utils.LWMActions.ContentTypedAction
import play.api.test.Helpers._

import scala.util.Success

class ApiDataControllerSpec extends WordSpec with TestBaseDefinition {

  val ns = Namespace("http://lwm.gm.fh-koeln.de")
  val repository = SesameRepository(ns)

  import repository.ops

  val controller = new ApiDataController(repository)
  val bindings = Bindings(ns)

  "An ApiDataController" should {
    "create the appropriate start values" in {
      import bindings.RoleBinding._

      val request = FakeRequest(
        GET,
        "api/populate"
      )

      val result = controller.populate(request)
      val numberOfRoles = repository.get[Role].map(_.size)

      status(result) shouldBe OK
      contentAsString(result) shouldBe "Graph created"
      numberOfRoles shouldBe Success(4)
    }
  }
}
