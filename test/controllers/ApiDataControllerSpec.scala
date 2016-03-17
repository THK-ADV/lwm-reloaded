package controllers

import base.TestBaseDefinition
import models.security.Role
import org.scalatest.WordSpec
import play.api.libs.json.JsValue
import play.api.mvc.{Action, Result, AnyContent, Request}
import play.api.test.FakeRequest
import store.bind.Bindings
import store.{Namespace, SesameRepository}
import utils.LwmActions.ContentTypedAction
import play.api.test.Helpers._

import scala.util.Success

class ApiDataControllerSpec extends WordSpec with TestBaseDefinition {

  val ns = Namespace("http://lwm.gm.fh-koeln.de")
  val repository = SesameRepository(ns)

  import repository.ops

  val controller = new ApiDataController(repository)
  val bindings = Bindings(ns)

}
