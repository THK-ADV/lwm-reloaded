package controllers

import play.api.mvc.{Action, Controller}
import store.SesameRepository

class ApiDataController(private val repository: SesameRepository) extends Controller {

  implicit val ns = repository.namespace

  def hello = Action { implicit request => Ok }
}