package controllers

import javax.inject.{Inject, Singleton}
import play.api.libs.json.Json
import play.api.mvc._

@Singleton
class HomepageController @Inject() (cc: ControllerComponents) extends AbstractController(cc) {

  def index = Action {
    Ok(Json.obj(
      "status" -> "OK",
      "message" -> "it works"
    ))
  }
}