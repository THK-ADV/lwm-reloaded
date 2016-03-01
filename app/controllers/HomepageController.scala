package controllers

import play.api.libs.json.{JsString, Json}
import play.api.mvc._

class HomepageController extends Controller {

  def index = Action {
    Ok(Json.obj(
      "status" -> "OK",
      "message" -> "it works"
    ))
  }

}