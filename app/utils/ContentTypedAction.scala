package utils

import play.api.libs.json.JsValue
import play.api.mvc._

import scala.concurrent.Future

object ContentTypedAction {

  def apply(block: Request[JsValue] => Result)(implicit mimeType: LWMMimeType): Action[JsValue] = Action(LWMBodyParser.parseWith(mimeType))(block)

  def async(block: Request[JsValue] => Future[Result])(implicit mimeType: LWMMimeType): Action[JsValue] = Action.async(LWMBodyParser.parseWith(mimeType))(block)

}

object SecuredContentTypedAction {

  def apply[A](f: Session => Boolean)(block: Request[JsValue] => Result)(implicit mimeType: LWMMimeType): Action[JsValue] = Action(LWMBodyParser.parseWith(mimeType)) {request =>
      if (f(request.session)) block(request) else ???
  }
}

