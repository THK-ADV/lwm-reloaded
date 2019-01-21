package services

import javax.inject.Inject
import play.api.inject.ApplicationLifecycle
import play.api.libs.json.JsValue
import play.api.libs.ws.WSClient

import scala.concurrent.{ExecutionContext, Future}

class Webservice @Inject()(ws: WSClient, applicationLifecycle: ApplicationLifecycle) {

  def get[A](url: String)(parse: JsValue => A)(implicit executor: ExecutionContext) =
    ws.url(url).get.map(resp => parse(resp.json))

  applicationLifecycle.addStopHook { () =>
    Future.successful(ws.close)
  }
}
