package service

import javax.inject.Inject
import play.api.inject.ApplicationLifecycle
import play.api.libs.json.JsValue
import play.api.libs.ws.WSClient

import scala.concurrent.{ExecutionContext, Future}

class Webservice @Inject()(ws: WSClient, applicationLifecycle: ApplicationLifecycle) {

  def get[A](url: String)(parse: JsValue => A)(implicit executor: ExecutionContext): Future[A] =
    ws.url(url).get.map(resp => parse(resp.json))

  def getWithToken[A](url: String, token: String)(parse: JsValue => A)(implicit executor: ExecutionContext): Future[A] =
    ws.url(url).addHttpHeaders("Authorization" -> token).get.map(resp => parse(resp.json))

  def postWithToken[A](url: String, token: String, body: JsValue)(parse: JsValue => A)(implicit executor: ExecutionContext): Future[A] =
    ws.url(url).addHttpHeaders("Authorization" -> token, "Content-Type" -> "application/json").post(body).map(resp => parse(resp.json))

  def putWithToken[A](url: String, token: String, body: JsValue)(parse: JsValue => A)(implicit executor: ExecutionContext): Future[A] =
    ws.url(url).addHttpHeaders("Authorization" -> token, "Content-Type" -> "application/json").put(body).map(resp => parse(resp.json))

  applicationLifecycle.addStopHook { () =>
    Future.successful(ws.close)
  }
}
