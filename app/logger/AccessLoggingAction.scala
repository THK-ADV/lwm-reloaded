package logger

import controllers.helper.RequestOps
import play.api.Logger
import play.api.mvc.{AnyContent, Request, Result}

import scala.concurrent.{ExecutionContext, Future}

object AccessLoggingAction extends RequestOps {
  lazy val logger = Logger("access")

  def log(f: Request[AnyContent] => Future[Result])(request: Request[AnyContent])(implicit ctx: ExecutionContext): Future[Result] =
    for {
      result <- f(request)
      _ = logger.info(s"request: user=${request.systemId} method=${request.method} uri=${request.uri} remote-address=${request.remoteAddress} req_body=${request.body} resp_status=${result.header.status}")
    } yield result
}