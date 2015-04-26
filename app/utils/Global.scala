package utils

import akka.util.Timeout
import play.api.Play.current
import play.api.libs.concurrent.Akka
import play.api.mvc._
import play.api.{Application, GlobalSettings, Logger, Play}

object Global extends GlobalSettings {

  import scala.concurrent.duration._

  lazy val serviceName = "lwm"

  implicit val timeout = Timeout(5.seconds)

  override def onStart(app: Application) {
    Akka.system.actorOf(SessionHandler.props(app.configuration), "sessions")
    Logger.debug("Application has started")
  }

  override def onStop(app: Application) {
    Logger.debug("Application shutdown...")
  }

  override def onRouteRequest(req: RequestHeader): Option[Handler] = {
    super.onRouteRequest(req)
  }
}