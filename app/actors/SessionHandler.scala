package utils

import java.util.UUID
import akka.actor.{Actor, Props}
import org.joda.time.{DateTime, Period}
import play.api.Configuration
import utils.SessionHandler._

object SessionHandler {

  case class AuthenticationRequest(user: String, password: String)

  case class LogoutRequest(sessionID: String)

  case class Session(id: String, user: String)

  private[SessionHandler] case object SessionTick

  def props(config: Configuration) = Props(new SessionHandler(config))
}

class SessionHandler(config: Configuration) extends Actor {

  import LDAPAuthentication._

  import scala.concurrent.ExecutionContext.Implicits.global
  import scala.concurrent.duration._

  var sessions: Map[Session, DateTime] = Map.empty

  val DN = config.getString("lwm.bindDN").get
  val GDN = config.getString("lwm.groupDN").get
  val bindHost = config.getString("lwm.bindHost").get
  val bindPort = config.getInt("lwm.bindPort").get
  val lifetime = config.getInt("lwm.sessions.lifetime").getOrElse(8)

  context.system.scheduler.schedule(1.minutes, 1.minutes, self, SessionHandler.SessionTick)

  override def receive: Receive = {
    case SessionTick ⇒
      sessions = sessions.filterNot { session ⇒
        new Period(DateTime.now(), session._2).getMinutes > lifetime
      }

    case AuthenticationRequest(user, password) ⇒
      val requester = sender()
      val authFuture = authenticate(user, password, bindHost, bindPort, DN)

      authFuture.map {
        case l@Left(error) ⇒
          requester ! l
        case Right(success) ⇒
          val session = Session(UUID.randomUUID().toString, user)
          sessions += (session -> DateTime.now())
          requester ! Right(session)
      }

    case LogoutRequest(sessionID) ⇒ sessions = sessions.filterNot(_._1.id == sessionID)

  }
}
