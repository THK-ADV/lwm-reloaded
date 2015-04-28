package utils

import java.util.UUID
import akka.actor.{PoisonPill, Actor, Props}
import org.joda.time.{DateTime, Period}
import play.api.Configuration
import utils.SessionHandler._

import scala.concurrent.duration.FiniteDuration

object SessionHandler {

  case class AuthenticationRequest(user: String, password: String)

  case class LogoutRequest(sessionID: String)

  case class Session(id: String, user: String)

  private[SessionHandler] case object SessionTick

  def props(authenticator: Authenticator, sessionTimeout: FiniteDuration) = Props(new SessionHandler(authenticator, sessionTimeout))
}

class SessionHandler(authenticator: Authenticator, lifetime: FiniteDuration) extends Actor {

  import scala.concurrent.ExecutionContext.Implicits.global
  import scala.concurrent.duration._

  var sessions: Map[Session, DateTime] = Map.empty

  context.system.scheduler.schedule(1.minutes, 1.minutes, self, SessionHandler.SessionTick)

  override def receive: Receive = {
    case SessionTick ⇒
      sessions = sessions.filterNot {
        case (session, sessionTime) =>
          new Period(DateTime.now(), sessionTime).getMinutes > lifetime.toMinutes
      }

    case AuthenticationRequest(user, password) ⇒
      val requester = sender()
      val authFuture = authenticator.authenticate(user, password)

      authFuture map { member =>
        if(member) {
          val session = Session(UUID.randomUUID().toString, user)
          sessions += (session -> DateTime.now())
          requester ! Right(session)
        }
        else {
          requester ! Left("invalid credentials")
        }
      }

    case LogoutRequest(sessionID) ⇒ sessions = sessions.filterNot(_._1.id == sessionID)

  }
}
