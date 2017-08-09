package services

import java.util.UUID

import akka.actor.{Actor, ActorLogging, ActorRef, ActorSystem, Props}
import models.{InvalidSession, Session, ValidSession}
import akka.pattern.pipe
import dao.UserDao

import scala.concurrent.Future
import scala.util.control.NonFatal

trait SessionHandlingService {

  def newSession(user: String, password: String): Future[Session]

  def isValid(iD: UUID): Future[Boolean]

  def deleteSession(id: UUID): Future[Boolean]
}

class ActorBasedSessionService(system: ActorSystem, authenticator: LdapService, userDao: UserDao) extends SessionHandlingService {

  import services.SessionServiceActor._
  import akka.pattern.ask
  import akka.util.Timeout
  import system.dispatcher

  import scala.concurrent.duration._

  private val ref = system.actorOf(SessionServiceActor.props(authenticator, userDao))
  private implicit val timeout = Timeout(5.seconds)

  override def newSession(user: String, password: String): Future[Session] = {
    (ref ? SessionRequest(user, password))
      .mapTo[Authentication]
      .flatMap {
        case Authenticated(valid) => Future.successful(valid)
        case NotAuthenticated(invalid) => Future.successful(invalid)
        case AuthenticationError(error) => Future.failed(error)
      }
  }

  override def isValid(id: UUID): Future[Boolean] = (ref ? ValidationRequest(id)).mapTo[Boolean]

  override def deleteSession(id: UUID): Future[Boolean] = (ref ? SessionRemovalRequest(id)).mapTo[Boolean]
}

object SessionServiceActor {

  def props(ldap: LdapService, userDao: UserDao): Props = Props(new SessionServiceActor(ldap, userDao))

  case class SessionRequest(user: String, password: String)

  private[SessionServiceActor] case object Update

  case class SessionRemovalRequest(id: UUID)

  case class ValidationRequest(id: UUID)

  sealed trait Authentication

  case class Authenticated(session: ValidSession) extends Authentication

  case class NotAuthenticated(invalid: InvalidSession) extends Authentication

  case class AuthenticationError(error: Throwable) extends Authentication

  private[services] case class Processed(receiver: ActorRef, session: Map[String, ValidSession], response: Authentication)

}

class SessionServiceActor(ldap: LdapService, userDao: UserDao) extends Actor with ActorLogging {

  import services.SessionServiceActor._

  import scala.concurrent.duration._

  implicit val dispatcher = context.system.dispatcher

  context.system.scheduler.schedule(5.seconds, 10.seconds, self, Update)

  override def receive: Receive = sessionHandling(Map.empty)

  def sessionHandling(sessions: Map[String, ValidSession]): Receive = {
    case SessionRequest(user, password) =>
      val requester = sender()
      ldap.authenticate(user, password)
        .flatMap(isAuthorized => resolve(user, isAuthorized))
        .map {
          case valid@ValidSession(username, _, _, _) => Processed(requester, sessions + (username -> valid), Authenticated(valid))
          case invalid@InvalidSession(_) => Processed(requester, sessions, NotAuthenticated(invalid))
        }
        .recover {
          case NonFatal(error) => Processed(requester, sessions, AuthenticationError(error))
        }
        .pipeTo(self)

    case Processed(receiver, ns, response) =>
      receiver ! response
      context become sessionHandling(ns)

    case ValidationRequest(id) =>
      val requester = sender()
      requester ! sessions.exists(_._2.id == id)

    case SessionRemovalRequest(id) =>
      val requester = sender()
      val (ns, response) =
        sessions
          .find(_._2.id == id)
          .fold((sessions, false))(t => (sessions - t._1, true))

      requester ! response
      context become sessionHandling(ns)

    case Update =>
      context become sessionHandling(sessions.filter(_._2.expirationDate.isAfterNow))
  }

  def resolve(systemId: String, isAuthorized: Boolean): Future[Session] = {
    if (isAuthorized) {
      for {
        someUser <- userDao.userId(systemId)
        session <- someUser.fold {
          ldap.user2(systemId)
            .flatMap(user => userDao.createOrUpdate(user))
            .map(user => ValidSession(systemId, user._1.id))
        } { uuid =>
          Future.successful(ValidSession(systemId, uuid))
        }
      } yield session
    } else
      Future.successful(InvalidSession("Invalid credentials"))
  }
}