package services

import java.util.UUID

import akka.actor.{Actor, ActorLogging, ActorSystem, Props}
import models.{InvalidSession, Session, ValidSession}
import store.Resolvers

import scala.concurrent.Future
import scala.util.{Failure, Success}

trait SessionHandlingService {

  def newSession(user: String, password: String): Future[Session]

  def isValid(iD: UUID): Future[Boolean]

  def deleteSession(id: UUID): Future[Boolean]
}

class ActorBasedSessionService(system: ActorSystem, authenticator: LdapService, resolvers: Resolvers) extends SessionHandlingService {

  import SessionServiceActor._
  import akka.pattern.ask
  import akka.util.Timeout
  import system.dispatcher

  import scala.concurrent.duration._

  private val ref = system.actorOf(SessionServiceActor.props(authenticator, resolvers))
  private implicit val timeout = Timeout(5.seconds)

  override def newSession(user: String, password: String): Future[Session] = {
    val promise = concurrent.Promise[Session]()
    (ref ? SessionRequest(user, password)).mapTo[Authentication].onComplete {
      case Success(response) =>
        response match {
          case Authenticated(session) =>
            promise.success(session)
          case NotAuthenticated(invalid) =>
            promise.success(invalid)
          case AuthenticationError(error) =>
            promise.failure(error)
        }
      case Failure(error) =>
        promise.failure(error)
    }
    promise.future
  }

  override def isValid(id: UUID): Future[Boolean] = (ref ? ValidationRequest(id)).map {
    case ValidationSuccess =>
      true
    case ValidationFailure(reason) =>
      false
  }

  override def deleteSession(id: UUID): Future[Boolean] = (ref ? SessionRemovalRequest(id)).map {
    case RemovalSuccessful =>
      true
    case RemovalFailure(reason) =>
      false
  }
}

object SessionServiceActor {

  def props(ldap: LdapService, resolvers: Resolvers): Props = Props(new SessionServiceActor(ldap)(resolvers))

  private[services] case class SessionRemovalRequest(id: UUID)

  private[services] sealed trait RemovalResponse

  private[services] case class RemovalFailure(reason: String) extends RuntimeException(reason) with RemovalResponse

  private[services] case object RemovalSuccessful extends RemovalResponse

  case class SessionRequest(user: String, password: String)

  private[services] case class ValidationRequest(id: UUID)

  private[services] sealed trait ValidationResponse

  private[services] case object ValidationSuccess extends ValidationResponse

  private[services] case class ValidationFailure(reason: String) extends RuntimeException(reason) with ValidationResponse

  private[SessionServiceActor] case object Update

  sealed trait Authentication

  case class Authenticated(session: ValidSession) extends Authentication

  case class NotAuthenticated(invalid: InvalidSession) extends Authentication

  case class AuthenticationError(error: Throwable) extends Authentication

}

class SessionServiceActor(ldap: LdapService)(resolvers: Resolvers) extends Actor with ActorLogging {

  import SessionServiceActor._
  import resolvers._

  import scala.concurrent.duration._

  implicit val dispatcher = context.system.dispatcher

  var sessions: Map[String, ValidSession] = Map.empty

  context.system.scheduler.schedule(5.seconds, 10.seconds, self, Update)

  override def receive: Receive = {
    case SessionRequest(user, password) =>
      val requester = sender()

      def resolve(auth: Boolean): Future[Session] = if (auth) {
        userId(user) match {
          case Success(Some(userId)) => Future.successful {
            ValidSession(user.toLowerCase, userId)
          }
          case Success(_) => ldap.user(user)(degree).map(missingUserData).flatMap {
            case Success(_) => resolve(auth)
            case Failure(t) => Future.failed(t)
          }
          case Failure(e) => Future.failed(e)
        }
      } else Future.successful(InvalidSession("Invalid credentials"))

      ldap.authenticate(user, password).flatMap(resolve).onComplete {
        case Success(session @ ValidSession(username, _, _, _)) =>
          sessions = sessions + (username -> session)
          requester ! Authenticated(session)
        case Success(invalid @ InvalidSession(_)) =>
          requester ! NotAuthenticated(invalid)
        case Failure(e) =>
          requester ! AuthenticationError(e)
      }

    case ValidationRequest(id) =>
      sessions.find(_._2.id == id) match {
        case Some((user, session)) =>
          sender() ! ValidationSuccess

        case None =>
          sender() ! ValidationFailure("Unknown session id.")
      }

    case SessionRemovalRequest(id) =>
      sessions.find { case (user, session) => session.id == id } match {
        case Some((user, session)) =>
          sessions -= user
          sender() ! RemovalSuccessful
        case None =>
          sender() ! RemovalFailure("Unknown session id.")
      }

    case Update =>
      sessions = sessions.filter(_._2.expirationDate.isAfterNow)
  }

}