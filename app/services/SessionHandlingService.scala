package services

import java.util.UUID

import akka.actor.{Actor, ActorLogging, ActorSystem, Props}
import models.Session
import store.Resolvers

import scala.concurrent.Future
import scala.util.{Failure, Success}

trait SessionHandlingService {

  def newSession(user: String, password: String): Future[Session]

  def isValid(iD: UUID): Future[Boolean]

  def deleteSession(id: UUID): Future[Boolean]
}

class ActorBasedSessionService(system: ActorSystem, authenticator: LDAPService, resolvers: Resolvers) extends SessionHandlingService {

  import SessionServiceActor._
  import akka.pattern.ask
  import akka.util.Timeout
  import system.dispatcher

  import scala.concurrent.duration._

  private val ref = system.actorOf(SessionServiceActor.props(authenticator, resolvers))
  private implicit val timeout = Timeout(5.seconds)

  override def newSession(user: String, password: String): Future[Session] = {
    val promise = concurrent.Promise[Session]()
    (ref ? SessionRequest(user, password)).mapTo[AuthenticationResponse].onComplete {
      case Success(response) =>
        response match {
          case AuthenticationSuccess(session) =>
            promise.success(session)
          case AuthenticationFailure(message) =>
            promise.failure(new RuntimeException(message))
        }
      case Failure(t) =>
        promise.failure(t)
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

  def props(ldap: LDAPService, resolvers: Resolvers): Props = Props(new SessionServiceActor(ldap)(resolvers))

  private[services] case class SessionRemovalRequest(id: UUID)

  private[services] sealed trait RemovalResponse

  private[services] case class RemovalFailure(reason: String) extends RuntimeException(reason) with RemovalResponse

  private[services] case object RemovalSuccessful extends RemovalResponse


  private[services] case class ValidationRequest(id: UUID)

  case class SessionRequest(user: String, password: String)


  private[services] sealed trait ValidationResponse

  private[services] case object ValidationSuccess extends ValidationResponse

  private[services] case class ValidationFailure(reason: String) extends RuntimeException(reason) with ValidationResponse


  private[SessionServiceActor] case object Update


  private[services] trait AuthenticationResponse

  case class AuthenticationSuccess(session: Session) extends AuthenticationResponse

  case class AuthenticationFailure(message: String) extends AuthenticationResponse

}

class SessionServiceActor(ldap: LDAPService)(resolvers: Resolvers) extends Actor with ActorLogging {

  import SessionServiceActor._
  import resolvers._

  import scala.concurrent.duration._

  implicit val dispatcher = context.system.dispatcher

  var sessions: Map[String, Session] = Map.empty

  context.system.scheduler.schedule(5.seconds, 10.seconds, self, Update)


  override def receive: Receive = {
    case SessionRequest(user, password) =>
      val requester = sender()

      def resolve(auth: Boolean): Future[Session] = if (auth) {
        username(user) match {
          case Some(userId) => Future.successful {
            Session(user.toLowerCase, userId)
          }
          case _ => ldap.attributes(user).map(missingUserData).flatMap {
            case Success(_) => resolve(auth)
            case Failure(t) => Future.failed(t)
          }
        }
      }

      else Future.failed(new Throwable("Invalid credentials"))

      ldap.authenticate(user, password).flatMap(resolve).onComplete {
        case Success(session) =>
          sessions = sessions + (session.username -> session)
          requester ! AuthenticationSuccess(session)
        case Failure(e) =>
          requester ! AuthenticationFailure(e.getMessage)
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