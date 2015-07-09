package services

import java.util.UUID

import akka.actor.{Actor, ActorLogging, ActorSystem, Props}
import akka.routing.{DefaultResizer, RoundRobinPool}
import models.Session
import utils.Authenticator

import scala.concurrent.Future
import scala.util.{Failure, Success}


trait SessionHandlingService {

  def newSession(user: String, password: String): Future[Session]

  def isValid(iD: UUID): Future[Boolean]

  def deleteSession(id: UUID): Future[Boolean]

}


class ActorBasedSessionService(system: ActorSystem) extends SessionHandlingService {

  import SessionServiceActor._
  import akka.util.Timeout
  import system.dispatcher
  import akka.pattern.ask

  import scala.concurrent.duration._

  private val ref = system.actorOf(RoundRobinPool(10, resizer = Some(DefaultResizer(10, 20))).props(SessionServiceActor.props(???)))
  private implicit val timeout = Timeout(5.seconds)

  override def newSession(user: String, password: String): Future[Session] = (ref ? SessionRequest(user, password)).mapTo[Session]

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

  def props(authenticator: Authenticator): Props = Props(new SessionServiceActor(authenticator))

  sealed trait RemovalResponse

  sealed trait ValidationResponse

  case class SessionRemovalRequest(id: UUID)

  case class RemovalFailure(reason: String) extends RuntimeException(reason) with RemovalResponse

  case class ValidationRequest(id: UUID)

  case class ValidationFailure(reason: String) extends RuntimeException(reason) with ValidationResponse

  case class SessionRequest(user: String, password: String)

  case object RemovalSuccessful extends RemovalResponse

  case object ValidationSuccess extends ValidationResponse

  case object Update
}

class SessionServiceActor(authenticator: Authenticator) extends Actor with ActorLogging {

  import SessionServiceActor._

  import scala.concurrent.duration._

  implicit val dispatcher = context.system.dispatcher

  var sessions: Map[String, Session] = Map.empty

  context.system.scheduler.schedule(5.seconds, 10.seconds, self, Update)


  override def receive: Receive = {
    case SessionRequest(user, password) =>
      val requester = sender()

      authenticator.authenticate(user, password).onComplete {
        case Success(authenticated) =>
          if (authenticated) {
            val session = Session(user.toLowerCase)
            sessions = sessions + (session.user -> session)
            requester ! session
          } else {
            requester ! ValidationFailure("Invalid Credentials")
          }
        case Failure(e) =>
          requester ! ValidationFailure(e.getMessage)
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
