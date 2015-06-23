package services

import java.util.UUID

import akka.actor.{Actor, ActorLogging, ActorSystem, Props}
import models.Session

import scala.concurrent.Future


trait SessionHandlingService {

  def newSession(user: String): Future[Session]

  def isValid(iD: UUID): Future[Boolean]

  def deleteSession(id: UUID): Future[Boolean]

}


class ActorBasedSessionService(system: ActorSystem) extends SessionHandlingService {

  import SessionServiceActor._
  import akka.pattern.ask
  import akka.util.Timeout
  import system.dispatcher

  import scala.concurrent.duration._

  private val ref = system.actorOf(SessionServiceActor.props)
  private implicit val timeout = Timeout(5.seconds)

  override def newSession(user: String): Future[Session] = (ref ? SessionRequest(user)).mapTo[Session]

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

  case class SessionRemovalRequest(id: UUID)

  sealed trait RemovalResponse

  case object RemovalSuccessful extends RemovalResponse

  case class RemovalFailure(reason: String) extends RuntimeException(reason) with RemovalResponse

  case class ValidationRequest(id: UUID)

  sealed trait ValidationResponse

  case object ValidationSuccess extends ValidationResponse

  case class ValidationFailure(reason: String) extends RuntimeException(reason) with ValidationResponse

  case class SessionRequest(user: String)

  case object Update


  def props: Props = Props(new SessionServiceActor)
}

class SessionServiceActor extends Actor with ActorLogging {

  import SessionServiceActor._

  import scala.concurrent.duration._

  implicit val dispatcher = context.system.dispatcher

  var sessions: Map[String, Session] = Map.empty

  context.system.scheduler.schedule(5.seconds, 10.seconds, self, Update)

  override def receive: Receive = {
    case SessionRequest(user) =>

      val session = Session(user.toLowerCase)
      sessions = sessions + (session.user -> session)
      sender() ! session

    case ValidationRequest(id) =>
      sessions.find(_._2.id == id) match {
        case Some((user, session)) =>
          if (session.expirationDate.isAfterNow) {
            sender() ! ValidationSuccess
          } else {
            sender() ! ValidationFailure("Session timeout.")
            sessions = sessions.filterNot { case (user, session) =>
              session.id == id
            }
          }

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