package actors

import java.util.UUID

import akka.actor.ActorSystem
import akka.pattern.ask
import akka.util.Timeout
import base.TestBaseDefinition
import dao.UserDao
import models._
import org.mockito.Matchers._
import org.mockito.Mockito.when
import org.scalatest.WordSpec
import org.scalatest.mock.MockitoSugar.mock
import services.{LdapService, LdapUser, SessionServiceActor}
import services.SessionServiceActor.{Authenticated, Authentication, AuthenticationError, NotAuthenticated}

import scala.concurrent.{Await, Future}
import scala.concurrent.duration._
import scala.language.postfixOps

class SessionServiceActorSpec extends WordSpec with TestBaseDefinition {

  implicit val timeout = Timeout(5 seconds)
  implicit val system = ActorSystem("TestSystem")

  val ldap = mock[LdapService]
  val userDao = mock[UserDao]

  val user = LdapUser("mi1111", "Last", "First", "Email", User.EmployeeType, None, None, UUID.randomUUID())
  val actorRef = system.actorOf(SessionServiceActor.props(ldap, userDao))

  "A SessionServiceActor" should {

    "block unauthorized users" in {
      when(ldap.authenticate(anyString(), anyString())).thenReturn(Future.successful(false))

      val future = (actorRef ? SessionServiceActor.SessionRequest("", "")).mapTo[Authentication]
      val result = Await.result(future, timeout.duration)

      result match {
        case NotAuthenticated(invalid) => invalid.message shouldBe "Invalid credentials"
        case _ => fail("Should not return a success")
      }
    }

    "not create a user if an appropriate role has not been found" in {
      val errMsg = "No appropriate Role found while resolving user"

      when(ldap.authenticate(anyString(), anyString())).thenReturn(Future.successful(true))
      when(ldap.user2(anyString())).thenReturn(Future.successful(user))
      when(userDao.userId(anyObject())).thenReturn(Future.successful(None))
      when(userDao.createOrUpdate(anyObject())).thenReturn(Future.failed(new Throwable(errMsg)))

      val future = (actorRef ? SessionServiceActor.SessionRequest(user.systemId, "")).mapTo[Authentication]
      val result = Await.result(future, timeout.duration)

      result match {
        case AuthenticationError(error) =>
          error.getMessage shouldBe errMsg
        case _ => fail("Should not return a success")
      }
    }

    "create a user with role and session" in {
      val dbUser = DbUser(user.systemId, user.lastname, user.firstname, user.email, user.status, None, None, id = user.id)
      val auth = PostgresAuthorityAtom(dbUser.toLwmModel, PostgresRole(Role.Employee.label), None, UUID.randomUUID())

      when(ldap.authenticate(anyString(), anyString())).thenReturn(Future.successful(true))
      when(ldap.user2(anyString())).thenReturn(Future.successful(user))
      when(userDao.userId(anyObject())).thenReturn(Future.successful(None))
      when(userDao.createOrUpdate(anyObject())).thenReturn(Future.successful((dbUser.toLwmModel, Some(auth))))

      val future = (actorRef ? SessionServiceActor.SessionRequest(user.systemId, "")).mapTo[Authentication]
      val result = Await.result(future, timeout.duration)

      result match {
        case Authenticated(session) =>
          session.userId shouldBe dbUser.id
          session.username shouldBe dbUser.systemId
        case _ => fail("Should not return a failure")
      }
    }

    "authorize a user when he exists" in {
      when(ldap.authenticate(anyString(), anyString())).thenReturn(Future.successful(true))
      when(ldap.user2(anyString())).thenReturn(Future.successful(user))
      when(userDao.userId(anyObject())).thenReturn(Future.successful(Some(user.id)))

      val future = (actorRef ? SessionServiceActor.SessionRequest(user.systemId, "")).mapTo[Authentication]
      val result = Await.result(future, timeout.duration)

      result match {
        case Authenticated(session) =>
          session.userId shouldBe user.id
          session.username shouldBe user.systemId
        case _ => fail("Should not return a failure")
      }
    }
  }

  override protected def afterAll(): Unit = {
    system.terminate()
  }
}
