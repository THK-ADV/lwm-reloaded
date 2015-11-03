package actors

import akka.actor.ActorSystem
import akka.pattern.ask
import akka.util.Timeout
import base.TestBaseDefinition
import models.security.{Roles, Role}
import models.users.Student
import org.mockito.Matchers._
import org.mockito.Mockito.when
import org.scalatest.WordSpec
import org.scalatest.mock.MockitoSugar.mock
import services.SessionServiceActor
import services.SessionServiceActor.{AuthenticationSuccess, AuthenticationFailure}
import store.bind.Bindings
import store.{LwmResolvers, Namespace, SesameRepository}
import utils.LDAPService
import scala.concurrent.{Await, Future}
import scala.concurrent.duration._
import scala.language.postfixOps

class SessionServiceActorSpec extends WordSpec with TestBaseDefinition {

  implicit val timeout = Timeout(5 seconds)
  implicit val system = ActorSystem("TestSystem")

  val ns = Namespace("http://lwm.gm.fh-koeln.de/")
  val repository = SesameRepository(ns)
  val ldap = mock[LDAPService]
  val resolver = new LwmResolvers(repository)
  val bindings = Bindings[repository.Rdf](ns)

  val user = Student("mi1111", "Last", "First", "Email", "111111", Student.randomUUID)
  val actorRef = system.actorOf(SessionServiceActor.props(ldap, resolver))

  "A SessionServiceActor" should {

    "block unauthorized users" in {
      when(ldap.authenticate(anyString(), anyString())).thenReturn(Future.successful(false))


      val future = actorRef ? SessionServiceActor.SessionRequest("", "")
      val result = Await.result(future, timeout.duration)

      result match {
        case a: AuthenticationFailure => a.message shouldBe "Invalid credentials"
        case _ => fail("Should not return a success")
      }
    }

    "not create a user if an appropriate role has not been found" in {
      when(ldap.authenticate(anyString(), anyString())).thenReturn(Future.successful(true))
      when(ldap.attributes(anyString())).thenReturn(Future.successful(user))

      val future = actorRef ? SessionServiceActor.SessionRequest(user.systemId, "")
      val result = Await.result(future, timeout.duration)

      result match {
        case a: AuthenticationFailure =>
          a.message shouldBe "No student role found while resolving user"
        case _ => fail("Should not return a success")
      }
    }

    "create a session when a user is authorized and contains entries" in {
      when(ldap.authenticate(anyString(), anyString())).thenReturn(Future.successful(true))
      when(ldap.attributes(anyString())).thenReturn(Future.successful(user))
      import bindings.RoleBinding._
      import bindings.permissionBinder

      repository.add[Role](Roles.student)
      repository.add[Role](Roles.admin)

      val future = actorRef ? SessionServiceActor.SessionRequest(user.systemId, "")
      val result = Await.result(future, timeout.duration)

      result match {
        case a: AuthenticationSuccess =>
          a.session.userId shouldBe user.id
          a.session.username shouldBe user.systemId
        case _ => fail("Should not return a failure")
      }
    }
  }


  override protected def beforeEach(): Unit = {
    repository.withConnection { conn =>
      repository.rdfStore.removeGraph(conn, repository.ns)
    }
  }

  override protected def afterAll(): Unit = {
    system.terminate()
  }
}
