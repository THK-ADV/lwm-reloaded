package actors

import java.util.UUID

import akka.actor.ActorSystem
import akka.testkit.TestKit
import base.TestBaseDefinition
import models.users.{Employee, Student, User}
import org.mockito.Matchers.anyObject
import org.mockito.Mockito.when
import org.scalatest.WordSpecLike
import org.scalatest.mock.MockitoSugar._
import services.{LDAPService, LdapSyncServiceActor}
import store.bind.Bindings
import store.{Namespace, Resolvers, SesameRepository}

import scala.concurrent.Future
import scala.util.{Failure, Success}

class LdapSyncServiceActorSpec extends TestKit(ActorSystem("test_system")) with WordSpecLike with TestBaseDefinition {

  val namespace = Namespace("http://lwm.gm.fh-koeln.de/")
  val repository = SesameRepository(namespace)
  val bindings = Bindings[repository.Rdf](namespace)
  val ldapService = mock[LDAPService]
  val resolvers = mock[Resolvers]

  val actorRef = system.actorOf(LdapSyncServiceActor.props(repository, ldapService, resolvers))

  import scala.util.Random.{nextBoolean, nextInt}

  val enrollments = List(UUID.randomUUID, UUID.randomUUID, UUID.randomUUID, UUID.randomUUID)

  def randomUser(amount: Int): Set[User] = {
    val types = List(User.employeeType, User.lecturerType)

    (0 until amount).map { i =>
      if (nextBoolean)
        Student(s"systemId$i", s"lastname$i", s"firstname$i", s"email$i", s"regId$i", enrollments(nextInt(enrollments.size)))
      else
        Employee(s"systemId$i", s"lastname$i", s"firstname$i", s"email$i", types(nextInt(types.size)))
    }.toSet
  }

  def deltaUser(users: Set[User]): Set[User] = {
    users map {
      case alterLastname if alterLastname.lastname.contains(String.valueOf(10)) => alterLastname match {
        case student: Student => Student(student.systemId, "otherLastname", student.firstname, student.email, student.registrationId, student.enrollment)
        case employee: Employee => Employee(employee.systemId, "otherLastname", employee.firstname, employee.email, employee.status)
      }

      case alterFirstname if alterFirstname.firstname.contains(String.valueOf(20)) => alterFirstname match {
        case student: Student => Student(student.systemId, student.lastname, "otherFirstname", student.email, student.registrationId, student.enrollment)
        case employee: Employee => Employee(employee.systemId, employee.lastname, "otherFirstname", employee.email, employee.status)
      }

      case alterEmail if alterEmail.email.contains(String.valueOf(30)) => alterEmail match {
        case student: Student => Student(student.systemId, student.lastname, student.firstname, "otherEmail", student.registrationId, student.enrollment)
        case employee: Employee => Employee(employee.systemId, employee.lastname, employee.firstname, "otherEmail", employee.status)
      }

      case user => user match {
        case student: Student => Student(student.systemId, student.lastname, student.firstname, student.email, student.registrationId, student.enrollment)
        case employee: Employee => Employee(employee.systemId, employee.lastname, employee.firstname, employee.email, employee.status)
      }
    }
  }

  import akka.testkit._
  import scala.concurrent.duration._
  import bindings.UserBinding._

  "A LdapSyncServiceActorSpec" should {

    "successfully update those users who differs" in {
      val users = randomUser(100)
      val numberOfDiffs = 3

      when(ldapService.users(anyObject())(anyObject())).thenReturn(Future.successful(deltaUser(users).drop(5)))
      when(resolvers.degree(anyObject())).thenReturn(Success(enrollments(nextInt(enrollments.size))))

      repository.addMany(users) foreach { _ =>
        10.seconds.dilated

        actorRef ! LdapSyncServiceActor.SyncRequest

        awaitAssert {
          repository.get[User](userBinder, classUri) match {
            case Success(updatedUser) =>
              updatedUser.size shouldBe users.size

              updatedUser forall { updated =>
                users.exists(_.id == updated.id)
              } shouldBe true

              updatedUser.toList map { updated =>
                users.contains(updated)
              } count(_ == false) shouldBe numberOfDiffs

            case Failure(e) =>
              fail("there should be some users", e)
          }
        }
      }
    }

    "not update users when there are no changes" in {
      val users = randomUser(100)
      val numberOfDiffs = 0

      when(ldapService.users(anyObject())(anyObject())).thenReturn(Future.successful(users))
      when(resolvers.degree(anyObject())).thenReturn(Success(enrollments(nextInt(enrollments.size))))

      repository.addMany(users) foreach { _ =>
        10.seconds.dilated

        actorRef ! LdapSyncServiceActor.SyncRequest

        awaitAssert {
          repository.get[User](userBinder, classUri) match {
            case Success(updatedUser) =>
              updatedUser.size shouldBe users.size

              updatedUser forall { updated =>
                users.exists(_.id == updated.id)
              } shouldBe true

              updatedUser.toList map { updated =>
                users.contains(updated)
              } count(_ == false) shouldBe numberOfDiffs

            case Failure(e) =>
              fail("there should be some users", e)
          }
        }
      }
    }

    "do nothing when ldap dies" in {
      val users = randomUser(100)
      val numberOfDiffs = 0

      when(ldapService.users(anyObject())(anyObject())).thenReturn(Future.failed(new Exception("Oops, something went wrong")))
      when(resolvers.degree(anyObject())).thenReturn(Success(enrollments(nextInt(enrollments.size))))

      repository.addMany(users) foreach { _ =>
        10.seconds.dilated

        actorRef ! LdapSyncServiceActor.SyncRequest

        awaitAssert {
          repository.get[User](userBinder, classUri) match {
            case Success(updatedUser) =>
              updatedUser.size shouldBe users.size

              updatedUser forall { updated =>
                users.exists(_.id == updated.id)
              } shouldBe true

              updatedUser.toList map { updated =>
                users.contains(updated)
              } count(_ == false) shouldBe numberOfDiffs

            case Failure(e) =>
              fail("there should be some users", e)
          }
        }
      }
    }
  }

  override protected def beforeEach(): Unit = {
    super.beforeEach()

    repository.connection { conn =>
      repository.rdfStore.removeGraph(conn, repository.ns)
    }
  }
}
