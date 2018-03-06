package actors

import akka.actor.ActorSystem
import akka.testkit.TestKit
import base.TestBaseDefinition
import dao.UserDao
import models._
import org.mockito.Matchers.anyObject
import org.mockito.Mockito.when
import org.scalatest.WordSpecLike
import org.scalatest.mock.MockitoSugar._
import services.ldap.LdapSyncServiceActor.SyncRequest
import services.ldap.{LdapService, LdapSyncServiceActor, LdapUser}

import scala.concurrent.Future

final class LdapSyncServiceActorSpec extends TestKit(ActorSystem("test_system")) with WordSpecLike with TestBaseDefinition {

  private val ldapService = mock[LdapService]
  private val userDao = mock[UserDao]

  private val actorRef = system.actorOf(LdapSyncServiceActor.props(ldapService, userDao))

  import akka.testkit._
  import dao.AbstractDaoSpec.{degrees, populateEmployees, populateStudents}

  import scala.concurrent.duration._
  import scala.util.Random.nextBoolean

  10.seconds.dilated

  "A LdapSyncServiceActorSpec" should {

    "successfully update update all users by synchronizing with ldap source" in {
      val students = populateStudents(2).map(_.toLwmModel)
      val employees = populateEmployees(2).map(_.toLwmModel)
      val all = students ++ employees
      val ladpUsers = all.map {
        case s: PostgresStudent =>
          val abbrev = degrees.find(_.id == s.enrollment).map(_.abbreviation.toLowerCase).get
          LdapUser(s.systemId, if (nextBoolean) s"${s.lastname}NEW" else s.lastname, s.firstname, s.email, User.StudentType, Some(s.registrationId), Some(abbrev))
        case e: PostgresEmployee =>
          LdapUser(e.systemId, if (nextBoolean) s"${e.lastname}NEW" else e.lastname, if (nextBoolean) s"${e.firstname}NEW" else e.firstname, e.email, User.EmployeeType, None, None)
      }.toSet

      val updated = ladpUsers.map { l =>
        val degree = l.degreeAbbrev.flatMap(a => degrees.find(_.abbreviation.toLowerCase == a.toLowerCase)).map(_.id)
        val id = all.find(_.systemId == l.systemId).map(_.id).get
        val user = DbUser(l.systemId, l.lastname, l.firstname, l.email, l.status, l.registrationId, degree, id = id)

        Future.successful((user.toLwmModel, Option.empty[PostgresAuthorityAtom]))
      }.toSeq

      when(userDao.get(atomic = false)).thenReturn(Future.successful(all))
      when(ldapService.users(anyObject())).thenReturn(Future.successful(ladpUsers))
      when(userDao.createOrUpdate(anyObject())).thenReturn(updated.head, updated.tail:_*)

      actorRef ! SyncRequest
    }
  }

  override protected def afterAll(): Unit = {
    super.afterAll()

    system.terminate
  }
}
