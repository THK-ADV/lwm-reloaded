package services

import base.TestBaseDefinition
import models.Labwork
import models.applications.LabworkApplication
import models.users.User
import org.mockito.Matchers._
import org.mockito.Mockito.when
import org.scalatest.WordSpec
import org.scalatest.mock.MockitoSugar._
import org.scalatest.time.{Seconds, Span}
import store.bind.Bindings
import store.{SesameRepository, Namespace}
import utils.PTree._

import scala.util.Random

class GroupServiceSpec extends WordSpec with TestBaseDefinition {

  val ns = Namespace("http://lwm.gm.fh-koeln.de/")

  val repository = SesameRepository(ns)

  import repository._

  val bindings = Bindings(ns)

  val applicationService = mock[LabworkApplicationService]

  val groupService = new GroupService(repository, applicationService)

  "A group service" should {

    "generate an alphabetical sequence of letters" in {

      val checkAgainst = 'A' to 'Z'
      val alphabetically = groupService.alphabeticalOrdering(26)

      checkAgainst.forall(c => alphabetically.contains(c.toString)) shouldBe true
    }

    "sort a list of applicants for a given labwork" in {
      val labworkUUID = Labwork.randomUUID

      val users = Vector(
        User.randomUUID,
        User.randomUUID,
        User.randomUUID,
        User.randomUUID,
        User.randomUUID,
        User.randomUUID,
        User.randomUUID
      )

      val applications = Vector(
        LabworkApplication(labworkUUID, users(0), Set(users(1))),
        LabworkApplication(labworkUUID, users(1), Set(users(0), users(3))),
        LabworkApplication(labworkUUID, users(2), Set(users(3))),
        LabworkApplication(labworkUUID, users(3), Set(users(2), users(1))),
        LabworkApplication(labworkUUID, users(4), Set(users(5))),
        LabworkApplication(labworkUUID, users(5), Set(users(4))),
        LabworkApplication(labworkUUID, users(6), Set.empty)
      )
      when(applicationService.applicationsFor(labworkUUID)).thenReturn(Some(applications))

      val sorted = groupService.sortApplicantsFor(labworkUUID)
      val directlySorted = sortWithPairs(Random.shuffle(applications).map(z => (z.applicant, z.friends.toList)))

      sorted match {
        case Some(v) => v.last shouldBe directlySorted.last
        case None => fail("Should've returned and sorted applicants")
      }
    }

    "sort the lists of labwork applicants concurrently" in {
      val labwork1 = Labwork.randomUUID
      val labwork2 = Labwork.randomUUID
      val labwork3 = Labwork.randomUUID

      val alab1 = Vector(
        User.randomUUID,
        User.randomUUID,
        User.randomUUID,
        User.randomUUID,
        User.randomUUID,
        User.randomUUID,
        User.randomUUID
      )
      val alab2 = Vector(
        User.randomUUID,
        User.randomUUID,
        User.randomUUID,
        User.randomUUID,
        User.randomUUID,
        User.randomUUID
      )

      val alab3 = Vector(
        User.randomUUID,
        User.randomUUID,
        User.randomUUID,
        User.randomUUID,
        User.randomUUID,
        User.randomUUID,
        User.randomUUID,
        User.randomUUID
      )

      val applicationsLabwork1 = Vector(
        LabworkApplication(labwork1, alab1(0), Set(alab1(1))),
        LabworkApplication(labwork1, alab1(1), Set(alab1(0), alab1(3))),
        LabworkApplication(labwork1, alab1(2), Set(alab1(3))),
        LabworkApplication(labwork1, alab1(3), Set(alab1(2), alab1(1))),
        LabworkApplication(labwork1, alab1(4), Set(alab1(5))),
        LabworkApplication(labwork1, alab1(5), Set(alab1(4))),
        LabworkApplication(labwork1, alab1(6), Set.empty)
      )

      val applicationsLabwork2 = Vector(
        LabworkApplication(labwork2, alab2(0), Set(alab2(1))),
        LabworkApplication(labwork2, alab2(1), Set(alab2(0), alab2(3))),
        LabworkApplication(labwork2, alab2(2), Set(alab2(3))),
        LabworkApplication(labwork2, alab2(3), Set(alab2(2), alab2(1))),
        LabworkApplication(labwork2, alab2(4), Set(alab2(5))),
        LabworkApplication(labwork2, alab2(5), Set(alab2(4)))
      )
      val applicationsLabwork3 = Vector(
        LabworkApplication(labwork3, alab3(0), Set(alab3(1))),
        LabworkApplication(labwork3, alab3(1), Set(alab3(0), alab3(3))),
        LabworkApplication(labwork3, alab3(2), Set(alab3(3))),
        LabworkApplication(labwork3, alab3(3), Set(alab3(2), alab3(1))),
        LabworkApplication(labwork3, alab3(4), Set(alab3(5))),
        LabworkApplication(labwork3, alab3(5), Set(alab3(4))),
        LabworkApplication(labwork3, alab3(6), Set.empty),
        LabworkApplication(labwork3, alab3(7), Set.empty)
      )

      when(applicationService.applicationsFor(labwork1)).thenReturn(Some(applicationsLabwork1))
      when(applicationService.applicationsFor(labwork2)).thenReturn(Some(applicationsLabwork2))
      when(applicationService.applicationsFor(labwork3)).thenReturn(Some(applicationsLabwork3))

      val sortedMap = groupService.sortApplicantsForMany(List(labwork1, labwork2, labwork3))

      whenReady(sortedMap) {
        case Some(v) =>
          v.contains(labwork1) shouldBe true
          v.contains(labwork2) shouldBe true
          v.contains(labwork3) shouldBe true

          v(labwork1).size shouldBe applicationsLabwork1.size
          v(labwork2).size shouldBe applicationsLabwork2.size
          v(labwork3).size shouldBe applicationsLabwork3.size

          v(labwork1).forall(alab1.contains) shouldBe true
          v(labwork2).forall(alab2.contains) shouldBe true
          v(labwork3).forall(alab3.contains) shouldBe true

        case None => fail("Future Should contains some result")
      }
    }
  }
}
