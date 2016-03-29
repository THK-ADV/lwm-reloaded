package services

import base.TestBaseDefinition
import models.labwork.{Labwork, LabworkApplication}
import models.users.User
import org.mockito.Mockito.when
import org.scalatest.WordSpec
import org.scalatest.mock.MockitoSugar._
import utils.PTree._

import scala.util.{Failure, Random, Success}

class GroupServiceSpec extends WordSpec with TestBaseDefinition {

  val applicationService = mock[LabworkApplicationService]

  val groupService = new GroupService(applicationService)

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

      val applications = Set(
        LabworkApplication(labworkUUID, users(0), Set(users(1))),
        LabworkApplication(labworkUUID, users(1), Set(users(0), users(3))),
        LabworkApplication(labworkUUID, users(2), Set(users(3))),
        LabworkApplication(labworkUUID, users(3), Set(users(2), users(1))),
        LabworkApplication(labworkUUID, users(4), Set(users(5))),
        LabworkApplication(labworkUUID, users(5), Set(users(4))),
        LabworkApplication(labworkUUID, users(6), Set.empty)
      )
      when(applicationService.applicationsFor(labworkUUID)).thenReturn(Success(applications))

      val sorted = groupService.sortApplicantsFor(labworkUUID)
      val directlySorted = sortWithPairs(Random.shuffle(applications).map(z => (z.applicant, z.friends.toList)))

      sorted match {
        case Success(v) => v.last shouldBe directlySorted.last
        case Failure(e) => fail(s"Should've returned and sorted applicants: ${e.getMessage}")
      }
    }

  }
}
