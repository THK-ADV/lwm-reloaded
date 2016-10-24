package services

import java.util.UUID

import base.TestBaseDefinition
import models.labwork.{Labwork, LabworkApplication}
import models.users.User
import org.mockito.Mockito.when
import org.scalatest.WordSpec
import org.scalatest.mock.MockitoSugar._
import utils.PreferenceSort._

import scala.util.{Failure, Random, Success}

class GroupServiceSpec extends WordSpec with TestBaseDefinition {

  val applicationService = mock[LabworkApplicationService]

  val groupService = new GroupService(applicationService)

  val labwork = UUID.randomUUID
  val users = ((0 until 100) map (_ => UUID.randomUUID)).toVector
  val apps = users.map(id => LabworkApplication(labwork, id, Set.empty)).toSet

  "A group service" should {

    "generate an alphabetical sequence of letters" in {
      val checkAgainst = 'A' to 'Z'
      val alphabetically = groupService.alphabeticalOrdering(26)

      checkAgainst.forall(c => alphabetically.contains(c.toString)) shouldBe true
    }

    "sort a list of applicants for a given labwork" in {
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
        LabworkApplication(labwork, users(0), Set(users(1))),
        LabworkApplication(labwork, users(1), Set(users(0), users(3))),
        LabworkApplication(labwork, users(2), Set(users(3))),
        LabworkApplication(labwork, users(3), Set(users(2), users(1))),
        LabworkApplication(labwork, users(4), Set(users(5))),
        LabworkApplication(labwork, users(5), Set(users(4))),
        LabworkApplication(labwork, users(6), Set.empty)
      )
      when(applicationService.applicationsFor(labwork)).thenReturn(Success(applications))

      val sorted = groupService.sortApplicantsFor(labwork)
      val directlySorted = sort(Random.shuffle(applications).map(z => (z.applicant, z.friends)))

      sorted match {
        case Success(v) => v.last shouldBe directlySorted.last
        case Failure(e) => fail(s"Should've returned and sorted applicants: ${e.getMessage}")
      }
    }

    "generate groups by given strategy" in {
      val count = Count("8")
      val range = Range("13", "15")

      when(applicationService.applicationsFor(labwork)).thenReturn(Success(apps))

      groupService.groupBy(labwork, count) match {
        case Success(groups) =>
          groups.size shouldBe count.value.toInt
          groups.flatMap(_.members) shouldBe users.toSet
        case Failure(e) =>
          fail("Error while creating groups by count", e)
      }

      groupService.groupBy(labwork, range) match {
        case Success(groups) =>
          groups.size shouldBe Math.round(users.size.toFloat / range.max.toFloat)
          groups.flatMap(_.members) shouldBe users.toSet
        case Failure(e) =>
          fail("Error while creating groups by range", e)
      }
    }

    "stop creating groups when no applications have been found" in {
      when(applicationService.applicationsFor(labwork)).thenReturn(Success(Set.empty[LabworkApplication]))

      groupService.groupBy(labwork, Count("1")) match {
        case Failure(e) =>
          e.getMessage should include ("Predicate does not hold")
        case _ => fail("group service did not stop working on failure case")
      }
    }

    "stop creating groups when max is lower than min" in {
      when(applicationService.applicationsFor(labwork)).thenReturn(Success(apps))

      groupService.groupBy(labwork, Range("5", "3")) match {
        case Failure(e) =>
          e.getMessage should include ("Predicate does not hold")
        case _ => fail("group service did not stop working on failure case")
      }
    }

  }
}
