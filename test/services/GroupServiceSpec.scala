package services

import java.util.UUID

import base.TestBaseDefinition
import models.{PostgresLabworkApplication, SesameLabworkApplication, User}
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
  val apps = users.map(id => SesameLabworkApplication(labwork, id, Set.empty)).toSet
  val apps2 = users.map(id => PostgresLabworkApplication(labwork, id, Set.empty))

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
        SesameLabworkApplication(labwork, users(0), Set(users(1))),
        SesameLabworkApplication(labwork, users(1), Set(users(0), users(3))),
        SesameLabworkApplication(labwork, users(2), Set(users(3))),
        SesameLabworkApplication(labwork, users(3), Set(users(2), users(1))),
        SesameLabworkApplication(labwork, users(4), Set(users(5))),
        SesameLabworkApplication(labwork, users(5), Set(users(4))),
        SesameLabworkApplication(labwork, users(6), Set.empty)
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
      val count = CountGrouping("8")
      val range = RangeGrouping("13", "15")

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
      when(applicationService.applicationsFor(labwork)).thenReturn(Success(Set.empty[SesameLabworkApplication]))

      groupService.groupBy(labwork, CountGrouping("1")) match {
        case Failure(e) =>
          e.getMessage should include ("Predicate does not hold")
        case _ => fail("group service did not stop working on failure case")
      }
    }

    "stop creating groups when max is lower than min" in {
      when(applicationService.applicationsFor(labwork)).thenReturn(Success(apps))

      groupService.groupBy(labwork, RangeGrouping("5", "3")) match {
        case Failure(e) =>
          e.getMessage should include ("Predicate does not hold")
        case _ => fail("group service did not stop working on failure case")
      }
    }

  }

  "A better group service" should {
    "generate an alphabetical sequence of letters" in {
      val checkAgainst = 'A' to 'Z'
      val alphabetically = GroupService.alphabeticalOrdering(26)

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

      val applications = Vector(
        PostgresLabworkApplication(labwork, users(0), Set(users(1))),
        PostgresLabworkApplication(labwork, users(1), Set(users(0), users(3))),
        PostgresLabworkApplication(labwork, users(2), Set(users(3))),
        PostgresLabworkApplication(labwork, users(3), Set(users(2), users(1))),
        PostgresLabworkApplication(labwork, users(4), Set(users(5))),
        PostgresLabworkApplication(labwork, users(5), Set(users(4))),
        PostgresLabworkApplication(labwork, users(6), Set.empty)
      )

      val sorted = GroupService.sort(applications)
      val directlySorted = sort(Random.shuffle(applications).map(z => (z.applicant, z.friends)))

      sorted.last shouldBe directlySorted.last
    }

    "generate groups by given strategy" in {
      val count = CountGrouping("8")
      val range = RangeGrouping("13", "15")

      val groups = GroupService.groupApplicantsBy(count, apps2, labwork)
      groups.size shouldBe count.value.toInt
      groups.flatMap(_.members).sorted shouldBe users.sorted

      val groups2 = GroupService.groupApplicantsBy(range, apps2, labwork)
      groups2.size shouldBe Math.round(users.size.toFloat / range.max.toFloat)
      groups2.flatMap(_.members).sorted shouldBe users.sorted
    }

    "apply grouping strategies properly" in {
      val twentyfour = users.take(24)
      val twentysix = users.take(26)
      val twentyseven = users.take(27)
      val twentynine = users.take(29)

      val four = 4
      CountGrouping(four.toString).apply(twentyfour).size shouldBe twentyfour.size / four

      val five = 5
      val countFive = CountGrouping(five.toString).apply(twentyfour)
      countFive.size shouldBe five
      countFive.take(five - 1).forall(_.size == five) shouldBe true
      countFive.last.size shouldBe five - 1

      val eight = 8
      val ten = 10
      val eightToTen = RangeGrouping(eight.toString, ten.toString)
      val r1 = eightToTen.apply(twentysix)
      val r2 = eightToTen.apply(twentyseven)
      val r3 = eightToTen.apply(twentynine)

      List(r1, r2, r3).forall(_.size == 3) shouldBe true
      r1.take(2).forall(_.size == eight + 1) shouldBe true
      r1.last.size shouldBe eight

      r2.forall(_.size == eight + 1) shouldBe true

      r3.take(2).forall(_.size == eight + 1 + 1) shouldBe true
      r3.last.size shouldBe eight + 1

      val nine = 9
      val eightToNine = RangeGrouping(eight.toString, nine.toString)
      val r4 = eightToNine.apply(twentynine)

      r4.take(2).forall(_.size == eight + 1 + 1) shouldBe true
      r4.last.size shouldBe eight + 1
    }
  }
}
