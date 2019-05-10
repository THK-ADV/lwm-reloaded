package service

import java.util.UUID

import base.TestBaseDefinition
import models.LabworkApplication
import org.joda.time.DateTime
import org.scalatest.WordSpec

import scala.util.Random

class GroupServiceSpec extends WordSpec with TestBaseDefinition {

  "A GroupServiceSpec" should {
    "generate no alphabetical sequence if amount of chars is less than zero" in {
      GroupService.alphabeticalOrdering(0) shouldBe empty
      GroupService.alphabeticalOrdering(-1) shouldBe empty
    }

    "generate an alphabetical sequence of letters for our entire alphabet (26 chars)" in {
      val checkAgainst = 'A' to 'Z'
      val alphabetically = GroupService.alphabeticalOrdering(26)

      checkAgainst.map(_.toString) should contain theSameElementsAs alphabetically
    }

    "generate an alphabetical sequence of letters for less than 26 chars" in {
      val checkAgainst = 'A' to 'Z'
      val alphabetically = GroupService.alphabeticalOrdering(20)

      checkAgainst.take(20).map(_.toString) should contain theSameElementsAs alphabetically
    }

    "generate an alphabetical sequence of letters for more than 26 chars by using numbers as a suffix (level 1)" in {
      val alph = 'A' to 'Z' map (_.toString)
      val suffixed = alph take 4 map (a => s"$a-1")
      val alphabetically = GroupService.alphabeticalOrdering(30)

      alph ++ suffixed should contain theSameElementsAs alphabetically
    }

    "generate an alphabetical sequence of letters for more than 52 chars by using numbers as a suffix (level 2)" in {
      val alph = 'A' to 'Z' map (_.toString)
      val suffixed1 = alph map (a => s"$a-1")
      val suffixed2 = alph take 5 map (a => s"$a-2")
      val alphabetically = GroupService.alphabeticalOrdering(26 * 2 + 5)

      alph ++ suffixed1 ++ suffixed2 should contain theSameElementsAs alphabetically
    }

    "sort a list of applicants for a given labwork" in {
      import utils.PreferenceSort._

      val labwork = UUID.randomUUID

      val users = Vector(
        UUID.randomUUID,
        UUID.randomUUID,
        UUID.randomUUID,
        UUID.randomUUID,
        UUID.randomUUID,
        UUID.randomUUID,
        UUID.randomUUID
      )

      val applications = Vector(
        LabworkApplication(labwork, users(0), Set(users(1)), DateTime.now),
        LabworkApplication(labwork, users(1), Set(users(0), users(3)), DateTime.now),
        LabworkApplication(labwork, users(2), Set(users(3)), DateTime.now),
        LabworkApplication(labwork, users(3), Set(users(2), users(1)), DateTime.now),
        LabworkApplication(labwork, users(4), Set(users(5)), DateTime.now),
        LabworkApplication(labwork, users(5), Set(users(4)), DateTime.now),
        LabworkApplication(labwork, users(6), Set.empty, DateTime.now)
      )

      val sorted = GroupService.sort(applications)
      val directlySorted = sort(Random.shuffle(applications).map(z => (z.applicant, z.friends)))

      sorted should contain theSameElementsAs directlySorted // hopefully, this is always true
    }

    "generate groups by both strategies" in {
      import service.GroupingStrategy._

      val labwork = UUID.randomUUID
      val users = ((0 until 100) map (_ => UUID.randomUUID)).toVector
      val apps = users.map(id => LabworkApplication(labwork, id, Set.empty, DateTime.now))

      def testCount(value: Int): Unit = {
        val count = Count(value)
        val groups = GroupService.groupApplicantsBy(count)(apps, labwork)
        groups.size shouldBe count.value
        groups.map(_.label) shouldBe ('A' to 'Z').take(count.value).map(_.toString)
        groups.flatMap(_.members) should contain theSameElementsAs users
      }

      def testRange(min: Int, max: Int): Unit = {
        val range = Range(min, max)
        val groups2 = GroupService.groupApplicantsBy(range)(apps, labwork)
        val size = Math.round(users.size.toFloat / range.max.toFloat)
        groups2.size shouldBe size
        groups2.map(_.label) shouldBe ('A' to 'Z').take(size).map(_.toString)
        groups2.flatMap(_.members) should contain theSameElementsAs users
      }

      testCount(8)
      testCount(10)
      testCount(20)
      testRange(13, 15)
      testRange(18, 19)
    }
  }
}
