package invalidation

import base.SesameDbSpec
import models.{AssignmentEntry, AssignmentEntryType, AssignmentPlan, SesameLabwork}

import scala.util.Random._
import scala.util.Success

class AssignmentPlanInvalidation extends SesameDbSpec {

  "An AssignmentPlan invalidation" should {

    def atypes: Stream[AssignmentEntryType] = Stream.continually(AssignmentEntryType(s"String$nextInt", nextBoolean()))
    def aentry: Stream[AssignmentEntry] = Stream.continually(AssignmentEntry(nextInt, "label", (atypes take 10).toSet))
    def aplans: Stream[AssignmentPlan] = Stream.continually(AssignmentPlan(SesameLabwork.randomUUID, 1, 2, (aentry take 10).toSet))

    "invalidate the assignment plan and subsequent assignment entries" in {
      import bindings.AssignmentPlanDescriptor

      val plans = (aplans take 100).toSet
      val toInvalidate = shuffle(plans) take 30

      repo.addMany[AssignmentPlan](plans)

      toInvalidate foreach (a => repo.invalidate[AssignmentPlan](AssignmentPlan.generateUri(a)))

      repo.getAll[AssignmentPlan] shouldBe Success(plans diff toInvalidate)

      repo.deepGetAll[AssignmentPlan] map (_ map (_.id)) shouldBe Success(plans map (_.id))
    }
  }
}
