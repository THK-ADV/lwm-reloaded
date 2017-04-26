package invalidation

import base.SesameDbSpec
import models.{SesameAssignmentEntry, SesameAssignmentEntryType, SesameAssignmentPlan, SesameLabwork}

import scala.util.Random._
import scala.util.Success

class SesameAssignmentPlanInvalidation extends SesameDbSpec {

  "An AssignmentPlan invalidation" should {

    def atypes: Stream[SesameAssignmentEntryType] = Stream.continually(SesameAssignmentEntryType(s"String$nextInt", nextBoolean()))
    def aentry: Stream[SesameAssignmentEntry] = Stream.continually(SesameAssignmentEntry(nextInt, "label", (atypes take 10).toSet))
    def aplans: Stream[SesameAssignmentPlan] = Stream.continually(SesameAssignmentPlan(SesameLabwork.randomUUID, 1, 2, (aentry take 10).toSet))

    "invalidate the assignment plan and subsequent assignment entries" in {
      import bindings.AssignmentPlanDescriptor

      val plans = (aplans take 100).toSet
      val toInvalidate = shuffle(plans) take 30

      repo.addMany[SesameAssignmentPlan](plans)

      toInvalidate foreach (a => repo.invalidate[SesameAssignmentPlan](SesameAssignmentPlan.generateUri(a)))

      repo.getAll[SesameAssignmentPlan] shouldBe Success(plans diff toInvalidate)

      repo.deepGetAll[SesameAssignmentPlan] map (_ map (_.id)) shouldBe Success(plans map (_.id))
    }
  }
}
