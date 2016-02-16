package services

import java.util.UUID

import models.{AssignmentPlan, Group}
import models.schedule.Timetable
import utils.{Gen, Genesis}

trait ScheduleGenesisServiceLike {

  def generate(labwork: UUID, timetable: Timetable, groups: Set[Group], assignmentPlan: AssignmentPlan, competitive: Vector[ScheduleG]): (Gen[ScheduleG, Conflict, Int], Int)
}

class ScheduleGenesisService(private val scheduleService: ScheduleService) extends ScheduleGenesisServiceLike {

  override def generate(labwork: UUID, timetable: Timetable, groups: Set[Group], assignmentPlan: AssignmentPlan, competitive: Vector[ScheduleG]): (Gen[ScheduleG, Conflict, Int], Int) = {
    val pop = scheduleService.populate(100, timetable, groups)

    implicit val evaluation = scheduleService.eval(competitive, assignmentPlan.numberOfEntries)
    implicit val mutate = scheduleService.mut
    implicit val cross = scheduleService.cross
    import utils.TypeClasses.instances._
    import utils.Ops.MonadInstances.intM

    Genesis.measureByTaking[ScheduleG, Conflict, Int](pop, 200)
  }
}
