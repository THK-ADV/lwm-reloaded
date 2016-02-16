package services

import base.TestBaseDefinition
import models._
import models.schedule.{Timetable, TimetableEntry}
import models.semester.{Blacklist, Semester}
import models.users.{Employee, Student}
import org.joda.time.DateTime
import org.joda.time.format.DateTimeFormat
import org.scalatest.WordSpec

class ScheduleSpecHelper {

  def prepare(labworks: Vector[Labwork], entries: Vector[Set[TimetableEntry]]): Vector[(Timetable, Set[Group])] = {
    import scala.util.Random._

    val numberOfGroups = entries.map(_.size).max / labworks.map(_.assignmentPlan.numberOfEntries).max
    val groupSize = 10
    val students = (0 until numberOfGroups * groupSize).map(_ => Student.randomUUID).toVector

    labworks.zip(entries).map {
      case (l, e) =>
        val count = e.size / l.assignmentPlan.numberOfEntries
        val g = shuffle(students).take(count * groupSize).grouped(groupSize).map(s => Group("", l.id, s.toSet, Group.randomUUID)).toSet
        val t = Timetable(l.id, e, DateTime.now, Blacklist.empty, Timetable.randomUUID)
        (t, g)
    }
  }
}

class ScheduleGenesisServiceSpec extends WordSpec with TestBaseDefinition {

  val scheduleService = new ScheduleService
  val helper = new ScheduleSpecHelper
  val scheduleGenesisService = new ScheduleGenesisService(scheduleService)

  val ft = DateTimeFormat.forPattern("dd/MM/yyyy HH:mm:ss")
  val fd = DateTimeFormat.forPattern("dd/MM/yyyy")

  "A ScheduleGenesisServiceSpec" should {

    "generate an initial scheduleG with one population and generation" in {
      val plan = AssignmentPlan(2, Set.empty[AssignmentEntry])
      val mi = Degree("mi", Degree.randomUUID)
      val ap1 = Course("ap1", "c1", Employee.randomUUID, 1, Course.randomUUID)
      val semester1 = Semester("semester1", "start1", "end1", "exam1", Blacklist.empty, Semester.randomUUID)
      val ap1Prak = Labwork("ap1Prak", "desc1", semester1.id, ap1.id, mi.id, plan)

      val ap1Entries = Set(
        TimetableEntry(Employee.randomUUID, Room.randomUUID, mi.id, fd.parseDateTime("27/10/2015"), ft.parseDateTime("27/10/2015 08:00:00"), ft.parseDateTime("27/10/2015 09:00:00"), fd.parseDateTime("27/10/2015")),
        TimetableEntry(Employee.randomUUID, Room.randomUUID, mi.id, fd.parseDateTime("27/10/2015"), ft.parseDateTime("27/10/2015 09:00:00"), ft.parseDateTime("27/10/2015 10:00:00"), fd.parseDateTime("27/10/2015")),
        TimetableEntry(Employee.randomUUID, Room.randomUUID, mi.id, fd.parseDateTime("27/10/2015"), ft.parseDateTime("27/10/2015 10:00:00"), ft.parseDateTime("27/10/2015 11:00:00"), fd.parseDateTime("27/10/2015")),
        TimetableEntry(Employee.randomUUID, Room.randomUUID, mi.id, fd.parseDateTime("29/10/2015"), ft.parseDateTime("29/10/2015 14:00:00"), ft.parseDateTime("29/10/2015 15:00:00"), fd.parseDateTime("29/10/2015")),
        TimetableEntry(Employee.randomUUID, Room.randomUUID, mi.id, fd.parseDateTime("29/10/2015"), ft.parseDateTime("29/10/2015 14:00:00"), ft.parseDateTime("29/10/2015 15:00:00"), fd.parseDateTime("29/10/2015")),
        TimetableEntry(Employee.randomUUID, Room.randomUUID, mi.id, fd.parseDateTime("29/10/2015"), ft.parseDateTime("29/10/2015 15:00:00"), ft.parseDateTime("29/10/2015 16:00:00"), fd.parseDateTime("29/10/2015")),
        TimetableEntry(Employee.randomUUID, Room.randomUUID, mi.id, fd.parseDateTime("29/10/2015"), ft.parseDateTime("29/10/2015 15:00:00"), ft.parseDateTime("29/10/2015 16:00:00"), fd.parseDateTime("29/10/2015")),
        TimetableEntry(Employee.randomUUID, Room.randomUUID, mi.id, fd.parseDateTime("29/10/2015"), ft.parseDateTime("29/10/2015 16:00:00"), ft.parseDateTime("29/10/2015 17:00:00"), fd.parseDateTime("29/10/2015")),
        TimetableEntry(Employee.randomUUID, Room.randomUUID, mi.id, fd.parseDateTime("29/10/2015"), ft.parseDateTime("29/10/2015 16:00:00"), ft.parseDateTime("29/10/2015 17:00:00"), fd.parseDateTime("29/10/2015")),
        TimetableEntry(Employee.randomUUID, Room.randomUUID, mi.id, fd.parseDateTime("30/10/2015"), ft.parseDateTime("30/10/2015 08:00:00"), ft.parseDateTime("30/10/2015 09:00:00"), fd.parseDateTime("30/10/2015")),
        TimetableEntry(Employee.randomUUID, Room.randomUUID, mi.id, fd.parseDateTime("30/10/2015"), ft.parseDateTime("30/10/2015 08:00:00"), ft.parseDateTime("30/10/2015 09:00:00"), fd.parseDateTime("30/10/2015")),
        TimetableEntry(Employee.randomUUID, Room.randomUUID, mi.id, fd.parseDateTime("30/10/2015"), ft.parseDateTime("30/10/2015 09:00:00"), ft.parseDateTime("30/10/2015 10:00:00"), fd.parseDateTime("30/10/2015")),
        TimetableEntry(Employee.randomUUID, Room.randomUUID, mi.id, fd.parseDateTime("30/10/2015"), ft.parseDateTime("30/10/2015 09:00:00"), ft.parseDateTime("30/10/2015 10:00:00"), fd.parseDateTime("30/10/2015")),
        TimetableEntry(Employee.randomUUID, Room.randomUUID, mi.id, fd.parseDateTime("30/10/2015"), ft.parseDateTime("30/10/2015 10:00:00"), ft.parseDateTime("30/10/2015 11:00:00"), fd.parseDateTime("30/10/2015")),
        TimetableEntry(Employee.randomUUID, Room.randomUUID, mi.id, fd.parseDateTime("30/10/2015"), ft.parseDateTime("30/10/2015 10:00:00"), ft.parseDateTime("30/10/2015 11:00:00"), fd.parseDateTime("30/10/2015")),
        TimetableEntry(Employee.randomUUID, Room.randomUUID, mi.id, fd.parseDateTime("30/10/2015"), ft.parseDateTime("30/10/2015 11:00:00"), ft.parseDateTime("30/10/2015 12:00:00"), fd.parseDateTime("30/10/2015")),
        TimetableEntry(Employee.randomUUID, Room.randomUUID, mi.id, fd.parseDateTime("30/10/2015"), ft.parseDateTime("30/10/2015 11:00:00"), ft.parseDateTime("30/10/2015 12:00:00"), fd.parseDateTime("30/10/2015")),
        TimetableEntry(Employee.randomUUID, Room.randomUUID, mi.id, fd.parseDateTime("30/10/2015"), ft.parseDateTime("30/10/2015 12:00:00"), ft.parseDateTime("30/10/2015 13:00:00"), fd.parseDateTime("30/10/2015")),
        TimetableEntry(Employee.randomUUID, Room.randomUUID, mi.id, fd.parseDateTime("30/10/2015"), ft.parseDateTime("30/10/2015 12:00:00"), ft.parseDateTime("30/10/2015 13:00:00"), fd.parseDateTime("30/10/2015")),
        TimetableEntry(Employee.randomUUID, Room.randomUUID, mi.id, fd.parseDateTime("30/10/2015"), ft.parseDateTime("30/10/2015 13:00:00"), ft.parseDateTime("30/10/2015 14:00:00"), fd.parseDateTime("30/10/2015")),
        TimetableEntry(Employee.randomUUID, Room.randomUUID, mi.id, fd.parseDateTime("30/10/2015"), ft.parseDateTime("30/10/2015 13:00:00"), ft.parseDateTime("30/10/2015 14:00:00"), fd.parseDateTime("30/10/2015")), // ======
        TimetableEntry(Employee.randomUUID, Room.randomUUID, mi.id, fd.parseDateTime("03/11/2015"), ft.parseDateTime("03/11/2015 08:00:00"), ft.parseDateTime("03/11/2015 09:00:00"), fd.parseDateTime("03/11/2015")),
        TimetableEntry(Employee.randomUUID, Room.randomUUID, mi.id, fd.parseDateTime("03/11/2015"), ft.parseDateTime("03/11/2015 09:00:00"), ft.parseDateTime("03/11/2015 10:00:00"), fd.parseDateTime("03/11/2015")),
        TimetableEntry(Employee.randomUUID, Room.randomUUID, mi.id, fd.parseDateTime("03/11/2015"), ft.parseDateTime("03/11/2015 10:00:00"), ft.parseDateTime("03/11/2015 11:00:00"), fd.parseDateTime("03/11/2015")),
        TimetableEntry(Employee.randomUUID, Room.randomUUID, mi.id, fd.parseDateTime("05/11/2015"), ft.parseDateTime("05/11/2015 14:00:00"), ft.parseDateTime("05/11/2015 15:00:00"), fd.parseDateTime("05/11/2015")),
        TimetableEntry(Employee.randomUUID, Room.randomUUID, mi.id, fd.parseDateTime("05/11/2015"), ft.parseDateTime("05/11/2015 14:00:00"), ft.parseDateTime("05/11/2015 15:00:00"), fd.parseDateTime("05/11/2015")),
        TimetableEntry(Employee.randomUUID, Room.randomUUID, mi.id, fd.parseDateTime("05/11/2015"), ft.parseDateTime("05/11/2015 15:00:00"), ft.parseDateTime("05/11/2015 16:00:00"), fd.parseDateTime("05/11/2015")),
        TimetableEntry(Employee.randomUUID, Room.randomUUID, mi.id, fd.parseDateTime("05/11/2015"), ft.parseDateTime("05/11/2015 15:00:00"), ft.parseDateTime("05/11/2015 16:00:00"), fd.parseDateTime("05/11/2015")),
        TimetableEntry(Employee.randomUUID, Room.randomUUID, mi.id, fd.parseDateTime("05/11/2015"), ft.parseDateTime("05/11/2015 16:00:00"), ft.parseDateTime("05/11/2015 17:00:00"), fd.parseDateTime("05/11/2015")),
        TimetableEntry(Employee.randomUUID, Room.randomUUID, mi.id, fd.parseDateTime("05/11/2015"), ft.parseDateTime("05/11/2015 16:00:00"), ft.parseDateTime("05/11/2015 17:00:00"), fd.parseDateTime("05/11/2015")),
        TimetableEntry(Employee.randomUUID, Room.randomUUID, mi.id, fd.parseDateTime("06/11/2015"), ft.parseDateTime("06/11/2015 08:00:00"), ft.parseDateTime("06/11/2015 09:00:00"), fd.parseDateTime("06/11/2015")),
        TimetableEntry(Employee.randomUUID, Room.randomUUID, mi.id, fd.parseDateTime("06/11/2015"), ft.parseDateTime("06/11/2015 08:00:00"), ft.parseDateTime("06/11/2015 09:00:00"), fd.parseDateTime("06/11/2015")),
        TimetableEntry(Employee.randomUUID, Room.randomUUID, mi.id, fd.parseDateTime("06/11/2015"), ft.parseDateTime("06/11/2015 09:00:00"), ft.parseDateTime("06/11/2015 10:00:00"), fd.parseDateTime("06/11/2015")),
        TimetableEntry(Employee.randomUUID, Room.randomUUID, mi.id, fd.parseDateTime("06/11/2015"), ft.parseDateTime("06/11/2015 09:00:00"), ft.parseDateTime("06/11/2015 10:00:00"), fd.parseDateTime("06/11/2015")),
        TimetableEntry(Employee.randomUUID, Room.randomUUID, mi.id, fd.parseDateTime("06/11/2015"), ft.parseDateTime("06/11/2015 10:00:00"), ft.parseDateTime("06/11/2015 11:00:00"), fd.parseDateTime("06/11/2015")),
        TimetableEntry(Employee.randomUUID, Room.randomUUID, mi.id, fd.parseDateTime("06/11/2015"), ft.parseDateTime("06/11/2015 10:00:00"), ft.parseDateTime("06/11/2015 11:00:00"), fd.parseDateTime("06/11/2015")),
        TimetableEntry(Employee.randomUUID, Room.randomUUID, mi.id, fd.parseDateTime("06/11/2015"), ft.parseDateTime("06/11/2015 11:00:00"), ft.parseDateTime("06/11/2015 12:00:00"), fd.parseDateTime("06/11/2015")),
        TimetableEntry(Employee.randomUUID, Room.randomUUID, mi.id, fd.parseDateTime("06/11/2015"), ft.parseDateTime("06/11/2015 11:00:00"), ft.parseDateTime("06/11/2015 12:00:00"), fd.parseDateTime("06/11/2015")),
        TimetableEntry(Employee.randomUUID, Room.randomUUID, mi.id, fd.parseDateTime("06/11/2015"), ft.parseDateTime("06/11/2015 12:00:00"), ft.parseDateTime("06/11/2015 13:00:00"), fd.parseDateTime("06/11/2015")),
        TimetableEntry(Employee.randomUUID, Room.randomUUID, mi.id, fd.parseDateTime("06/11/2015"), ft.parseDateTime("06/11/2015 12:00:00"), ft.parseDateTime("06/11/2015 13:00:00"), fd.parseDateTime("06/11/2015")),
        TimetableEntry(Employee.randomUUID, Room.randomUUID, mi.id, fd.parseDateTime("06/11/2015"), ft.parseDateTime("06/11/2015 13:00:00"), ft.parseDateTime("06/11/2015 14:00:00"), fd.parseDateTime("06/11/2015")),
        TimetableEntry(Employee.randomUUID, Room.randomUUID, mi.id, fd.parseDateTime("06/11/2015"), ft.parseDateTime("06/11/2015 13:00:00"), ft.parseDateTime("06/11/2015 14:00:00"), fd.parseDateTime("06/11/2015"))
      )

      val prep = helper.prepare(Vector(ap1Prak), Vector(ap1Entries)).head

      val result = scheduleGenesisService.generate(ap1Prak.id, prep._1, prep._2, plan, Vector.empty[ScheduleG])
      val eval = scheduleService.evaluate(result._1.elem, plan.numberOfEntries, Vector.empty[ScheduleG])

      eval.conflicts shouldBe empty
      eval.value shouldBe 0

      result._2 shouldEqual 0
      result._1.evaluate.err shouldBe empty
      result._1.evaluate.value shouldBe 0
      result._1.elem.labwork shouldEqual ap1Prak.id
      result._1.elem.entries.groupBy(_.group) forall {
        case (_, ss) => ss.size == plan.numberOfEntries
      } shouldBe true
    }

    "generate a further scheduleG with few populations and generations" in {
      val plan = AssignmentPlan(2, Set.empty[AssignmentEntry])
      val mi = Degree("mi", Degree.randomUUID)
      val ap1 = Course("ap1", "c1", Employee.randomUUID, 1, Course.randomUUID)
      val ma1 = Course("ma1", "c2", Employee.randomUUID, 1, Course.randomUUID)
      val semester1 = Semester("semester1", "start1", "end1", "exam1", Blacklist.empty, Semester.randomUUID)
      val ap1Prak = Labwork("ap1Prak", "desc1", semester1.id, ap1.id, mi.id, plan)
      val ma1Prak = Labwork("ma1Prak", "desc2", semester1.id, ma1.id, mi.id, plan)

      val ap1Entries = Set(
        TimetableEntry(Employee.randomUUID, Room.randomUUID, mi.id, fd.parseDateTime("27/10/2015"), ft.parseDateTime("27/10/2015 08:00:00"), ft.parseDateTime("27/10/2015 09:00:00"), fd.parseDateTime("27/10/2015")),
        TimetableEntry(Employee.randomUUID, Room.randomUUID, mi.id, fd.parseDateTime("27/10/2015"), ft.parseDateTime("27/10/2015 09:00:00"), ft.parseDateTime("27/10/2015 10:00:00"), fd.parseDateTime("27/10/2015")),
        TimetableEntry(Employee.randomUUID, Room.randomUUID, mi.id, fd.parseDateTime("27/10/2015"), ft.parseDateTime("27/10/2015 10:00:00"), ft.parseDateTime("27/10/2015 11:00:00"), fd.parseDateTime("27/10/2015")),
        TimetableEntry(Employee.randomUUID, Room.randomUUID, mi.id, fd.parseDateTime("29/10/2015"), ft.parseDateTime("29/10/2015 14:00:00"), ft.parseDateTime("29/10/2015 15:00:00"), fd.parseDateTime("29/10/2015")),
        TimetableEntry(Employee.randomUUID, Room.randomUUID, mi.id, fd.parseDateTime("29/10/2015"), ft.parseDateTime("29/10/2015 14:00:00"), ft.parseDateTime("29/10/2015 15:00:00"), fd.parseDateTime("29/10/2015")),
        TimetableEntry(Employee.randomUUID, Room.randomUUID, mi.id, fd.parseDateTime("29/10/2015"), ft.parseDateTime("29/10/2015 15:00:00"), ft.parseDateTime("29/10/2015 16:00:00"), fd.parseDateTime("29/10/2015")),
        TimetableEntry(Employee.randomUUID, Room.randomUUID, mi.id, fd.parseDateTime("29/10/2015"), ft.parseDateTime("29/10/2015 15:00:00"), ft.parseDateTime("29/10/2015 16:00:00"), fd.parseDateTime("29/10/2015")),
        TimetableEntry(Employee.randomUUID, Room.randomUUID, mi.id, fd.parseDateTime("29/10/2015"), ft.parseDateTime("29/10/2015 16:00:00"), ft.parseDateTime("29/10/2015 17:00:00"), fd.parseDateTime("29/10/2015")),
        TimetableEntry(Employee.randomUUID, Room.randomUUID, mi.id, fd.parseDateTime("29/10/2015"), ft.parseDateTime("29/10/2015 16:00:00"), ft.parseDateTime("29/10/2015 17:00:00"), fd.parseDateTime("29/10/2015")),
        TimetableEntry(Employee.randomUUID, Room.randomUUID, mi.id, fd.parseDateTime("30/10/2015"), ft.parseDateTime("30/10/2015 08:00:00"), ft.parseDateTime("30/10/2015 09:00:00"), fd.parseDateTime("30/10/2015")),
        TimetableEntry(Employee.randomUUID, Room.randomUUID, mi.id, fd.parseDateTime("30/10/2015"), ft.parseDateTime("30/10/2015 08:00:00"), ft.parseDateTime("30/10/2015 09:00:00"), fd.parseDateTime("30/10/2015")),
        TimetableEntry(Employee.randomUUID, Room.randomUUID, mi.id, fd.parseDateTime("30/10/2015"), ft.parseDateTime("30/10/2015 09:00:00"), ft.parseDateTime("30/10/2015 10:00:00"), fd.parseDateTime("30/10/2015")),
        TimetableEntry(Employee.randomUUID, Room.randomUUID, mi.id, fd.parseDateTime("30/10/2015"), ft.parseDateTime("30/10/2015 09:00:00"), ft.parseDateTime("30/10/2015 10:00:00"), fd.parseDateTime("30/10/2015")),
        TimetableEntry(Employee.randomUUID, Room.randomUUID, mi.id, fd.parseDateTime("30/10/2015"), ft.parseDateTime("30/10/2015 10:00:00"), ft.parseDateTime("30/10/2015 11:00:00"), fd.parseDateTime("30/10/2015")),
        TimetableEntry(Employee.randomUUID, Room.randomUUID, mi.id, fd.parseDateTime("30/10/2015"), ft.parseDateTime("30/10/2015 10:00:00"), ft.parseDateTime("30/10/2015 11:00:00"), fd.parseDateTime("30/10/2015")),
        TimetableEntry(Employee.randomUUID, Room.randomUUID, mi.id, fd.parseDateTime("30/10/2015"), ft.parseDateTime("30/10/2015 11:00:00"), ft.parseDateTime("30/10/2015 12:00:00"), fd.parseDateTime("30/10/2015")),
        TimetableEntry(Employee.randomUUID, Room.randomUUID, mi.id, fd.parseDateTime("30/10/2015"), ft.parseDateTime("30/10/2015 11:00:00"), ft.parseDateTime("30/10/2015 12:00:00"), fd.parseDateTime("30/10/2015")),
        TimetableEntry(Employee.randomUUID, Room.randomUUID, mi.id, fd.parseDateTime("30/10/2015"), ft.parseDateTime("30/10/2015 12:00:00"), ft.parseDateTime("30/10/2015 13:00:00"), fd.parseDateTime("30/10/2015")),
        TimetableEntry(Employee.randomUUID, Room.randomUUID, mi.id, fd.parseDateTime("30/10/2015"), ft.parseDateTime("30/10/2015 12:00:00"), ft.parseDateTime("30/10/2015 13:00:00"), fd.parseDateTime("30/10/2015")),
        TimetableEntry(Employee.randomUUID, Room.randomUUID, mi.id, fd.parseDateTime("30/10/2015"), ft.parseDateTime("30/10/2015 13:00:00"), ft.parseDateTime("30/10/2015 14:00:00"), fd.parseDateTime("30/10/2015")),
        TimetableEntry(Employee.randomUUID, Room.randomUUID, mi.id, fd.parseDateTime("30/10/2015"), ft.parseDateTime("30/10/2015 13:00:00"), ft.parseDateTime("30/10/2015 14:00:00"), fd.parseDateTime("30/10/2015")), // ======
        TimetableEntry(Employee.randomUUID, Room.randomUUID, mi.id, fd.parseDateTime("03/11/2015"), ft.parseDateTime("03/11/2015 08:00:00"), ft.parseDateTime("03/11/2015 09:00:00"), fd.parseDateTime("03/11/2015")),
        TimetableEntry(Employee.randomUUID, Room.randomUUID, mi.id, fd.parseDateTime("03/11/2015"), ft.parseDateTime("03/11/2015 09:00:00"), ft.parseDateTime("03/11/2015 10:00:00"), fd.parseDateTime("03/11/2015")),
        TimetableEntry(Employee.randomUUID, Room.randomUUID, mi.id, fd.parseDateTime("03/11/2015"), ft.parseDateTime("03/11/2015 10:00:00"), ft.parseDateTime("03/11/2015 11:00:00"), fd.parseDateTime("03/11/2015")),
        TimetableEntry(Employee.randomUUID, Room.randomUUID, mi.id, fd.parseDateTime("05/11/2015"), ft.parseDateTime("05/11/2015 14:00:00"), ft.parseDateTime("05/11/2015 15:00:00"), fd.parseDateTime("05/11/2015")),
        TimetableEntry(Employee.randomUUID, Room.randomUUID, mi.id, fd.parseDateTime("05/11/2015"), ft.parseDateTime("05/11/2015 14:00:00"), ft.parseDateTime("05/11/2015 15:00:00"), fd.parseDateTime("05/11/2015")),
        TimetableEntry(Employee.randomUUID, Room.randomUUID, mi.id, fd.parseDateTime("05/11/2015"), ft.parseDateTime("05/11/2015 15:00:00"), ft.parseDateTime("05/11/2015 16:00:00"), fd.parseDateTime("05/11/2015")),
        TimetableEntry(Employee.randomUUID, Room.randomUUID, mi.id, fd.parseDateTime("05/11/2015"), ft.parseDateTime("05/11/2015 15:00:00"), ft.parseDateTime("05/11/2015 16:00:00"), fd.parseDateTime("05/11/2015")),
        TimetableEntry(Employee.randomUUID, Room.randomUUID, mi.id, fd.parseDateTime("05/11/2015"), ft.parseDateTime("05/11/2015 16:00:00"), ft.parseDateTime("05/11/2015 17:00:00"), fd.parseDateTime("05/11/2015")),
        TimetableEntry(Employee.randomUUID, Room.randomUUID, mi.id, fd.parseDateTime("05/11/2015"), ft.parseDateTime("05/11/2015 16:00:00"), ft.parseDateTime("05/11/2015 17:00:00"), fd.parseDateTime("05/11/2015")),
        TimetableEntry(Employee.randomUUID, Room.randomUUID, mi.id, fd.parseDateTime("06/11/2015"), ft.parseDateTime("06/11/2015 08:00:00"), ft.parseDateTime("06/11/2015 09:00:00"), fd.parseDateTime("06/11/2015")),
        TimetableEntry(Employee.randomUUID, Room.randomUUID, mi.id, fd.parseDateTime("06/11/2015"), ft.parseDateTime("06/11/2015 08:00:00"), ft.parseDateTime("06/11/2015 09:00:00"), fd.parseDateTime("06/11/2015")),
        TimetableEntry(Employee.randomUUID, Room.randomUUID, mi.id, fd.parseDateTime("06/11/2015"), ft.parseDateTime("06/11/2015 09:00:00"), ft.parseDateTime("06/11/2015 10:00:00"), fd.parseDateTime("06/11/2015")),
        TimetableEntry(Employee.randomUUID, Room.randomUUID, mi.id, fd.parseDateTime("06/11/2015"), ft.parseDateTime("06/11/2015 09:00:00"), ft.parseDateTime("06/11/2015 10:00:00"), fd.parseDateTime("06/11/2015")),
        TimetableEntry(Employee.randomUUID, Room.randomUUID, mi.id, fd.parseDateTime("06/11/2015"), ft.parseDateTime("06/11/2015 10:00:00"), ft.parseDateTime("06/11/2015 11:00:00"), fd.parseDateTime("06/11/2015")),
        TimetableEntry(Employee.randomUUID, Room.randomUUID, mi.id, fd.parseDateTime("06/11/2015"), ft.parseDateTime("06/11/2015 10:00:00"), ft.parseDateTime("06/11/2015 11:00:00"), fd.parseDateTime("06/11/2015")),
        TimetableEntry(Employee.randomUUID, Room.randomUUID, mi.id, fd.parseDateTime("06/11/2015"), ft.parseDateTime("06/11/2015 11:00:00"), ft.parseDateTime("06/11/2015 12:00:00"), fd.parseDateTime("06/11/2015")),
        TimetableEntry(Employee.randomUUID, Room.randomUUID, mi.id, fd.parseDateTime("06/11/2015"), ft.parseDateTime("06/11/2015 11:00:00"), ft.parseDateTime("06/11/2015 12:00:00"), fd.parseDateTime("06/11/2015")),
        TimetableEntry(Employee.randomUUID, Room.randomUUID, mi.id, fd.parseDateTime("06/11/2015"), ft.parseDateTime("06/11/2015 12:00:00"), ft.parseDateTime("06/11/2015 13:00:00"), fd.parseDateTime("06/11/2015")),
        TimetableEntry(Employee.randomUUID, Room.randomUUID, mi.id, fd.parseDateTime("06/11/2015"), ft.parseDateTime("06/11/2015 12:00:00"), ft.parseDateTime("06/11/2015 13:00:00"), fd.parseDateTime("06/11/2015")),
        TimetableEntry(Employee.randomUUID, Room.randomUUID, mi.id, fd.parseDateTime("06/11/2015"), ft.parseDateTime("06/11/2015 13:00:00"), ft.parseDateTime("06/11/2015 14:00:00"), fd.parseDateTime("06/11/2015")),
        TimetableEntry(Employee.randomUUID, Room.randomUUID, mi.id, fd.parseDateTime("06/11/2015"), ft.parseDateTime("06/11/2015 13:00:00"), ft.parseDateTime("06/11/2015 14:00:00"), fd.parseDateTime("06/11/2015"))
      )
      val ma1Entries = Set(
        TimetableEntry(Employee.randomUUID, Room.randomUUID, mi.id, fd.parseDateTime("26/10/2015"), ft.parseDateTime("26/10/2015 08:00:00"), ft.parseDateTime("26/10/2015 11:00:00"), fd.parseDateTime("26/10/2015")),
        TimetableEntry(Employee.randomUUID, Room.randomUUID, mi.id, fd.parseDateTime("27/10/2015"), ft.parseDateTime("27/10/2015 08:00:00"), ft.parseDateTime("27/10/2015 11:00:00"), fd.parseDateTime("27/10/2015")),
        TimetableEntry(Employee.randomUUID, Room.randomUUID, mi.id, fd.parseDateTime("29/10/2015"), ft.parseDateTime("29/10/2015 08:00:00"), ft.parseDateTime("29/10/2015 11:00:00"), fd.parseDateTime("29/10/2015")),
        TimetableEntry(Employee.randomUUID, Room.randomUUID, mi.id, fd.parseDateTime("29/10/2015"), ft.parseDateTime("29/10/2015 11:00:00"), ft.parseDateTime("29/10/2015 14:00:00"), fd.parseDateTime("29/10/2015")),
        TimetableEntry(Employee.randomUUID, Room.randomUUID, mi.id, fd.parseDateTime("30/10/2015"), ft.parseDateTime("30/10/2015 11:00:00"), ft.parseDateTime("30/10/2015 14:00:00"), fd.parseDateTime("30/10/2015")),
        TimetableEntry(Employee.randomUUID, Room.randomUUID, mi.id, fd.parseDateTime("30/10/2015"), ft.parseDateTime("30/10/2015 14:00:00"), ft.parseDateTime("30/10/2015 17:00:00"), fd.parseDateTime("30/10/2015")), // ======
        TimetableEntry(Employee.randomUUID, Room.randomUUID, mi.id, fd.parseDateTime("02/11/2015"), ft.parseDateTime("02/11/2015 08:00:00"), ft.parseDateTime("02/11/2015 11:00:00"), fd.parseDateTime("02/11/2015")),
        TimetableEntry(Employee.randomUUID, Room.randomUUID, mi.id, fd.parseDateTime("03/11/2015"), ft.parseDateTime("03/11/2015 08:00:00"), ft.parseDateTime("03/11/2015 11:00:00"), fd.parseDateTime("03/11/2015")),
        TimetableEntry(Employee.randomUUID, Room.randomUUID, mi.id, fd.parseDateTime("05/11/2015"), ft.parseDateTime("05/11/2015 08:00:00"), ft.parseDateTime("05/11/2015 11:00:00"), fd.parseDateTime("05/11/2015")),
        TimetableEntry(Employee.randomUUID, Room.randomUUID, mi.id, fd.parseDateTime("05/11/2015"), ft.parseDateTime("05/11/2015 11:00:00"), ft.parseDateTime("05/11/2015 14:00:00"), fd.parseDateTime("05/11/2015")),
        TimetableEntry(Employee.randomUUID, Room.randomUUID, mi.id, fd.parseDateTime("06/11/2015"), ft.parseDateTime("06/11/2015 11:00:00"), ft.parseDateTime("06/11/2015 14:00:00"), fd.parseDateTime("06/11/2015")),
        TimetableEntry(Employee.randomUUID, Room.randomUUID, mi.id, fd.parseDateTime("06/11/2015"), ft.parseDateTime("06/11/2015 14:00:00"), ft.parseDateTime("06/11/2015 17:00:00"), fd.parseDateTime("06/11/2015"))
      )

      val prep = helper.prepare(Vector(ap1Prak, ma1Prak), Vector(ap1Entries, ma1Entries))
      val ap = prep.head
      val ma = prep.last

      val comp = scheduleService.populate(1, ap._1, ap._2)
      val result = scheduleGenesisService.generate(ma1Prak.id, ma._1, ma._2, plan, comp)
      val eval = scheduleService.evaluate(result._1.elem, plan.numberOfEntries, comp)

      println(s"gen ${result._2}")
      println(s"conflict size ${eval.conflicts.size}")
      println(s"conflict value ${eval.value}")
      eval.conflicts shouldBe empty
      eval.value shouldBe 0

      result._2 should be >= 0
      result._1.evaluate.err shouldBe empty
      result._1.evaluate.value shouldBe 0
      result._1.elem.labwork shouldEqual ma1Prak.id
      result._1.elem.entries.groupBy(_.group) forall {
        case (_, ss) => ss.size == plan.numberOfEntries
      } shouldBe true
    }

    "generate yet another scheduleG with few populations and generations" in {
      val plan = AssignmentPlan(2, Set.empty[AssignmentEntry])
      val mi = Degree("mi", Degree.randomUUID)
      val ap1 = Course("ap1", "c1", Employee.randomUUID, 1, Course.randomUUID)
      val ma1 = Course("ma1", "c2", Employee.randomUUID, 1, Course.randomUUID)
      val gdvk = Course("gdvk", "c3", Employee.randomUUID, 1, Course.randomUUID)
      val semester1 = Semester("semester1", "start1", "end1", "exam1", Blacklist.empty, Semester.randomUUID)
      val ap1Prak = Labwork("ap1Prak", "desc1", semester1.id, ap1.id, mi.id, plan)
      val ma1Prak = Labwork("ma1Prak", "desc2", semester1.id, ma1.id, mi.id, plan)
      val gdvkPrak = Labwork("gdvkPrak", "desc3", semester1.id, ma1.id, mi.id, plan)

      val ap1Entries = Set(
        TimetableEntry(Employee.randomUUID, Room.randomUUID, mi.id, fd.parseDateTime("27/10/2015"), ft.parseDateTime("27/10/2015 08:00:00"), ft.parseDateTime("27/10/2015 09:00:00"), fd.parseDateTime("27/10/2015")),
        TimetableEntry(Employee.randomUUID, Room.randomUUID, mi.id, fd.parseDateTime("27/10/2015"), ft.parseDateTime("27/10/2015 09:00:00"), ft.parseDateTime("27/10/2015 10:00:00"), fd.parseDateTime("27/10/2015")),
        TimetableEntry(Employee.randomUUID, Room.randomUUID, mi.id, fd.parseDateTime("27/10/2015"), ft.parseDateTime("27/10/2015 10:00:00"), ft.parseDateTime("27/10/2015 11:00:00"), fd.parseDateTime("27/10/2015")),
        TimetableEntry(Employee.randomUUID, Room.randomUUID, mi.id, fd.parseDateTime("29/10/2015"), ft.parseDateTime("29/10/2015 14:00:00"), ft.parseDateTime("29/10/2015 15:00:00"), fd.parseDateTime("29/10/2015")),
        TimetableEntry(Employee.randomUUID, Room.randomUUID, mi.id, fd.parseDateTime("29/10/2015"), ft.parseDateTime("29/10/2015 14:00:00"), ft.parseDateTime("29/10/2015 15:00:00"), fd.parseDateTime("29/10/2015")),
        TimetableEntry(Employee.randomUUID, Room.randomUUID, mi.id, fd.parseDateTime("29/10/2015"), ft.parseDateTime("29/10/2015 15:00:00"), ft.parseDateTime("29/10/2015 16:00:00"), fd.parseDateTime("29/10/2015")),
        TimetableEntry(Employee.randomUUID, Room.randomUUID, mi.id, fd.parseDateTime("29/10/2015"), ft.parseDateTime("29/10/2015 15:00:00"), ft.parseDateTime("29/10/2015 16:00:00"), fd.parseDateTime("29/10/2015")),
        TimetableEntry(Employee.randomUUID, Room.randomUUID, mi.id, fd.parseDateTime("29/10/2015"), ft.parseDateTime("29/10/2015 16:00:00"), ft.parseDateTime("29/10/2015 17:00:00"), fd.parseDateTime("29/10/2015")),
        TimetableEntry(Employee.randomUUID, Room.randomUUID, mi.id, fd.parseDateTime("29/10/2015"), ft.parseDateTime("29/10/2015 16:00:00"), ft.parseDateTime("29/10/2015 17:00:00"), fd.parseDateTime("29/10/2015")),
        TimetableEntry(Employee.randomUUID, Room.randomUUID, mi.id, fd.parseDateTime("30/10/2015"), ft.parseDateTime("30/10/2015 08:00:00"), ft.parseDateTime("30/10/2015 09:00:00"), fd.parseDateTime("30/10/2015")),
        TimetableEntry(Employee.randomUUID, Room.randomUUID, mi.id, fd.parseDateTime("30/10/2015"), ft.parseDateTime("30/10/2015 08:00:00"), ft.parseDateTime("30/10/2015 09:00:00"), fd.parseDateTime("30/10/2015")),
        TimetableEntry(Employee.randomUUID, Room.randomUUID, mi.id, fd.parseDateTime("30/10/2015"), ft.parseDateTime("30/10/2015 09:00:00"), ft.parseDateTime("30/10/2015 10:00:00"), fd.parseDateTime("30/10/2015")),
        TimetableEntry(Employee.randomUUID, Room.randomUUID, mi.id, fd.parseDateTime("30/10/2015"), ft.parseDateTime("30/10/2015 09:00:00"), ft.parseDateTime("30/10/2015 10:00:00"), fd.parseDateTime("30/10/2015")),
        TimetableEntry(Employee.randomUUID, Room.randomUUID, mi.id, fd.parseDateTime("30/10/2015"), ft.parseDateTime("30/10/2015 10:00:00"), ft.parseDateTime("30/10/2015 11:00:00"), fd.parseDateTime("30/10/2015")),
        TimetableEntry(Employee.randomUUID, Room.randomUUID, mi.id, fd.parseDateTime("30/10/2015"), ft.parseDateTime("30/10/2015 10:00:00"), ft.parseDateTime("30/10/2015 11:00:00"), fd.parseDateTime("30/10/2015")),
        TimetableEntry(Employee.randomUUID, Room.randomUUID, mi.id, fd.parseDateTime("30/10/2015"), ft.parseDateTime("30/10/2015 11:00:00"), ft.parseDateTime("30/10/2015 12:00:00"), fd.parseDateTime("30/10/2015")),
        TimetableEntry(Employee.randomUUID, Room.randomUUID, mi.id, fd.parseDateTime("30/10/2015"), ft.parseDateTime("30/10/2015 11:00:00"), ft.parseDateTime("30/10/2015 12:00:00"), fd.parseDateTime("30/10/2015")),
        TimetableEntry(Employee.randomUUID, Room.randomUUID, mi.id, fd.parseDateTime("30/10/2015"), ft.parseDateTime("30/10/2015 12:00:00"), ft.parseDateTime("30/10/2015 13:00:00"), fd.parseDateTime("30/10/2015")),
        TimetableEntry(Employee.randomUUID, Room.randomUUID, mi.id, fd.parseDateTime("30/10/2015"), ft.parseDateTime("30/10/2015 12:00:00"), ft.parseDateTime("30/10/2015 13:00:00"), fd.parseDateTime("30/10/2015")),
        TimetableEntry(Employee.randomUUID, Room.randomUUID, mi.id, fd.parseDateTime("30/10/2015"), ft.parseDateTime("30/10/2015 13:00:00"), ft.parseDateTime("30/10/2015 14:00:00"), fd.parseDateTime("30/10/2015")),
        TimetableEntry(Employee.randomUUID, Room.randomUUID, mi.id, fd.parseDateTime("30/10/2015"), ft.parseDateTime("30/10/2015 13:00:00"), ft.parseDateTime("30/10/2015 14:00:00"), fd.parseDateTime("30/10/2015")), // ======
        TimetableEntry(Employee.randomUUID, Room.randomUUID, mi.id, fd.parseDateTime("03/11/2015"), ft.parseDateTime("03/11/2015 08:00:00"), ft.parseDateTime("03/11/2015 09:00:00"), fd.parseDateTime("03/11/2015")),
        TimetableEntry(Employee.randomUUID, Room.randomUUID, mi.id, fd.parseDateTime("03/11/2015"), ft.parseDateTime("03/11/2015 09:00:00"), ft.parseDateTime("03/11/2015 10:00:00"), fd.parseDateTime("03/11/2015")),
        TimetableEntry(Employee.randomUUID, Room.randomUUID, mi.id, fd.parseDateTime("03/11/2015"), ft.parseDateTime("03/11/2015 10:00:00"), ft.parseDateTime("03/11/2015 11:00:00"), fd.parseDateTime("03/11/2015")),
        TimetableEntry(Employee.randomUUID, Room.randomUUID, mi.id, fd.parseDateTime("05/11/2015"), ft.parseDateTime("05/11/2015 14:00:00"), ft.parseDateTime("05/11/2015 15:00:00"), fd.parseDateTime("05/11/2015")),
        TimetableEntry(Employee.randomUUID, Room.randomUUID, mi.id, fd.parseDateTime("05/11/2015"), ft.parseDateTime("05/11/2015 14:00:00"), ft.parseDateTime("05/11/2015 15:00:00"), fd.parseDateTime("05/11/2015")),
        TimetableEntry(Employee.randomUUID, Room.randomUUID, mi.id, fd.parseDateTime("05/11/2015"), ft.parseDateTime("05/11/2015 15:00:00"), ft.parseDateTime("05/11/2015 16:00:00"), fd.parseDateTime("05/11/2015")),
        TimetableEntry(Employee.randomUUID, Room.randomUUID, mi.id, fd.parseDateTime("05/11/2015"), ft.parseDateTime("05/11/2015 15:00:00"), ft.parseDateTime("05/11/2015 16:00:00"), fd.parseDateTime("05/11/2015")),
        TimetableEntry(Employee.randomUUID, Room.randomUUID, mi.id, fd.parseDateTime("05/11/2015"), ft.parseDateTime("05/11/2015 16:00:00"), ft.parseDateTime("05/11/2015 17:00:00"), fd.parseDateTime("05/11/2015")),
        TimetableEntry(Employee.randomUUID, Room.randomUUID, mi.id, fd.parseDateTime("05/11/2015"), ft.parseDateTime("05/11/2015 16:00:00"), ft.parseDateTime("05/11/2015 17:00:00"), fd.parseDateTime("05/11/2015")),
        TimetableEntry(Employee.randomUUID, Room.randomUUID, mi.id, fd.parseDateTime("06/11/2015"), ft.parseDateTime("06/11/2015 08:00:00"), ft.parseDateTime("06/11/2015 09:00:00"), fd.parseDateTime("06/11/2015")),
        TimetableEntry(Employee.randomUUID, Room.randomUUID, mi.id, fd.parseDateTime("06/11/2015"), ft.parseDateTime("06/11/2015 08:00:00"), ft.parseDateTime("06/11/2015 09:00:00"), fd.parseDateTime("06/11/2015")),
        TimetableEntry(Employee.randomUUID, Room.randomUUID, mi.id, fd.parseDateTime("06/11/2015"), ft.parseDateTime("06/11/2015 09:00:00"), ft.parseDateTime("06/11/2015 10:00:00"), fd.parseDateTime("06/11/2015")),
        TimetableEntry(Employee.randomUUID, Room.randomUUID, mi.id, fd.parseDateTime("06/11/2015"), ft.parseDateTime("06/11/2015 09:00:00"), ft.parseDateTime("06/11/2015 10:00:00"), fd.parseDateTime("06/11/2015")),
        TimetableEntry(Employee.randomUUID, Room.randomUUID, mi.id, fd.parseDateTime("06/11/2015"), ft.parseDateTime("06/11/2015 10:00:00"), ft.parseDateTime("06/11/2015 11:00:00"), fd.parseDateTime("06/11/2015")),
        TimetableEntry(Employee.randomUUID, Room.randomUUID, mi.id, fd.parseDateTime("06/11/2015"), ft.parseDateTime("06/11/2015 10:00:00"), ft.parseDateTime("06/11/2015 11:00:00"), fd.parseDateTime("06/11/2015")),
        TimetableEntry(Employee.randomUUID, Room.randomUUID, mi.id, fd.parseDateTime("06/11/2015"), ft.parseDateTime("06/11/2015 11:00:00"), ft.parseDateTime("06/11/2015 12:00:00"), fd.parseDateTime("06/11/2015")),
        TimetableEntry(Employee.randomUUID, Room.randomUUID, mi.id, fd.parseDateTime("06/11/2015"), ft.parseDateTime("06/11/2015 11:00:00"), ft.parseDateTime("06/11/2015 12:00:00"), fd.parseDateTime("06/11/2015")),
        TimetableEntry(Employee.randomUUID, Room.randomUUID, mi.id, fd.parseDateTime("06/11/2015"), ft.parseDateTime("06/11/2015 12:00:00"), ft.parseDateTime("06/11/2015 13:00:00"), fd.parseDateTime("06/11/2015")),
        TimetableEntry(Employee.randomUUID, Room.randomUUID, mi.id, fd.parseDateTime("06/11/2015"), ft.parseDateTime("06/11/2015 12:00:00"), ft.parseDateTime("06/11/2015 13:00:00"), fd.parseDateTime("06/11/2015")),
        TimetableEntry(Employee.randomUUID, Room.randomUUID, mi.id, fd.parseDateTime("06/11/2015"), ft.parseDateTime("06/11/2015 13:00:00"), ft.parseDateTime("06/11/2015 14:00:00"), fd.parseDateTime("06/11/2015")),
        TimetableEntry(Employee.randomUUID, Room.randomUUID, mi.id, fd.parseDateTime("06/11/2015"), ft.parseDateTime("06/11/2015 13:00:00"), ft.parseDateTime("06/11/2015 14:00:00"), fd.parseDateTime("06/11/2015"))
      )
      val ma1Entries = Set(
        TimetableEntry(Employee.randomUUID, Room.randomUUID, mi.id, fd.parseDateTime("26/10/2015"), ft.parseDateTime("26/10/2015 08:00:00"), ft.parseDateTime("26/10/2015 11:00:00"), fd.parseDateTime("26/10/2015")),
        TimetableEntry(Employee.randomUUID, Room.randomUUID, mi.id, fd.parseDateTime("27/10/2015"), ft.parseDateTime("27/10/2015 08:00:00"), ft.parseDateTime("27/10/2015 11:00:00"), fd.parseDateTime("27/10/2015")),
        TimetableEntry(Employee.randomUUID, Room.randomUUID, mi.id, fd.parseDateTime("29/10/2015"), ft.parseDateTime("29/10/2015 08:00:00"), ft.parseDateTime("29/10/2015 11:00:00"), fd.parseDateTime("29/10/2015")),
        TimetableEntry(Employee.randomUUID, Room.randomUUID, mi.id, fd.parseDateTime("29/10/2015"), ft.parseDateTime("29/10/2015 11:00:00"), ft.parseDateTime("29/10/2015 14:00:00"), fd.parseDateTime("29/10/2015")),
        TimetableEntry(Employee.randomUUID, Room.randomUUID, mi.id, fd.parseDateTime("30/10/2015"), ft.parseDateTime("30/10/2015 11:00:00"), ft.parseDateTime("30/10/2015 14:00:00"), fd.parseDateTime("30/10/2015")),
        TimetableEntry(Employee.randomUUID, Room.randomUUID, mi.id, fd.parseDateTime("30/10/2015"), ft.parseDateTime("30/10/2015 14:00:00"), ft.parseDateTime("30/10/2015 17:00:00"), fd.parseDateTime("30/10/2015")), // ======
        TimetableEntry(Employee.randomUUID, Room.randomUUID, mi.id, fd.parseDateTime("02/11/2015"), ft.parseDateTime("02/11/2015 08:00:00"), ft.parseDateTime("02/11/2015 11:00:00"), fd.parseDateTime("02/11/2015")),
        TimetableEntry(Employee.randomUUID, Room.randomUUID, mi.id, fd.parseDateTime("03/11/2015"), ft.parseDateTime("03/11/2015 08:00:00"), ft.parseDateTime("03/11/2015 11:00:00"), fd.parseDateTime("03/11/2015")),
        TimetableEntry(Employee.randomUUID, Room.randomUUID, mi.id, fd.parseDateTime("05/11/2015"), ft.parseDateTime("05/11/2015 08:00:00"), ft.parseDateTime("05/11/2015 11:00:00"), fd.parseDateTime("05/11/2015")),
        TimetableEntry(Employee.randomUUID, Room.randomUUID, mi.id, fd.parseDateTime("05/11/2015"), ft.parseDateTime("05/11/2015 11:00:00"), ft.parseDateTime("05/11/2015 14:00:00"), fd.parseDateTime("05/11/2015")),
        TimetableEntry(Employee.randomUUID, Room.randomUUID, mi.id, fd.parseDateTime("06/11/2015"), ft.parseDateTime("06/11/2015 11:00:00"), ft.parseDateTime("06/11/2015 14:00:00"), fd.parseDateTime("06/11/2015")),
        TimetableEntry(Employee.randomUUID, Room.randomUUID, mi.id, fd.parseDateTime("06/11/2015"), ft.parseDateTime("06/11/2015 14:00:00"), ft.parseDateTime("06/11/2015 17:00:00"), fd.parseDateTime("06/11/2015"))
      )
      val gdvkEntries = Set(
        TimetableEntry(Employee.randomUUID, Room.randomUUID, mi.id, fd.parseDateTime("30/10/2015"), ft.parseDateTime("30/10/2015 08:00:00"), ft.parseDateTime("30/10/2015 11:00:00"), fd.parseDateTime("30/10/2015")),
        TimetableEntry(Employee.randomUUID, Room.randomUUID, mi.id, fd.parseDateTime("30/10/2015"), ft.parseDateTime("30/10/2015 11:00:00"), ft.parseDateTime("30/10/2015 13:00:00"), fd.parseDateTime("30/10/2015")),
        TimetableEntry(Employee.randomUUID, Room.randomUUID, mi.id, fd.parseDateTime("30/10/2015"), ft.parseDateTime("30/10/2015 14:00:00"), ft.parseDateTime("30/10/2015 17:00:00"), fd.parseDateTime("30/10/2015")), // ======
        TimetableEntry(Employee.randomUUID, Room.randomUUID, mi.id, fd.parseDateTime("06/11/2015"), ft.parseDateTime("06/11/2015 08:00:00"), ft.parseDateTime("06/11/2015 11:00:00"), fd.parseDateTime("06/11/2015")),
        TimetableEntry(Employee.randomUUID, Room.randomUUID, mi.id, fd.parseDateTime("06/11/2015"), ft.parseDateTime("06/11/2015 11:00:00"), ft.parseDateTime("06/11/2015 13:00:00"), fd.parseDateTime("06/11/2015")),
        TimetableEntry(Employee.randomUUID, Room.randomUUID, mi.id, fd.parseDateTime("06/11/2015"), ft.parseDateTime("06/11/2015 14:00:00"), ft.parseDateTime("06/11/2015 17:00:00"), fd.parseDateTime("06/11/2015"))
      )

      val prep = helper.prepare(Vector(ap1Prak, ma1Prak, gdvkPrak), Vector(ap1Entries, ma1Entries, gdvkEntries))
      val apS = prep(0)
      val maS = prep(1)
      val gdvkS = prep.last

      val comp = scheduleService.populate(1, apS._1, apS._2) ++ scheduleService.populate(1, maS._1, maS._2)
      val result = scheduleGenesisService.generate(gdvk.id, gdvkS._1, gdvkS._2, plan, comp)
      val eval = scheduleService.evaluate(result._1.elem, plan.numberOfEntries, comp)

      println(s"gen ${result._2}")
      println(s"conflict size ${eval.conflicts.size}")
      println(s"conflict value ${eval.value}")
      eval.conflicts shouldBe empty
      eval.value shouldBe 0

      result._2 should be > 0
      result._1.evaluate.err shouldBe empty
      result._1.evaluate.value shouldBe 0
      result._1.elem.labwork shouldEqual gdvkPrak.id
      result._1.elem.entries.groupBy(_.group) forall {
        case (_, ss) => ss.size == plan.numberOfEntries
      } shouldBe true
    }
  }
}
