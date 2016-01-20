package services

import base.TestBaseDefinition
import models.schedule.{Timetable, TimetableEntry}
import models.users.{Student, Employee}
import models._
import org.joda.time.DateTime
import org.joda.time.format.DateTimeFormat
import org.scalatest.WordSpec
import store.bind.Bindings
import store.{SesameRepository, Namespace}
import utils.Genesis

class ScheduleSpecHelper(private val scheduleService: ScheduleService) {

  def schedules(labworks: Vector[Labwork], entries: Vector[Set[TimetableEntry]], pop: Int): Vector[Vector[ScheduleG]] = {
    import scala.util.Random._

    val numberOfGroups = entries.map(_.size).max / labworks.map(_.assignmentPlan.numberOfEntries).max
    val groupSize = 10
    val students = (0 until numberOfGroups * groupSize).map(_ => Student.randomUUID).toVector

    labworks.zip(entries).map {
      case (l, e) =>
        val count = e.size / l.assignmentPlan.numberOfEntries
        val g = shuffle(students).take(count * groupSize).grouped(groupSize).map(s => Group("", l.id, s.toSet, Group.randomUUID)).toSet
        val t = Timetable(l.id, e, DateTime.now, Set.empty[DateTime], 0, Timetable.randomUUID)
        scheduleService.populate(pop, t, g)
    }
  }
}

class ScheduleGenesisSpec extends WordSpec with TestBaseDefinition {

  val ns = Namespace("http://lwm.gm.fh-koeln.de/")
  val repo = SesameRepository(ns)
  import repo._
  val bindings = Bindings(ns)

  val scheduleService = new ScheduleService(repo)
  val helper = new ScheduleSpecHelper(scheduleService)

  "A ScheduleGenesisSpec" should {

    "generate an initial scheduleG with one population and generation" in {
      import bindings.AssignmentPlanBinding._
      import bindings.CourseBinding._
      import bindings.DegreeBinding._
      import bindings.SemesterBinding._
      import bindings.LabworkBinding._

      val plan = AssignmentPlan(2, Set.empty[AssignmentEntry])
      val mi = Degree("mi", Degree.randomUUID)
      val ap1 = Course("ap1", "c1", Employee.randomUUID, 1, Course.randomUUID)
      val semester1 = Semester("semester1", "start1", "end1", "exam1", Semester.randomUUID)
      val ap1Prak = Labwork("ap1Prak", "desc1", semester1.id, ap1.id, mi.id, plan)

      val ft = DateTimeFormat.forPattern("dd/MM/yyyy HH:mm:ss")
      val fd = DateTimeFormat.forPattern("dd/MM/yyyy")
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

      val pops = helper.schedules(Vector(ap1Prak), Vector(ap1Entries), 1).head

      repo add mi
      repo add ap1
      repo add semester1
      repo add ap1Prak

      implicit val evaluation = scheduleService.eval(Vector.empty[ScheduleG], plan.numberOfEntries)
      implicit val mutate = scheduleService.mut
      implicit val cross = scheduleService.cross
      import utils.TypeClasses.instances._
      import utils.Ops.MonadInstances.intM

      val result = Genesis.measureByTaking[ScheduleG, Conflict, Int](pops, 1)

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
      import bindings.AssignmentPlanBinding._
      import bindings.CourseBinding._
      import bindings.DegreeBinding._
      import bindings.SemesterBinding._
      import bindings.LabworkBinding._
      import bindings.ScheduleBinding._
      import bindings.GroupBinding._

      val plan = AssignmentPlan(2, Set.empty[AssignmentEntry])
      val mi = Degree("mi", Degree.randomUUID)
      val ap1 = Course("ap1", "c1", Employee.randomUUID, 1, Course.randomUUID)
      val ma1 = Course("ma1", "c2", Employee.randomUUID, 1, Course.randomUUID)
      val semester1 = Semester("semester1", "start1", "end1", "exam1", Semester.randomUUID)
      val ap1Prak = Labwork("ap1Prak", "desc1", semester1.id, ap1.id, mi.id, plan)
      val ma1Prak = Labwork("ma1Prak", "desc2", semester1.id, ma1.id, mi.id, plan)

      val ft = DateTimeFormat.forPattern("dd/MM/yyyy HH:mm:ss")
      val fd = DateTimeFormat.forPattern("dd/MM/yyyy")
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

      val pops = helper.schedules(Vector(ap1Prak, ma1Prak), Vector(ap1Entries, ma1Entries), 2)
      val ap = pops.head.head
      val ma = pops.last

      repo add mi
      repo addMany Vector(ap1, ma1)
      repo add semester1
      repo addMany Vector(ap1Prak, ma1Prak)
      repo addMany ap.entries.map(_.group)
      repo add scheduleService.toSchedule(ap)

      val all = scheduleService.competitive(ma1Prak.id).get.get
      val app = scheduleService.appointments(ma1Prak.id).get.get

      implicit val evaluation = scheduleService.eval(all, app)
      implicit val mutate = scheduleService.mut
      implicit val cross = scheduleService.cross
      import utils.TypeClasses.instances._
      import utils.Ops.MonadInstances.intM

      val result = Genesis.measureByTaking[ScheduleG, Conflict, Int](ma, 50)

      val a = scheduleService.evaluate(result._1.elem, app, all)
      println(s"gen ${result._2}")
      println(s"conflict size ${a.conflicts.size}")
      println(s"conflict value ${a.value}")
      a.conflicts shouldBe empty
      a.value shouldBe 0

      result._2 should be > 0
      result._1.evaluate.err shouldBe empty
      result._1.evaluate.value shouldBe 0
      result._1.elem.labwork shouldEqual ma1Prak.id
      result._1.elem.entries.groupBy(_.group) forall {
        case (_, ss) => ss.size == app
      } shouldBe true
    }

    "generate yet another scheduleG with few populations and generations" in {
      import bindings.AssignmentPlanBinding._
      import bindings.CourseBinding._
      import bindings.DegreeBinding._
      import bindings.SemesterBinding._
      import bindings.LabworkBinding._
      import bindings.ScheduleBinding._
      import bindings.GroupBinding._

      val plan = AssignmentPlan(2, Set.empty[AssignmentEntry])
      val mi = Degree("mi", Degree.randomUUID)
      val ap1 = Course("ap1", "c1", Employee.randomUUID, 1, Course.randomUUID)
      val ma1 = Course("ma1", "c2", Employee.randomUUID, 1, Course.randomUUID)
      val gdvk = Course("gdvk", "c3", Employee.randomUUID, 1, Course.randomUUID)
      val semester1 = Semester("semester1", "start1", "end1", "exam1", Semester.randomUUID)
      val ap1Prak = Labwork("ap1Prak", "desc1", semester1.id, ap1.id, mi.id, plan)
      val ma1Prak = Labwork("ma1Prak", "desc2", semester1.id, ma1.id, mi.id, plan)
      val gdvkPrak = Labwork("gdvkPrak", "desc3", semester1.id, ma1.id, mi.id, plan)

      val ft = DateTimeFormat.forPattern("dd/MM/yyyy HH:mm:ss")
      val fd = DateTimeFormat.forPattern("dd/MM/yyyy")
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

      val pops = helper.schedules(Vector(ap1Prak, ma1Prak, gdvkPrak), Vector(ap1Entries, ma1Entries, gdvkEntries), 10)
      val apS = pops(0).head
      val maS = pops(1).head
      val gdvkS = pops.last

      repo add mi
      repo addMany Vector(ap1, ma1, gdvk)
      repo add semester1
      repo addMany Vector(ap1Prak, ma1Prak, gdvkPrak)
      repo addMany apS.entries.map(_.group)
      repo addMany maS.entries.map(_.group)
      repo addMany Vector(apS, maS).map(scheduleService.toSchedule)

      val all = scheduleService.competitive(gdvkPrak.id).get.get
      val app = scheduleService.appointments(gdvkPrak.id).get.get

      implicit val evaluation = scheduleService.eval(all, app)
      implicit val mutate = scheduleService.mut
      implicit val cross = scheduleService.cross
      import utils.TypeClasses.instances._
      import utils.Ops.MonadInstances.intM

      val result = Genesis.measureByTaking[ScheduleG, Conflict, Int](gdvkS, 100)

      val a = scheduleService.evaluate(result._1.elem, app, all)
      println(s"gen ${result._2}")
      println(s"conflict size ${a.conflicts.size}")
      println(s"conflict value ${a.value}")
      a.conflicts shouldBe empty
      a.value shouldBe 0

      result._2 should be > 0
      result._1.evaluate.err shouldBe empty
      result._1.evaluate.value shouldBe 0
      result._1.elem.labwork shouldEqual gdvkPrak.id
      result._1.elem.entries.groupBy(_.group) forall {
        case (_, ss) => ss.size == app
      } shouldBe true
    }
  }

  override protected def beforeEach(): Unit = {
    repo.withConnection { conn =>
      repo.rdfStore.removeGraph(conn, repo.ns)
    }
  }
}
