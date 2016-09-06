package invalidation.labwork

import java.util.UUID

import base.SesameDbSpec
import models.labwork._
import models.semester.Semester
import models.users.User
import models.{Course, Degree, Room}
import org.joda.time.{LocalDate, LocalTime}

import scala.util.Random._
import scala.util.{Failure, Success}

class LabworkInvalidation extends SesameDbSpec {

  "A labwork invalidation" should {
    def tte: Stream[TimetableEntry] = Stream.continually(TimetableEntry(Set(User.randomUUID), Room.randomUUID, 1, LocalTime.now, LocalTime.now plusHours 2))

    def tt(labwork: UUID): Stream[Timetable] = Stream.continually {
      if (nextBoolean()) Timetable(labwork, (tte take 20).toSet, LocalDate.now, Set())
      else Timetable(Labwork.randomUUID, (tte take 20).toSet, LocalDate.now, Set())
    }

    def aplans(labwork: UUID): Stream[AssignmentPlan] = Stream.continually {
      if (nextBoolean()) AssignmentPlan(labwork, 1, 2, Set())
      else AssignmentPlan(Labwork.randomUUID, 1, 2, Set())
    }

    def grps(labwork: UUID): Stream[Group] = Stream.continually {
      if (nextBoolean()) Group("Label", labwork, Set())
      else Group("Label", Labwork.randomUUID, Set())
    }

    def scheds(labwork: UUID): Stream[Schedule] = Stream.continually {
      if (nextBoolean()) Schedule(labwork, Set())
      else Schedule(Labwork.randomUUID, Set())
    }

    def rce(labwork: UUID): Stream[ReportCardEntry] = Stream.continually {
      if (nextBoolean()) ReportCardEntry(User.randomUUID, labwork, "Label", LocalDate.now, LocalTime.now, LocalTime.now plusHours 2, Room.randomUUID, Set())
      else ReportCardEntry(User.randomUUID, Labwork.randomUUID, "Label", LocalDate.now, LocalTime.now, LocalTime.now plusHours 2, Room.randomUUID, Set())
    }

    def rceval(labwork: UUID): Stream[ReportCardEvaluation] = Stream.continually {
      if (nextBoolean()) ReportCardEvaluation(User.randomUUID, labwork, "Label", bool = true, 0)
      else ReportCardEvaluation(User.randomUUID, Labwork.randomUUID, "Label", bool = true, 0)
    }

    def labapp(labwork: UUID): Stream[LabworkApplication] = Stream.continually {
      if (nextBoolean()) LabworkApplication(labwork, User.randomUUID, Set())
      else LabworkApplication(Labwork.randomUUID, User.randomUUID, Set())
    }

    def annot(labwork: UUID): Stream[Annotation] = Stream.continually {
      if (nextBoolean()) Annotation(User.randomUUID, labwork, ReportCardEntry.randomUUID, "Message")
      else Annotation(User.randomUUID, Labwork.randomUUID, ReportCardEntry.randomUUID, "Message")
    }

    "invalidate the labwork and subsequent assignment plans, groups, schedules and others" in {
      import bindings.{AnnotationDescriptor, AssignmentPlanDescriptor, GroupDescriptor, LabworkApplicationDescriptor, LabworkDescriptor, ReportCardEntryDescriptor, ReportCardEvaluationDescriptor, ScheduleDescriptor, TimetableDescriptor}

      val labwork = Labwork("Label", "Desc", Semester.randomUUID, Course.randomUUID, Degree.randomUUID)

      val assPlans = (aplans(labwork.id) take 20).toSet
      val groups = (grps(labwork.id) take 20).toSet
      val schedules = (scheds(labwork.id) take 20).toSet
      val reportCardEntries = (rce(labwork.id) take 20).toSet
      val reportCardEvaluations = (rceval(labwork.id) take 20).toSet
      val timetables = (tt(labwork.id) take 10).toSet
      val applications = (labapp(labwork.id) take 20).toSet
      val refApps = applications filterNot (_.labwork == labwork.id)
      val annotations = (annot(labwork.id) take 20).toSet
      val refAnnots = annotations filterNot (_.labwork == labwork.id)

      repo.add[Labwork](labwork)
      repo.addMany[AssignmentPlan](assPlans)
      repo.addMany[Group](groups)
      repo.addMany[Schedule](schedules)
      repo.addMany[ReportCardEntry](reportCardEntries)
      repo.addMany[ReportCardEvaluation](reportCardEvaluations)
      repo.addMany[Timetable](timetables)
      repo.addMany[LabworkApplication](applications)
      repo.addMany[Annotation](annotations)

      repo.invalidate[Labwork](Labwork.generateUri(labwork))

      repo.get[Labwork](Labwork.generateUri(labwork)) shouldBe Success(None)
      repo.getAll[AssignmentPlan] shouldBe Success(assPlans filterNot (_.labwork == labwork.id))
      repo.getAll[Group] shouldBe Success(groups filterNot (_.labwork == labwork.id))
      repo.getAll[Schedule] shouldBe Success(schedules filterNot (_.labwork == labwork.id))
      repo.getAll[ReportCardEntry] shouldBe Success(reportCardEntries filterNot (_.labwork == labwork.id))
      repo.getAll[Timetable] shouldBe Success(timetables filterNot (_.labwork == labwork.id))
      repo.getAll[LabworkApplication] match {
        case Success(set) =>
          set.toVector.sortBy(_.applicant) shouldBe refApps.toVector.sortBy(_.applicant)
        case Failure(e) => fail("no")
      }
      repo.getAll[Annotation] match {
        case Success(set) =>
          set.toVector.sortBy(_.student) shouldBe refAnnots.toVector.sortBy(_.student)
        case Failure(e) => fail("no")
      }

      repo.deepGet[Labwork](Labwork.generateUri(labwork)) map (_ map (_.id)) shouldBe Success(Some(labwork.id))
      repo.deepGetAll[AssignmentPlan] map (_ map (_.id)) shouldBe Success(assPlans map (_.id))
      repo.deepGetAll[Group] map (_ map (_.id)) shouldBe Success(groups map (_.id))
      repo.deepGetAll[Schedule] map (_ map (_.id)) shouldBe Success(schedules map (_.id))
      repo.deepGetAll[ReportCardEntry] map (_ map (_.id)) shouldBe Success(reportCardEntries map (_.id))
      repo.deepGetAll[Timetable] map (_ map (_.id)) shouldBe Success(timetables map (_.id))
      repo.deepGetAll[LabworkApplication] map (_ map (_.id)) shouldBe Success(applications map (_.id))
      repo.deepGetAll[Annotation] map (_ map (_.id)) shouldBe Success(annotations map (_.id))
    }
  }
}
