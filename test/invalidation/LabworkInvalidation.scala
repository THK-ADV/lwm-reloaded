package invalidation

import java.util.UUID

import base.SesameDbSpec
import models._
import org.joda.time.{LocalDate, LocalTime}

import scala.util.Random._
import scala.util.{Failure, Success}

class LabworkInvalidation extends SesameDbSpec {

  "A labwork invalidation" should {
    def tte: Stream[SesameTimetableEntry] = Stream.continually(SesameTimetableEntry(Set(User.randomUUID), SesameRoom.randomUUID, 1, LocalTime.now, LocalTime.now plusHours 2))

    def tt(labwork: UUID): Stream[SesameTimetable] = Stream.continually {
      if (nextBoolean()) SesameTimetable(labwork, (tte take 20).toSet, LocalDate.now, Set())
      else SesameTimetable(SesameLabwork.randomUUID, (tte take 20).toSet, LocalDate.now, Set())
    }

    def aplans(labwork: UUID): Stream[SesameAssignmentPlan] = Stream.continually {
      if (nextBoolean()) SesameAssignmentPlan(labwork, 1, 2, Set())
      else SesameAssignmentPlan(SesameLabwork.randomUUID, 1, 2, Set())
    }

    def grps(labwork: UUID): Stream[Group] = Stream.continually {
      if (nextBoolean()) Group("Label", labwork, Set())
      else Group("Label", SesameLabwork.randomUUID, Set())
    }

    def scheds(labwork: UUID): Stream[Schedule] = Stream.continually {
      if (nextBoolean()) Schedule(labwork, Set())
      else Schedule(SesameLabwork.randomUUID, Set())
    }

    def rce(labwork: UUID): Stream[SesameReportCardEntry] = Stream.continually {
      if (nextBoolean()) SesameReportCardEntry(User.randomUUID, labwork, "Label", LocalDate.now, LocalTime.now, LocalTime.now plusHours 2, SesameRoom.randomUUID, Set())
      else SesameReportCardEntry(User.randomUUID, SesameLabwork.randomUUID, "Label", LocalDate.now, LocalTime.now, LocalTime.now plusHours 2, SesameRoom.randomUUID, Set())
    }

    def rceval(labwork: UUID): Stream[SesameReportCardEvaluation] = Stream.continually {
      if (nextBoolean()) SesameReportCardEvaluation(User.randomUUID, labwork, "Label", bool = true, 0)
      else SesameReportCardEvaluation(User.randomUUID, SesameLabwork.randomUUID, "Label", bool = true, 0)
    }

    def labapp(labwork: UUID): Stream[SesameLabworkApplication] = Stream.continually {
      if (nextBoolean()) SesameLabworkApplication(labwork, User.randomUUID, Set())
      else SesameLabworkApplication(SesameLabwork.randomUUID, User.randomUUID, Set())
    }

    def annot(labwork: UUID): Stream[Annotation] = Stream.continually {
      if (nextBoolean()) Annotation(User.randomUUID, labwork, SesameReportCardEntry.randomUUID, "Message")
      else Annotation(User.randomUUID, SesameLabwork.randomUUID, SesameReportCardEntry.randomUUID, "Message")
    }

    "invalidate the labwork and subsequent assignment plans, groups, schedules and others" in {
      import bindings.{AnnotationDescriptor, AssignmentPlanDescriptor, GroupDescriptor, LabworkApplicationDescriptor, LabworkDescriptor, ReportCardEntryDescriptor, ReportCardEvaluationDescriptor, ScheduleDescriptor, TimetableDescriptor}

      val labwork = SesameLabwork("Label", "Desc", SesameSemester.randomUUID, SesameCourse.randomUUID, UUID.randomUUID)

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

      repo.add[SesameLabwork](labwork)
      repo.addMany[SesameAssignmentPlan](assPlans)
      repo.addMany[Group](groups)
      repo.addMany[Schedule](schedules)
      repo.addMany[SesameReportCardEntry](reportCardEntries)
      repo.addMany[SesameReportCardEvaluation](reportCardEvaluations)
      repo.addMany[SesameTimetable](timetables)
      repo.addMany[SesameLabworkApplication](applications)
      repo.addMany[Annotation](annotations)

      repo.invalidate[SesameLabwork](SesameLabwork.generateUri(labwork))

      repo.get[SesameLabwork](SesameLabwork.generateUri(labwork)) shouldBe Success(None)
      repo.getAll[SesameAssignmentPlan] shouldBe Success(assPlans filterNot (_.labwork == labwork.id))
      repo.getAll[Group] shouldBe Success(groups filterNot (_.labwork == labwork.id))
      repo.getAll[Schedule] shouldBe Success(schedules filterNot (_.labwork == labwork.id))
      repo.getAll[SesameReportCardEntry] shouldBe Success(reportCardEntries filterNot (_.labwork == labwork.id))
      repo.getAll[SesameTimetable] shouldBe Success(timetables filterNot (_.labwork == labwork.id))
      repo.getAll[SesameLabworkApplication] match {
        case Success(set) =>
          set.toVector.sortBy(_.applicant) shouldBe refApps.toVector.sortBy(_.applicant)
        case Failure(e) => fail("no")
      }
      repo.getAll[Annotation] match {
        case Success(set) =>
          set.toVector.sortBy(_.student) shouldBe refAnnots.toVector.sortBy(_.student)
        case Failure(e) => fail("no")
      }

      repo.deepGet[SesameLabwork](SesameLabwork.generateUri(labwork)) map (_ map (_.id)) shouldBe Success(Some(labwork.id))
      repo.deepGetAll[SesameAssignmentPlan] map (_ map (_.id)) shouldBe Success(assPlans map (_.id))
      repo.deepGetAll[Group] map (_ map (_.id)) shouldBe Success(groups map (_.id))
      repo.deepGetAll[Schedule] map (_ map (_.id)) shouldBe Success(schedules map (_.id))
      repo.deepGetAll[SesameReportCardEntry] map (_ map (_.id)) shouldBe Success(reportCardEntries map (_.id))
      repo.deepGetAll[SesameTimetable] map (_ map (_.id)) shouldBe Success(timetables map (_.id))
      repo.deepGetAll[SesameLabworkApplication] map (_ map (_.id)) shouldBe Success(applications map (_.id))
      repo.deepGetAll[Annotation] map (_ map (_.id)) shouldBe Success(annotations map (_.id))
    }
  }
}
