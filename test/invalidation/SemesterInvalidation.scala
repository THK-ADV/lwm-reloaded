package invalidation

import java.util.UUID

import base.SesameDbSpec
import models._
import org.joda.time.{LocalDate, LocalTime}

import scala.util.Random._
import scala.util.{Failure, Success}

class SemesterInvalidation extends SesameDbSpec {

  "A Semester invalidation" should {

    def labs(semester: UUID): Stream[SesameLabwork] = Stream.continually {
      if (nextBoolean()) SesameLabwork("Label", "Desc", semester, SesameCourse.randomUUID, PostgresDegree.randomUUID)
      else SesameLabwork("Label", "Desc", SesameSemester.randomUUID, SesameCourse.randomUUID, PostgresDegree.randomUUID)
    }

    def tte: Stream[TimetableEntry] = Stream.continually(TimetableEntry(Set(User.randomUUID), Room.randomUUID, 1, LocalTime.now, LocalTime.now plusHours 2))

    def tt(labwork: UUID): Stream[Timetable] = Stream.continually {
      if (nextBoolean()) Timetable(labwork, (tte take 20).toSet, LocalDate.now, Set())
      else Timetable(SesameLabwork.randomUUID, (tte take 20).toSet, LocalDate.now, Set())
    }

    def aplans(labwork: UUID): Stream[AssignmentPlan] = Stream.continually {
      if (nextBoolean()) AssignmentPlan(labwork, 1, 2, Set())
      else AssignmentPlan(SesameLabwork.randomUUID, 1, 2, Set())
    }

    def grps(labwork: UUID): Stream[Group] = Stream.continually {
      if (nextBoolean()) Group("Label", labwork, Set())
      else Group("Label", SesameLabwork.randomUUID, Set())
    }

    def scheds(labwork: UUID): Stream[Schedule] = Stream.continually {
      if (nextBoolean()) Schedule(labwork, Set())
      else Schedule(SesameLabwork.randomUUID, Set())
    }

    def rce(labwork: UUID): Stream[ReportCardEntry] = Stream.continually {
      if (nextBoolean()) ReportCardEntry(User.randomUUID, labwork, "Label", LocalDate.now, LocalTime.now, LocalTime.now plusHours 2, Room.randomUUID, Set())
      else ReportCardEntry(User.randomUUID, SesameLabwork.randomUUID, "Label", LocalDate.now, LocalTime.now, LocalTime.now plusHours 2, Room.randomUUID, Set())
    }

    def rceval(labwork: UUID): Stream[ReportCardEvaluation] = Stream.continually {
      if (nextBoolean()) ReportCardEvaluation(User.randomUUID, labwork, "Label", bool = true, 0)
      else ReportCardEvaluation(User.randomUUID, SesameLabwork.randomUUID, "Label", bool = true, 0)
    }

    def labapp(labwork: UUID): Stream[SesameLabworkApplication] = Stream.continually {
      if (nextBoolean()) SesameLabworkApplication(labwork, User.randomUUID, Set())
      else SesameLabworkApplication(SesameLabwork.randomUUID, User.randomUUID, Set())
    }

    def annot(labwork: UUID): Stream[Annotation] = Stream.continually {
      if (nextBoolean()) Annotation(User.randomUUID, labwork, ReportCardEntry.randomUUID, "Message")
      else Annotation(User.randomUUID, SesameLabwork.randomUUID, ReportCardEntry.randomUUID, "Message")
    }

    "invalidate the semester and subsequent labworks" in {
      import bindings.{AnnotationDescriptor, AssignmentPlanDescriptor, GroupDescriptor, LabworkApplicationDescriptor, LabworkDescriptor, ReportCardEntryDescriptor, ReportCardEvaluationDescriptor, ScheduleDescriptor, SemesterDescriptor, TimetableDescriptor}

      val semester = SesameSemester("Label", "Abbrev", LocalDate.now, LocalDate.now plusDays 2, LocalDate.now plusWeeks 2)
      val labworks = (labs(semester.id) take 100).toSet
      val refLabs = labworks filter (_.semester == semester.id)

      val assPlans = labworks flatMap (l => (aplans(l.id) take 20).toSet)
      val groups = labworks flatMap (l => (grps(l.id) take 20).toSet)
      val schedules = labworks flatMap (l => (scheds(l.id) take 20).toSet)
      val reportCardEntries = labworks flatMap (l => (rce(l.id) take 20).toSet)
      val reportCardEvaluations = labworks flatMap (l => (rceval(l.id) take 20).toSet)
      val timetables = labworks flatMap (l => (tt(l.id) take 10).toSet)
      val applications = labworks flatMap (l => (labapp(l.id) take 20).toSet)
      val refApps = applications filterNot (a => refLabs exists (_.id == a.labwork))
      val annotations = labworks flatMap (l => (annot(l.id) take 20).toSet)
      val refAnnots = annotations filterNot (a => refLabs exists (_.id == a.labwork))

      repo.add[SesameSemester](semester)
      repo.addMany[SesameLabwork](labworks)
      repo.addMany[AssignmentPlan](assPlans)
      repo.addMany[Group](groups)
      repo.addMany[Schedule](schedules)
      repo.addMany[ReportCardEntry](reportCardEntries)
      repo.addMany[ReportCardEvaluation](reportCardEvaluations)
      repo.addMany[Timetable](timetables)
      repo.addMany[SesameLabworkApplication](applications)
      repo.addMany[Annotation](annotations)

      repo.invalidate[SesameSemester](SesameSemester.generateUri(semester))

      repo.get[SesameSemester](SesameSemester.generateUri(semester)) shouldBe Success(None)
      repo.getAll[SesameLabwork] shouldBe Success(labworks filter (_.semester != semester.id))
      repo.getAll[AssignmentPlan] shouldBe Success(assPlans filterNot (a => refLabs exists (_.id == a.labwork)))
      repo.getAll[Group] shouldBe Success(groups filterNot (a => refLabs exists (_.id == a.labwork)))
      repo.getAll[Schedule] shouldBe Success(schedules filterNot (a => refLabs exists (_.id == a.labwork)))
      repo.getAll[ReportCardEntry] shouldBe Success(reportCardEntries filterNot (a => refLabs exists (_.id == a.labwork)))
      repo.getAll[Timetable] shouldBe Success(timetables filterNot (a => refLabs exists (_.id == a.labwork)))
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

      repo.deepGet[SesameSemester](SesameSemester.generateUri(semester)) map (_ map (_.id)) shouldBe Success(Some(semester.id))
      repo.deepGetAll[SesameLabwork] map (_ map (_.id)) shouldBe Success(labworks map (_.id))
      repo.deepGetAll[AssignmentPlan] map (_ map (_.id)) shouldBe Success(assPlans map (_.id))
      repo.deepGetAll[Group] map (_ map (_.id)) shouldBe Success(groups map (_.id))
      repo.deepGetAll[Schedule] map (_ map (_.id)) shouldBe Success(schedules map (_.id))
      repo.deepGetAll[ReportCardEntry] map (_ map (_.id)) shouldBe Success(reportCardEntries map (_.id))
      repo.deepGetAll[Timetable] map (_ map (_.id)) shouldBe Success(timetables map (_.id))
      repo.deepGetAll[SesameLabworkApplication] map (_ map (_.id)) shouldBe Success(applications map (_.id))
      repo.deepGetAll[Annotation] map (_ map (_.id)) shouldBe Success(annotations map (_.id))
    }
  }

}
