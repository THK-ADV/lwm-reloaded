package invalidation

import java.util.UUID

import base.SesameDbSpec
import models._
import org.joda.time.{LocalDate, LocalTime}

import scala.util.Random._
import scala.util.{Failure, Success}

class DegreeInvalidation extends SesameDbSpec {

  "A Degree invalidation" should {
    def tte: Stream[TimetableEntry] = Stream.continually(TimetableEntry(Set(User.randomUUID), SesameRoom.randomUUID, 1, LocalTime.now, LocalTime.now plusHours 2))

    def tt: Stream[Timetable] = Stream.continually {
      Timetable(SesameLabwork.randomUUID, (tte take 20).toSet, LocalDate.now, Set())
    }

    def labs(degree: UUID): Stream[SesameLabwork] = Stream.continually {
      if (nextBoolean()) SesameLabwork("Label", "Desc", SesameSemester.randomUUID, SesameCourse.randomUUID, degree)
      else SesameLabwork("Label", "Desc", SesameSemester.randomUUID, SesameCourse.randomUUID, UUID.randomUUID)
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
      if (nextBoolean()) ReportCardEntry(User.randomUUID, labwork, "Label", LocalDate.now, LocalTime.now, LocalTime.now plusHours 2, SesameRoom.randomUUID, Set())
      else ReportCardEntry(User.randomUUID, SesameLabwork.randomUUID, "Label", LocalDate.now, LocalTime.now, LocalTime.now plusHours 2, SesameRoom.randomUUID, Set())
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

    "invalidate the degree and subsequent timetable entries and labworks" in {
      import bindings.{
      DegreeDescriptor,
      LabworkDescriptor,
      AssignmentPlanDescriptor,
      GroupDescriptor,
      ScheduleDescriptor,
      ReportCardEntryDescriptor,
      ReportCardEvaluationDescriptor,
      TimetableDescriptor,
      LabworkApplicationDescriptor,
      AnnotationDescriptor
      }

      val degree = SesameDegree("Label", "Abbrev")
      val labworks = (labs(degree.id) take 100).toSet
      val refLabs = labworks filter (_.degree == degree.id)

      val assPlans = labworks flatMap (l => (aplans(l.id) take 20).toSet)
      val groups = labworks flatMap (l => (grps(l.id) take 20).toSet)
      val schedules = labworks flatMap (l => (scheds(l.id) take 20).toSet)
      val reportCardEntries = labworks flatMap (l => (rce(l.id) take 20).toSet)
      val reportCardEvaluations = labworks flatMap (l => (rceval(l.id) take 20).toSet)
      val timetables = labworks flatMap (_ => (tt take 10).toSet)
      val applications = labworks flatMap (l => (labapp(l.id) take 20).toSet)
      val refApps = applications filterNot (a => refLabs exists (_.id == a.labwork))
      val annotations = labworks flatMap (l => (annot(l.id) take 20).toSet)
      val refAnnots = annotations filterNot (a => refLabs exists (_.id == a.labwork))

      repo.add[SesameDegree](degree)
      repo.addMany[SesameLabwork](labworks)
      repo.addMany[AssignmentPlan](assPlans)
      repo.addMany[Group](groups)
      repo.addMany[Schedule](schedules)
      repo.addMany[ReportCardEntry](reportCardEntries)
      repo.addMany[ReportCardEvaluation](reportCardEvaluations)
      repo.addMany[Timetable](timetables)
      repo.addMany[SesameLabworkApplication](applications)
      repo.addMany[Annotation](annotations)

      repo.invalidate[SesameDegree](SesameDegree.generateUri(degree))

      repo.get[SesameDegree](SesameDegree.generateUri(degree)) shouldBe Success(None)
      repo.getAll[SesameLabwork] shouldBe Success(labworks filter (_.degree != degree.id))
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

      repo.deepGet[SesameDegree](SesameDegree.generateUri(degree)) map (_ map (_.id)) shouldBe Success(Some(degree.id))
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
