package invalidation

import java.util.UUID

import base.SesameDbSpec
import models._
import org.joda.time.{LocalDate, LocalTime}

import scala.util.Random._
import scala.util.{Failure, Success}

class CourseInvalidation extends SesameDbSpec {


  "A Course invalidation" should {

    def labs(course: UUID): Stream[SesameLabwork] = Stream.continually {
      if (nextBoolean) SesameLabwork("Label", "Desc", SesameSemester.randomUUID, course, UUID.randomUUID)
      else SesameLabwork("Label", "Desc", SesameSemester.randomUUID, SesameCourse.randomUUID, UUID.randomUUID)
    }

    def aplans(labwork: UUID): Stream[SesameAssignmentPlan] = Stream.continually {
      if (nextBoolean) SesameAssignmentPlan(labwork, 1, 2, Set())
      else SesameAssignmentPlan(SesameLabwork.randomUUID, 1, 2, Set())
    }

    def grps(labwork: UUID): Stream[Group] = Stream.continually {
      if (nextBoolean) Group("Label", labwork, Set())
      else Group("Label", SesameLabwork.randomUUID, Set())
    }

    def scheds(labwork: UUID): Stream[Schedule] = Stream.continually {
      if (nextBoolean) Schedule(labwork, Set())
      else Schedule(SesameLabwork.randomUUID, Set())
    }

    def rce(labwork: UUID): Stream[SesameReportCardEntry] = Stream.continually {
      if (nextBoolean) SesameReportCardEntry(User.randomUUID, labwork, "Label", LocalDate.now, LocalTime.now, LocalTime.now plusHours 2, SesameRoom.randomUUID, Set())
      else SesameReportCardEntry(User.randomUUID, SesameLabwork.randomUUID, "Label", LocalDate.now, LocalTime.now, LocalTime.now plusHours 2, SesameRoom.randomUUID, Set())
    }

    def rceval(labwork: UUID): Stream[SesameReportCardEvaluation] = Stream.continually {
      if (nextBoolean) SesameReportCardEvaluation(User.randomUUID, labwork, "Label", bool = true, 0)
      else SesameReportCardEvaluation(User.randomUUID, SesameLabwork.randomUUID, "Label", bool = true, 0)
    }

    def tte: Stream[SesameTimetableEntry] = Stream.continually(SesameTimetableEntry(Set(User.randomUUID), SesameRoom.randomUUID, 1, LocalTime.now, LocalTime.now plusHours 2))

    def tt(labwork: UUID): Stream[SesameTimetable] = Stream.continually {
      if (nextBoolean) SesameTimetable(labwork, (tte take 20).toSet, LocalDate.now, Set())
      else SesameTimetable(SesameLabwork.randomUUID, (tte take 20).toSet, LocalDate.now, Set())
    }

    def labapp(labwork: UUID): Stream[SesameLabworkApplication] = Stream.continually {
      if (nextBoolean) SesameLabworkApplication(labwork, User.randomUUID, Set())
      else SesameLabworkApplication(SesameLabwork.randomUUID, User.randomUUID, Set())
    }

    def annot(labwork: UUID): Stream[Annotation] = Stream.continually {
      if (nextBoolean) Annotation(User.randomUUID, labwork, SesameReportCardEntry.randomUUID, "Message")
      else Annotation(User.randomUUID, SesameLabwork.randomUUID, SesameReportCardEntry.randomUUID, "Message")
    }

    def auths(course: UUID): Stream[SesameAuthority] = Stream.continually {
      if (nextBoolean) SesameAuthority(User.randomUUID, SesameRole.randomUUID, Some(course))
      else SesameAuthority(User.randomUUID, SesameRole.randomUUID, Some(SesameCourse.randomUUID))
    }

    "invalidate the course and subsequent labwork and refroles" in {
      import bindings.{
      CourseDescriptor,
      AuthorityDescriptor,
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

      val course = SesameCourse("Label", "Desc", "Abbrev", User.randomUUID, 2)
      val labworks = (labs(course.id) take 100).toSet
      val refLabs = labworks filter (_.course == course.id)

      val authorities = (auths(course.id) take 100).toSet
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

      repo.add[SesameCourse](course)
      repo.addMany[SesameLabwork](labworks)
      repo.addMany[SesameAuthority](authorities)
      repo.addMany[SesameAssignmentPlan](assPlans)
      repo.addMany[Group](groups)
      repo.addMany[Schedule](schedules)
      repo.addMany[SesameReportCardEntry](reportCardEntries)
      repo.addMany[SesameReportCardEvaluation](reportCardEvaluations)
      repo.addMany[SesameTimetable](timetables)
      repo.addMany[SesameLabworkApplication](applications)
      repo.addMany[Annotation](annotations)

      repo.invalidate[SesameCourse](SesameCourse.generateUri(course))

      repo.get[SesameCourse](SesameCourse.generateUri(course)) shouldBe Success(None)
      repo.getAll[SesameLabwork] shouldBe Success(labworks filter (_.course != course.id))
      repo.getAll[SesameAuthority] shouldBe Success(authorities filter (_.course.get != course.id))
      repo.getAll[SesameAssignmentPlan] shouldBe Success(assPlans filterNot (a => refLabs exists (_.id == a.labwork)))
      repo.getAll[Group] shouldBe Success(groups filterNot (a => refLabs exists (_.id == a.labwork)))
      repo.getAll[Schedule] shouldBe Success(schedules filterNot (a => refLabs exists (_.id == a.labwork)))
      repo.getAll[SesameReportCardEntry] shouldBe Success(reportCardEntries filterNot (a => refLabs exists (_.id == a.labwork)))
      repo.getAll[SesameTimetable] shouldBe Success(timetables filterNot (a => refLabs exists (_.id == a.labwork)))
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

      repo.deepGet[SesameCourse](SesameCourse.generateUri(course)) map (_ map (_.id)) shouldBe Success(Some(course.id))
      repo.deepGetAll[SesameLabwork] map (_ map (_.id)) shouldBe Success(labworks map (_.id))
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
