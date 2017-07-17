package invalidation

import java.util.UUID

import base.SesameDbSpec
import models._
import org.joda.time.{LocalDate, LocalTime}

import scala.util.Random._
import scala.util.{Failure, Success}

class ReportCardInvalidation extends SesameDbSpec {

  "A ReportCard invalidation" should {

    def annot(reportCardEntry: UUID): Stream[Annotation] = Stream.continually {
      if (nextBoolean()) Annotation(User.randomUUID, SesameLabwork.randomUUID, reportCardEntry, "Message")
      else Annotation(User.randomUUID, SesameLabwork.randomUUID, SesameReportCardEntry.randomUUID, "Message")
    }

    def rcetypes: Stream[SesameReportCardEntryType] = Stream.continually(SesameReportCardEntryType(s"String$nextInt", nextBoolean()))
    def rce: Stream[SesameReportCardEntry] = Stream.continually(
      SesameReportCardEntry(User.randomUUID, SesameLabwork.randomUUID, "Label", LocalDate.now, LocalTime.now, LocalTime.now plusHours 2, SesameRoom.randomUUID, (rcetypes take 10).toSet))

    "invalidate the report card and subsequent report card entries and annotations" in {
      import bindings.{AnnotationDescriptor, ReportCardEntryDescriptor}

      val reportCardEntries = (rce take 100).toSet
      val toInvalidate = shuffle(reportCardEntries) take 30
      val annotations = toInvalidate flatMap (e => (annot(e.id) take 5).toSet)


      repo.addMany[SesameReportCardEntry](reportCardEntries)
      repo.addMany[Annotation](annotations)

      toInvalidate foreach (a => repo.invalidate[SesameReportCardEntry](SesameReportCardEntry.generateUri(a)))

      repo.getAll[SesameReportCardEntry] shouldBe Success(reportCardEntries diff toInvalidate)

      repo.getAll[Annotation] match {
        case Success(set) =>
          set.toVector.sortBy(_.student) shouldBe annotations.filterNot (a => toInvalidate exists (e => a.reportCardEntry == e.id)).toVector.sortBy(_.student)
        case Failure(e) => fail("no")
      }

      repo.deepGetAll[SesameReportCardEntry] map (_ map (_.id)) shouldBe Success(reportCardEntries map (_.id))
      repo.deepGetAll[Annotation] map (_ map (_.id)) shouldBe Success(annotations map (_.id))
    }
  }

}
