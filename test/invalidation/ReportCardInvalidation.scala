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
      if (nextBoolean()) Annotation(User.randomUUID, Labwork.randomUUID, reportCardEntry, "Message")
      else Annotation(User.randomUUID, Labwork.randomUUID, ReportCardEntry.randomUUID, "Message")
    }

    def rcetypes: Stream[ReportCardEntryType] = Stream.continually(ReportCardEntryType(s"String$nextInt", nextBoolean()))
    def rce: Stream[ReportCardEntry] = Stream.continually(
      ReportCardEntry(User.randomUUID, Labwork.randomUUID, "Label", LocalDate.now, LocalTime.now, LocalTime.now plusHours 2, Room.randomUUID, (rcetypes take 10).toSet))

    "invalidate the report card and subsequent report card entries and annotations" in {
      import bindings.{AnnotationDescriptor, ReportCardEntryDescriptor}

      val reportCardEntries = (rce take 100).toSet
      val toInvalidate = shuffle(reportCardEntries) take 30
      val annotations = toInvalidate flatMap (e => (annot(e.id) take 5).toSet)


      repo.addMany[ReportCardEntry](reportCardEntries)
      repo.addMany[Annotation](annotations)

      toInvalidate foreach (a => repo.invalidate[ReportCardEntry](ReportCardEntry.generateUri(a)))

      repo.getAll[ReportCardEntry] shouldBe Success(reportCardEntries diff toInvalidate)

      repo.getAll[Annotation] match {
        case Success(set) =>
          set.toVector.sortBy(_.student) shouldBe annotations.filterNot (a => toInvalidate exists (e => a.reportCardEntry == e.id)).toVector.sortBy(_.student)
        case Failure(e) => fail("no")
      }

      repo.deepGetAll[ReportCardEntry] map (_ map (_.id)) shouldBe Success(reportCardEntries map (_.id))
      repo.deepGetAll[Annotation] map (_ map (_.id)) shouldBe Success(annotations map (_.id))
    }
  }

}
