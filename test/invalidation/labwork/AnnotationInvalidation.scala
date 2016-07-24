package invalidation.labwork

import base.SesameDbSpec
import models.labwork.{Annotation, Labwork, ReportCardEntry}
import models.users.User

import scala.util.Random._
import scala.util.{Failure, Success}

class AnnotationInvalidation extends SesameDbSpec {

  "An Annotation invalidation" should {

    def annot: Stream[Annotation] = Stream.continually(Annotation(User.randomUUID, Labwork.randomUUID, ReportCardEntry.randomUUID, "Message"))

    "invalidate the annotation" in {
      import bindings.AnnotationDescriptor

      val annots = (annot take 100).toSet
      val toInvalidate = shuffle(annots) take 20

      repo.addMany[Annotation](annots)

      toInvalidate foreach (a => repo.invalidate[Annotation](Annotation.generateUri(a)))

      repo.getAll[Annotation] match {
        case Success(set) =>
          set.toVector.sortBy(_.student) shouldBe (annots diff toInvalidate).toVector.sortBy(_.student)
        case Failure(e) => fail("no")
      }

      repo.deepGetAll[Annotation] map (_ map (_.id)) shouldBe Success(annots map (_.id))
    }
  }

}
