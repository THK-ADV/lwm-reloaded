import models.{AnnotationLike, ReportCardEntryLike}
import play.api.libs.json.{Json, Writes}

package object controllers {
  implicit def reportCardsWithAnnotationWrites: Writes[(ReportCardEntryLike, Set[AnnotationLike])] = {
    import controllers.AnnotationController.annotationLikeWrites

    (o: (ReportCardEntryLike, Set[AnnotationLike])) =>
      Json.obj(
        "reportCardEntry" -> o._1,
        "annotations" -> o._2
      )
  }
}
