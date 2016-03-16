package controllers

import java.util.UUID

import controllers.crud.{AbstractCRUDController, AbstractCRUDControllerSpec}
import models.AssignmentEntryType._
import models.{AssignmentEntryType, ReportCardEntry, ReportCard}
import org.joda.time.{LocalTime, LocalDate}
import org.w3.banana.PointedGraph
import org.w3.banana.sesame.Sesame
import play.api.libs.json.{Json, Writes, JsValue}
import utils.LwmMimeType

class ReportControllerSpec extends AbstractCRUDControllerSpec[ReportCard, ReportCard] {

  override def entityTypeName: String = "reportCard"

  override val controller: AbstractCRUDController[ReportCard, ReportCard] = new ReportCardController(repository, sessionService, namespace, roleService) {

    override protected def fromInput(input: ReportCard, existing: Option[ReportCard]): ReportCard = entityToPass

    override protected def contextFrom: PartialFunction[Rule, SecureContext] = {
      case _ => NonSecureBlock
    }

    override protected def restrictedContext(restrictionId: String): PartialFunction[Rule, SecureContext] = {
      case _ => NonSecureBlock
    }
  }

  val entries = (0 until 5).map( n =>
    ReportCardEntry(n, n.toString, LocalDate.now.plusWeeks(n), LocalTime.now.plusHours(n), LocalTime.now.plusHours(n + 1), UUID.randomUUID(), AssignmentEntryType.all.toSet.map(fromProtocol))
  ).toSet

  override val entityToFail: ReportCard = ReportCard(UUID.randomUUID(), UUID.randomUUID(), entries)

  override val entityToPass: ReportCard = ReportCard(UUID.randomUUID(), UUID.randomUUID(), entries)

  override implicit val jsonWrites: Writes[ReportCard] = ReportCard.writes

  override val mimeType: LwmMimeType = LwmMimeType.reportCardV1Json

  import ops._
  import bindings.ReportCardBinding._
  override val pointedGraph: PointedGraph[Sesame] = entityToPass.toPG

  override val updateJson: JsValue = Json.obj(
    "student" -> entityToPass.student,
    "labwork" -> entityToPass.labwork,
    "entries" -> entityToPass.entries.drop(2),
    "id" -> entityToPass.id
  )

  override val inputJson: JsValue = Json.obj(
    "student" -> entityToPass.student,
    "labwork" -> entityToPass.labwork,
    "entries" -> entityToPass.entries,
    "id" -> entityToPass.id
  )
}
