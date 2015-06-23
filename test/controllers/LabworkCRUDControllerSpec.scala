package controllers

import java.util.UUID

import controllers.crud.{LabworkCRUDController, AbstractCRUDController}
import models.{LabworkProtocol, Labwork}
import play.api.libs.json.{Json, JsValue, Writes}

class LabworkCRUDControllerSpec extends AbstractCRUDControllerSpec[LabworkProtocol, Labwork] {
  override val entityToPass: Labwork = Labwork("label to pass", Labwork.randomUUID)

  override def entityTypeName: String = "Labwork"

  override val controller: AbstractCRUDController[LabworkProtocol, Labwork] = new LabworkCRUDController(repository, namespace) {
    override protected def fromInput(input: LabworkProtocol, id: Option[UUID]): Labwork = entityToPass
  }

  override val entityToFail: Labwork = Labwork("label to fail", Labwork.randomUUID)

  override implicit val jsonWrites: Writes[Labwork] = Labwork.writes

  override val mimeType: String = "application/json" //TODO: this should be a proper content type

  override val inputJson: JsValue = Json.obj(
    "label" -> "label input"
  )
}
