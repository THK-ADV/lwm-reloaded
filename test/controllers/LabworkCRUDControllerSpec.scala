package controllers

import controllers.crud.{LabworkCRUDController, AbstractCRUDController}
import models.Labwork
import play.api.libs.json.Writes

class LabworkCRUDControllerSpec extends AbstractCRUDControllerSpec[Labwork] {
  override val entityToPass: Labwork = Labwork("label to pass")

  override def entityTypeName: String = "Labwork"

  override val controller: AbstractCRUDController[Labwork] = new LabworkCRUDController(repository, namespace)

  override val entityToFail: Labwork = Labwork("label to fail")

  override implicit val jsonWrites: Writes[Labwork] = Labwork.writes

  override val mimeType: String = "application/json" //TODO: this should be a proper content type
}
