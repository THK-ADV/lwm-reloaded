package controllers

import models.Degree
import play.api.libs.json.{Writes, Reads}


class DegreeCRUDControllerSpec extends AbstractCRUDControllerSpec[Degree] {
  override val entityToPass: Degree = Degree("label to pass")

  override val controller: AbstractCRUDController[Degree] = new DegreeCRUDController(repository, namespace)

  override val entityToFail: Degree = Degree("label to fail")

  override def entityTypeName: String = "Degree"

  override implicit val jsonWrites: Writes[Degree] = Degree.writes

  override val mimeType: String = "application/json" //TODO: this should be a proper content type
}