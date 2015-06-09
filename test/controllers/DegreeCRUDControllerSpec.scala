package controllers

import models.Degree
import play.api.libs.json.{Writes, Reads}


class DegreeCRUDControllerSpec extends AbstractCRUDControllerSpec[Degree] {
  override val entityToPass: Degree = Degree("degree to pass")

  override def controller: AbstractCRUDController[Degree] = new DegreeCRUDController(repository, namespace)

  override val entityToFail: Degree = Degree("degree to fail")

  override def entityTypeName: String = "Degree"

  override implicit def jsonWrites: Writes[Degree] = Degree.writes
}