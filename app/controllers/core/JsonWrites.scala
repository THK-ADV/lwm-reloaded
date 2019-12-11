package controllers.core

import models.UniqueEntity
import play.api.libs.json.Writes

trait JsonWrites[O <: UniqueEntity] {
  protected implicit def writes: Writes[O]
}
