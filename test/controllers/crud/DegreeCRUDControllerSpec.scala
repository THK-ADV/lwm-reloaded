package controllers.crud

import java.util.UUID

import models.{Degree, DegreeProtocol}
import org.w3.banana.PointedGraph
import org.w3.banana.sesame.Sesame
import play.api.libs.json.{JsValue, Json, Writes}
import play.api.mvc.{Action, Result, AnyContent, Request}
import utils.LWMActions.ContentTypedAction
import utils.LwmMimeType

class DegreeCRUDControllerSpec extends AbstractCRUDControllerSpec[DegreeProtocol, Degree] {
  override val entityToPass: Degree = Degree("label to pass", Degree.randomUUID)

  override val controller: AbstractCRUDController[DegreeProtocol, Degree] = new DegreeCRUDController(repository, namespace, roleService) {

    override protected def fromInput(input: DegreeProtocol, id: Option[UUID]): Degree = entityToPass
  }

  override val entityToFail: Degree = Degree("label to fail", Degree.randomUUID)

  override implicit val jsonWrites: Writes[Degree] = Degree.writes

  override val mimeType: LwmMimeType = LwmMimeType.degreeV1Json

  override def entityTypeName: String = "degree"

  override val inputJson: JsValue = Json.obj(
    "label" -> entityToPass.label
  )

  import bindings.DegreeBinding._
  import ops._

  override def pointedGraph: PointedGraph[Sesame] = entityToPass.toPG
}