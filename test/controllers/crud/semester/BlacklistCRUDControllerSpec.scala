package controllers.crud.semester

import controllers.crud.{AbstractCRUDController, AbstractCRUDControllerSpec}
import models.semester.{Blacklist, BlacklistProtocol}
import org.joda.time.DateTime
import org.w3.banana.PointedGraph
import org.w3.banana.sesame.Sesame
import play.api.libs.json.{Json, Writes, JsValue}
import utils.LwmMimeType

class BlacklistCRUDControllerSpec extends AbstractCRUDControllerSpec[BlacklistProtocol, Blacklist, Blacklist] {

  import ops._
  import bindings.BlacklistDescriptor

  val dates = (0 until 10).map(DateTime.now.plusWeeks).toSet

  override def entityTypeName: String = "blacklist"

  override val controller: BlacklistCRUDController = new BlacklistCRUDController(repository, sessionService, namespace, roleService) {

    override protected def fromInput(input: BlacklistProtocol, existing: Option[Blacklist]): Blacklist = entityToPass

    override protected def contextFrom: PartialFunction[Rule, SecureContext] = {
      case _ => NonSecureBlock
    }
  }

  override val entityToFail: Blacklist = Blacklist("blacklist to fail", dates)

  override val entityToPass: Blacklist = Blacklist("blacklist to pass", dates)

  override implicit val jsonWrites: Writes[Blacklist] = Blacklist.writes

  override val atomizedEntityToPass: Blacklist = entityToPass

  override val atomizedEntityToFail: Blacklist = entityToFail

  override val jsonWritesAtom: Writes[Blacklist] = jsonWrites

  implicit val blacklistBinder = BlacklistDescriptor.binder

  override val pointedGraph: PointedGraph[Sesame] = entityToPass.toPG

  override val mimeType: LwmMimeType = LwmMimeType.blacklistV1Json

  override val inputJson: JsValue = Json.obj(
    "label" -> entityToPass.label,
    "dates" -> entityToPass.dates
  )

  override val updateJson: JsValue = Json.obj(
    "label" -> entityToPass.label,
    "dates" -> (entityToPass.dates + DateTime.now)
  )
}
