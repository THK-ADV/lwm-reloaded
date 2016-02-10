package controllers.crud.schedule

import java.util.UUID

import controllers.crud.{AbstractCRUDController, AbstractCRUDControllerSpec}
import models.Labwork
import models.schedule.{Timetable, TimetableEntry, TimetableProtocol}
import models.semester.Blacklist
import org.joda.time.DateTime
import org.w3.banana.PointedGraph
import org.w3.banana.sesame.Sesame
import play.api.libs.json.{Json, JsValue, Writes}
import utils.LwmMimeType

class TimetableCRUDControllerSpec extends AbstractCRUDControllerSpec[TimetableProtocol, Timetable] {
  override def entityTypeName: String = "timetable"

  override val controller: AbstractCRUDController[TimetableProtocol, Timetable] = new TimetableCRUDController(repository, namespace, roleService) {
    override protected def fromInput(input: TimetableProtocol, id: Option[UUID]): Timetable = entityToPass

    override protected def contextFrom: PartialFunction[Rule, SecureContext] = {
      case _ => NonSecureBlock
    }

    override protected def restrictedContext(moduleId: String): PartialFunction[Rule, SecureContext] = {
      case _ => NonSecureBlock
    }
  }

  override val entityToFail: Timetable = Timetable(Labwork.randomUUID, Set.empty[TimetableEntry], DateTime.now, Blacklist.empty, Timetable.randomUUID)

  override val entityToPass: Timetable = Timetable(Labwork.randomUUID, Set.empty[TimetableEntry], DateTime.now, Blacklist.empty, Timetable.randomUUID)

  override implicit val jsonWrites: Writes[Timetable] = Timetable.writes

  override val mimeType: LwmMimeType = LwmMimeType.timetableV1Json

  import ops._
  import bindings.TimetableBinding.timetableBinder

  override val pointedGraph: PointedGraph[Sesame] = entityToPass.toPG

  override val inputJson: JsValue = Json.obj(
    "labwork" -> entityToPass.labwork,
    "entries" -> entityToPass.entries,
    "start" -> entityToPass.start,
    "localBlacklist" -> entityToPass.localBlacklist
  )
}
