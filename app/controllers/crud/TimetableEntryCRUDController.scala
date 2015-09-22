package controllers.crud

import java.util.UUID

import models.UriGenerator
import models.timetable.{TimetableEntry, TimetableEntryProtocol}
import org.w3.banana.binder.{ClassUrisFor, FromPG, ToPG}
import org.w3.banana.sesame.Sesame
import play.api.libs.json.{Reads, Writes}
import play.api.mvc.Result
import services.RoleService
import store.{Namespace, SesameRepository}
import utils.LwmMimeType

import scala.collection.Map

class TimetableEntryCRUDController(val repository: SesameRepository, val namespace: Namespace, val roleService: RoleService) extends AbstractCRUDController[TimetableEntryProtocol, TimetableEntry] {
   override implicit def rdfWrites: ToPG[Sesame, TimetableEntry] = defaultBindings.TimetableEntryBinding.timetableEntryBinder

   override implicit def rdfReads: FromPG[Sesame, TimetableEntry] = defaultBindings.TimetableEntryBinding.timetableEntryBinder

   override implicit def classUrisFor: ClassUrisFor[Sesame, TimetableEntry] = defaultBindings.TimetableEntryBinding.classUri

   override implicit def uriGenerator: UriGenerator[TimetableEntry] = TimetableEntry

   override implicit def reads: Reads[TimetableEntryProtocol] = TimetableEntry.reads

   override implicit def writes: Writes[TimetableEntry] = TimetableEntry.writes

   override def getWithFilter(queryString: Map[String, Seq[String]]): Result = ???

   override protected def fromInput(input: TimetableEntryProtocol, id: Option[UUID]): TimetableEntry = ???

   override val mimeType: LwmMimeType = LwmMimeType.timetableEntryV1Json
}
