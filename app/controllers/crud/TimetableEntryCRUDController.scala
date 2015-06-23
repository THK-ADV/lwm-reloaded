package controllers.crud

import models.UriGenerator
import models.timetable.TimetableEntry
import org.w3.banana.binder.{ClassUrisFor, FromPG, ToPG}
import org.w3.banana.sesame.Sesame
import play.api.libs.json.{Reads, Writes}
import play.api.mvc.Result
import store.{Namespace, SesameRepository}

import scala.collection.Map

class TimetableEntryCRUDController(val repository: SesameRepository, val namespace: Namespace) extends AbstractCRUDController[TimetableEntry] {
   override implicit def rdfWrites: ToPG[Sesame, TimetableEntry] = defaultBindings.TimetableEntryBinding.timetableEntryBinder

   override implicit def rdfReads: FromPG[Sesame, TimetableEntry] = defaultBindings.TimetableEntryBinding.timetableEntryBinder

   override implicit def classUrisFor: ClassUrisFor[Sesame, TimetableEntry] = defaultBindings.TimetableEntryBinding.classUri

   override implicit def uriGenerator: UriGenerator[TimetableEntry] = TimetableEntry

   override implicit def reads: Reads[TimetableEntry] = TimetableEntry.reads

   override implicit def writes: Writes[TimetableEntry] = TimetableEntry.writes

   override def getWithFilter(queryString: Map[String, Seq[String]]): Result = ???
}
