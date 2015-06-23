package controllers.crud

import models.UriGenerator
import models.schedules.GroupSchedule
import org.w3.banana.binder.{ClassUrisFor, FromPG, ToPG}
import org.w3.banana.sesame.Sesame
import play.api.libs.json.{Reads, Writes}
import play.api.mvc.Result
import store.{Namespace, SesameRepository}

import scala.collection.Map

class GroupScheduleCRUDController(val repository: SesameRepository, val namespace: Namespace) extends AbstractCRUDController[GroupSchedule] {
   override implicit def rdfWrites: ToPG[Sesame, GroupSchedule] = defaultBindings.GroupScheduleBinding.groupScheduleBinder

   override implicit def rdfReads: FromPG[Sesame, GroupSchedule] = defaultBindings.GroupScheduleBinding.groupScheduleBinder

   override implicit def classUrisFor: ClassUrisFor[Sesame, GroupSchedule] = defaultBindings.GroupScheduleBinding.classUri

   override implicit def uriGenerator: UriGenerator[GroupSchedule] = GroupSchedule

   override implicit def reads: Reads[GroupSchedule] = GroupSchedule.reads

   override implicit def writes: Writes[GroupSchedule] = GroupSchedule.writes

   override def getWithFilter(queryString: Map[String, Seq[String]]): Result = ???
}
