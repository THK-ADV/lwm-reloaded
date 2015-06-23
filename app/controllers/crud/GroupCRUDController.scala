package controllers.crud

import models.{Group, UriGenerator}
import org.w3.banana.binder.{ClassUrisFor, FromPG, ToPG}
import org.w3.banana.sesame.Sesame
import play.api.libs.json.{Reads, Writes}
import store.{Namespace, SesameRepository}

class GroupCRUDController(val repository: SesameRepository, val namespace: Namespace) extends AbstractCRUDController[Group] {
   override implicit def rdfWrites: ToPG[Sesame, Group] = defaultBindings.GroupBinding.groupBinder

   override implicit def rdfReads: FromPG[Sesame, Group] = defaultBindings.GroupBinding.groupBinder

   override implicit def classUrisFor: ClassUrisFor[Sesame, Group] = defaultBindings.GroupBinding.classUri

   override implicit def uriGenerator: UriGenerator[Group] = Group

   override implicit def reads: Reads[Group] = Group.reads

   override implicit def writes: Writes[Group] = Group.writes
 }
